# Библиотеки
library(forecast)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(forecast)
library(data.table)

fill_forecast_ets <- function(data, time = "Date", frequency = 1, cols = NULL, n_periods = 0) {
  # Убедимся, что имена столбцов уникальны
  names(data) <- make.unique(names(data))
  
  # Проверка наличия столбца времени
  if (!time %in% names(data)) stop("The time column specified does not exist in the dataset.")
  
  # Проверка наличия неизменяемых столбцов только если cols не NULL
  if (!is.null(cols) && !all(cols %in% names(data))) stop("One or more specified columns to keep unchanged do not exist in the dataset.")
  
  # Сохраняем порядок столбцов
  original_order <- names(data)
  
  # Сохраняем временную шкалу и заменяем её на числовой ряд
  original_time <- data[[time]]
  data[[time]] <- seq_along(data[[time]])
  
  # Извлечение временной шкалы в числовом формате
  start_index <- min(data[[time]], na.rm = TRUE)
  end_index <- max(data[[time]], na.rm = TRUE)
  
  # --- Функции для обработки данных ---
  fill_missing_in_middle <- function(column) {
    if (!is.numeric(column)) return(column)  # Пропускаем нечисловые столбцы
    if (all(is.na(column))) return(column)
    first_non_na <- which(!is.na(column))[1]
    last_non_na <- which(!is.na(column))[length(which(!is.na(column)))]
    ts_data <- ts(column[first_non_na:last_non_na], frequency = frequency)
    imputed_values <- na.interp(ts_data)
    column[first_non_na:last_non_na] <- as.numeric(imputed_values)
    return(column)
  }
  
  forecast_column_start <- function(column, start_index, end_index) {
    if (!is.numeric(column)) return(column)  # Пропускаем нечисловые столбцы
    if (all(is.na(column))) return(column)
    first_known_index <- min(which(!is.na(column)))
    if (first_known_index == 1) return(column)
    known_values <- column[first_known_index:length(column)]
    ts_data <- ts(rev(known_values), frequency = frequency)
    forecast_horizon <- first_known_index - 1
    if (forecast_horizon > 0) {
      ets_model <- ets(ts_data)
      forecast_values <- forecast(ets_model, h = forecast_horizon)$mean
      forecast_values <- rev(as.numeric(forecast_values))
      full_values <- c(forecast_values, known_values)
    } else {
      full_values <- known_values
    }
    return(c(full_values, rep(NA, length(column) - length(full_values))))
  }
  
  forecast_column_end <- function(column, start_index, end_index) {
    if (!is.numeric(column)) return(column)  # Пропускаем нечисловые столбцы
    if (all(is.na(column))) return(column)
    last_known_index <- max(which(!is.na(column)))
    ts_data <- ts(column[1:last_known_index], frequency = frequency)
    forecast_horizon <- end_index - start_index - last_known_index + 1
    if (forecast_horizon > 0) {
      ets_model <- ets(ts_data)
      forecast_values <- forecast(ets_model, h = forecast_horizon)$mean
      full_values <- c(column[1:last_known_index], as.numeric(forecast_values))
    } else {
      full_values <- column
    }
    return(full_values)
  }
  
  # --- Обработка данных ---
  if (is.null(cols)) {
    # Если cols = NULL, исключаем все нечисловые столбцы, кроме time
    columns_to_process <- setdiff(names(data), time)
    columns_to_process <- columns_to_process[sapply(data[columns_to_process], is.numeric)]
  } else {
    # Если cols задан, обрабатываем все числовые столбцы, кроме time и cols
    columns_to_process <- setdiff(names(data), c(time, cols))
  }
  
  # Обработка данных: заполнение пропусков
  imputed_data <- data %>%
    mutate(across(all_of(columns_to_process), fill_missing_in_middle))
  
  result_start <- imputed_data %>%
    mutate(across(all_of(columns_to_process), ~ forecast_column_start(., start_index, end_index)))
  
  result_end <- imputed_data %>%
    mutate(across(all_of(columns_to_process), ~ forecast_column_end(., start_index, end_index)))
  
  processed_data <- map2_dfc(result_end[columns_to_process], result_start[columns_to_process], coalesce)
  
  # --- Продление неизменяемых столбцов ---
  if (!is.null(cols)) {
    # Продлеваем значения столбцов из cols
    extended_cols <- data %>%
      select(any_of(cols)) %>%
      map_df(~ c(.x, rep(last(.x[!is.na(.x)]), n_periods)))
  } else {
    extended_cols <- NULL  # Если cols не задан, ничего не продлеваем
  }
  
  # --- Прогноз на n_periods вперед ---
  if (!is.null(n_periods) && n_periods > 0) {
    forecast_future <- function(column) {
      if (!is.numeric(column)) return(rep(NA, n_periods))  # Пропускаем нечисловые столбцы
      ts_data <- ts(column, frequency = frequency)
      ets_model <- ets(ts_data)
      forecast_values <- forecast(ets_model, h = n_periods)$mean
      return(as.numeric(forecast_values))
    }
    
    future_values <- map_dfc(columns_to_process, ~ forecast_future(processed_data[[.x]]))
    colnames(future_values) <- columns_to_process
    
    future_time <- seq(max(original_time, na.rm = TRUE) + 1, by = 1, length.out = n_periods)
    
    future_data <- tibble(!!time := future_time, !!!setNames(future_values, columns_to_process))
    
    # Объединяем данные
    if (!is.null(extended_cols)) {
      filled_data <- bind_cols(
        bind_rows(data %>%
                    select(all_of(time)) %>%
                    mutate(!!time := original_time) %>%  # Восстанавливаем оригинальную временную шкалу
                    bind_cols(processed_data),
                  future_data),
        extended_cols
      )
    } else {
      filled_data <- bind_rows(data %>%
                                 select(all_of(time)) %>%
                                 mutate(!!time := original_time) %>%
                                 bind_cols(processed_data),
                               future_data)
    }
  } else {
    # Если n_periods = 0, объединяем только основную часть
    if (!is.null(extended_cols)) {
      filled_data <- bind_cols(
        data %>%
          select(all_of(time)) %>%
          mutate(!!time := original_time) %>%  # Восстанавливаем оригинальную временную шкалу
          bind_cols(processed_data),
        extended_cols
      )
    } else {
      filled_data <- data %>%
        select(all_of(time)) %>%
        mutate(!!time := original_time) %>%
        bind_cols(processed_data)
    }
  }
  
  # Расставляем столбцы в исходном порядке
  final_result <- filled_data %>%
    select(any_of(original_order), everything())
  
  return(final_result)
}

df <- read_csv("Исходные Данные\\Запасы\\Stock of vehicles by category and NUTS 2 regions.csv")

summary <- df %>%
    select(where(~ is.character(.) | is.factor(.))) %>%
    summarise(across(everything(), ~ list(unique(.)))) %>%
    unnest(cols = everything())

df$vehicle <- NULL
df$Unit <- NULL

df_wide <- df %>%
    pivot_wider(
        id_cols = c("measure", "Date"),
        names_from = "geo",
        values_from = "Value"
    )

df_wide <- df_wide %>%
    relocate(measure, .after = Date)

df_forecast <- fill_forecast_ets(
    data = df_wide,
    time = "Date",
    frequency = 1,
    cols = "measure",
    n_periods = 2
)

df_long <- df_forecast %>%
  pivot_longer(
    cols = -c(Date, measure),  # Все столбцы, кроме "Год" и "Ед. Изм."
    names_to = "Country",         # Новый столбец для названий стран
    values_to = "Volume"          # Новый столбец для значений
  )

df_long$Volume <- df_long$Volume / 1000000
df_long$measure <- "mln units"

fwrite(df_long, file = "Обработанные Данные\\Запасы\\stock-countries-2.tsv", sep = "\t", quote = FALSE)
