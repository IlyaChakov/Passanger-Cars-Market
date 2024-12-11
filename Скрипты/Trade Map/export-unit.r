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

df <- read_tsv("Исходные Данные\\Торговля\\export-unit.tsv")

# Замена названий столбцов
names(df) <- gsub("-Объемы экспорта", "", names(df))  # Убираем "экспорт " из названий столбцов

df <- df %>%
  select(-contains("Единица"))

df <- df %>%
    select(-last_col())

df <- df[df$Экспортеры != "Весь Мир", ]

df <- df %>%
    pivot_longer(
        cols = -"Экспортеры",
        names_to = "Год",
        values_to = "Объем"
    )

df$Год <- as.numeric(df$Год)
df$Экспортеры <- as.character(df$Экспортеры)
df$Объем <- as.numeric(df$Объем)

df <- df %>%
    pivot_wider(
        id_cols = "Год",
        names_from = "Экспортеры",
        values_from = "Объем"
    )

# Замена всех числовых ячеек с нулём на NA
df <- df %>%
  mutate(across(where(is.numeric), ~ ifelse(. == 0, NA, .)))

df$Джибути <- NULL

df_forecast <- fill_forecast_ets(
    data = df,
    time = "Год",
    frequency = 1,
    n_periods = 1
)

df_long <- df_forecast %>%
  pivot_longer(
    cols = -c(Год),  # Все столбцы, кроме "Год" и "Ед. Изм."
    names_to = "Страна",         # Новый столбец для названий стран
    values_to = "Объем"          # Новый столбец для значений
  )

df_long$Объем <- df_long$Объем / 1000000

df_long$'Ед. Изм.' <- "млн штук"

df_long <- df_long %>%
  relocate(`Ед. Изм.`, .after = Год)

fwrite(df_long, file = "Обработанные Данные\\Торговля\\export-countries.tsv", sep = "\t", quote = FALSE)
