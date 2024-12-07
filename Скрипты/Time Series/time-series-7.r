library(forecast)
library(dplyr)
library(purrr)

fill_forecast_ets <- function(data, time = "Date", frequency = 1, cols = "Unit") {
  # Убедимся, что имена столбцов уникальны
  names(data) <- make.unique(names(data))
  
  # Проверка наличия столбца времени и оставляемых без изменения столбцов
  if (!time %in% names(data)) stop("The time column specified does not exist in the dataset.")
  if (!all(cols %in% names(data))) stop("One or more specified columns to keep unchanged do not exist in the dataset.")
  
  # Сохраняем порядок столбцов
  original_order <- names(data)
  
  # Сохраняем временную шкалу и заменяем её на числовой ряд
  original_time <- data[[time]]  # Исходная временная шкала
  data[[time]] <- seq_along(data[[time]])  # Замена временной шкалы на числовой ряд
  
  # Извлечение временной шкалы в числовом формате
  start_index <- min(data[[time]], na.rm = TRUE)
  end_index <- max(data[[time]], na.rm = TRUE)
  
  # --- Функции для обработки данных ---
  
  # Заполнение пропусков в середине
  fill_missing_in_middle <- function(column) {
    if (all(is.na(column))) return(column) # Если весь столбец NA, возвращаем его
    first_non_na <- which(!is.na(column))[1]
    last_non_na <- which(!is.na(column))[length(which(!is.na(column)))]
    ts_data <- ts(column[first_non_na:last_non_na], frequency = frequency)
    imputed_values <- na.interp(ts_data)
    column[first_non_na:last_non_na] <- as.numeric(imputed_values)
    return(column)
  }
  
  # Прогнозирование пропусков в начале столбца
  forecast_column_start <- function(column, start_index, end_index) {
    if (all(is.na(column))) return(column) # Если весь столбец NA, возвращаем его
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
  
  # Прогнозирование пропусков в конце столбца
  forecast_column_end <- function(column, start_index, end_index) {
    if (all(is.na(column))) return(column) # Если весь столбец NA, возвращаем его
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
  
  # Столбцы для обработки
  columns_to_process <- setdiff(names(data), c(time, cols))
  
  # Заполнение пропусков в середине
  imputed_data <- data %>%
    mutate(across(all_of(columns_to_process), fill_missing_in_middle))
  
  # Прогнозирование пропусков в начале
  result_start <- imputed_data %>%
    mutate(across(all_of(columns_to_process), ~ forecast_column_start(., start_index, end_index)))
  
  # Прогнозирование пропусков в конце
  result_end <- imputed_data %>%
    mutate(across(all_of(columns_to_process), ~ forecast_column_end(., start_index, end_index)))
  
  # Объединение данных
  processed_data <- map2_dfc(result_end[columns_to_process], result_start[columns_to_process], coalesce)
  
  # Составляем итоговый датафрейм с сохранением исходной временной шкалы
  final_result <- data %>%
    select(all_of(cols), all_of(time)) %>%
    bind_cols(processed_data) %>%
    mutate(!!time := original_time) %>%  # Восстанавливаем временную шкалу
    select(all_of(original_order))  # Восстанавливаем порядок столбцов
  
  return(final_result)
}

# --- Пример использования функции ---

# Пример данных
df <- data.frame(
  Date = 2000:2020,
  Unit = "mln dollars",
  Variable1 = c(NA, NA, 115, 120, 130, 135, 140, NA, NA, 160, 165, 170, 180, 190, 200, 210, 220, 230, 240, NA, NA),
  Variable2 = c(NA, 50, 55, 60, NA, 70, 75, 80, NA, 95, 100, NA, 110, 115, NA, 125, 130, 135, NA, NA, NA)
)

# Заполнение пропусков и прогнозирование
final_result <- fill_forecast_ets(data = df, time = "Date", frequency = 1, cols = "Unit")

# Пример с месячными данными
df_monthly <- data.frame(
  Date = seq(as.Date("2000-01-01"), as.Date("2001-12-01"), by = "month"),
  Unit = "mln dollars",
  Variable1 = c(NA, 120, 125, 130, 135, NA, 145, 150, NA, 160, 165, 170, NA, 180, 185, 190, NA, 200, 205, 210, 215, 220, NA, NA)
)

# Заполнение пропусков и прогнозирование (месячные данные)
final_result_monthly <- fill_forecast_ets(data = df_monthly, time = "Date", frequency = 12, cols = "Unit")