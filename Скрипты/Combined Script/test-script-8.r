library(forecast)
library(dplyr)

fill_forecast_ets <- function(data, time = "Date", frequency = 1, cols = "Unit", n_periods = 5) {
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
  filled_data <- data %>%
    select(all_of(cols), all_of(time)) %>%
    bind_cols(processed_data) %>%
    mutate(!!time := original_time) %>%  # Восстанавливаем временную шкалу
    select(all_of(original_order))  # Восстанавливаем порядок столбцов
  
  # --- Прогноз на n_periods вперед ---
  forecast_future <- function(column) {
    ts_data <- ts(column, frequency = frequency)  # Преобразование в временной ряд
    ets_model <- ets(ts_data)                     # ETS модель
    forecast_values <- forecast(ets_model, h = n_periods)$mean  # Прогноз
    return(as.numeric(forecast_values))           # Возвращаем прогноз
  }
  
  # Прогнозируем будущее для каждого столбца
  future_values <- map_dfc(columns_to_process, ~ forecast_future(filled_data[[.x]]))
  colnames(future_values) <- columns_to_process
  
  # Расширяем временную шкалу для будущего
  future_time <- seq(max(original_time, na.rm = TRUE) + 1, by = 1, length.out = n_periods)
  
  # Создаем датафрейм для будущих данных
  future_data <- tibble(!!time := future_time, !!!setNames(future_values, columns_to_process))
  
  # Объединяем текущие и будущие данные
  final_result <- bind_rows(filled_data, future_data)
  
  return(final_result)
}


# Пример данных
df <- data.frame(
  Date = 2000:2020,
  Unit = "mln dollars",
  Variable1 = c(NA, NA, 115, 120, 130, 135, 140, NA, NA, 160, 165, 170, 180, 190, 200, 210, 220, 230, 240, NA, NA),
  Variable2 = c(NA, 50, 55, 60, NA, 70, 75, 80, NA, 95, 100, NA, 110, 115, NA, 125, 130, 135, NA, NA, NA)
)

# Прогноз на 5 периодов вперед
result <- fill_forecast_ets(data = df, time = "Date", frequency = 1, cols = "Unit", n_periods = 5)

# Вывод результата
print(result)
