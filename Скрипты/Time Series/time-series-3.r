# Установка библиотек
library(forecast)
library(dplyr)
library(tidyr)
library(purrr)

# Пример данных
df <- data.frame(
  Date = 2000:2020,
  Unit = "mln dollars",
  Variable1 = c(NA, NA, 115, 120, 130, 135, 140, NA, NA, 160, 165, 170, 180, 190, 200, 210, 220, 230, 240, NA, NA),
  Variable2 = c(NA, 50, 55, 60, NA, 70, 75, 80, NA, 95, 100, NA, 110, 115, NA, 125, 130, 135, NA, NA, NA)
)

# Установка частоты данных (годовые данные)
freq <- 1

# Укажите диапазон лет для прогноза
start_year <- min(df$Date)
end_year <- max(df$Date)

# Функция для заполнения пропусков в середине столбца
fill_missing_in_middle <- function(column) {
  # Определение индексов первого и последнего непустого значения
  first_non_na <- which(!is.na(column))[1]
  last_non_na <- which(!is.na(column))[length(which(!is.na(column)))]
  
  # Создание временного ряда только для диапазона
  ts_data <- ts(column[first_non_na:last_non_na], frequency = freq)
  
  # Заполнение пропусков в пределах диапазона
  imputed_values <- na.interp(ts_data)
  
  # Объединение данных: пропуски сохраняются в начале и конце
  column[first_non_na:last_non_na] <- as.numeric(imputed_values)
  
  return(column)
}

# Функция для прогнозирования пропущенных значений в начале столбца
forecast_column_start <- function(column, start_year, end_year) {
  # Найти первый известный индекс
  first_known_index <- min(which(!is.na(column)))
  
  # Если нет пропущенных значений в начале, вернуть столбец без изменений
  if (first_known_index == 1) {
    return(column)
  }
  
  # Извлечение известных значений
  known_values <- column[first_known_index:length(column)]
  known_start_year <- start_year + first_known_index - 1
  
  # Создаем временной ряд для известных значений (обращаем порядок)
  ts_data <- ts(rev(known_values), start = known_start_year, frequency = 1)
  
  # Рассчитываем горизонт прогноза (количество пропущенных лет в начале)
  forecast_horizon <- first_known_index - 1
  
  # Прогнозируем значения для пропусков
  if (forecast_horizon > 0) {
    ets_model <- ets(ts_data)
    forecast_values <- forecast(ets_model, h = forecast_horizon)$mean
    forecast_values <- rev(as.numeric(forecast_values)) # Возвращаем в прямой порядок
    
    # Вставляем спрогнозированные значения в начало
    full_values <- c(forecast_values, known_values)
  } else {
    full_values <- known_values
  }
  
  # Дополняем до полной длины исходного столбца
  return(c(full_values, rep(NA, length(column) - length(full_values))))
}

# Функция для прогнозирования пропущенных значений в конце столбца
forecast_column_end <- function(column, start_year, end_year) {
  # Находим последний известный год и данные
  last_known_index <- max(which(!is.na(column)))
  last_known_year <- start_year + last_known_index - 1
  
  # Если данные заканчиваются раньше end_year, заполняем временной ряд
  ts_data <- ts(column[1:last_known_index], start = start_year, frequency = 1)
  
  # Прогнозируем недостающие значения
  forecast_horizon <- end_year - last_known_year
  if (forecast_horizon > 0) {
    ets_model <- ets(ts_data)
    forecast_values <- forecast(ets_model, h = forecast_horizon)$mean
    full_values <- c(column[1:last_known_index], as.numeric(forecast_values))
  } else {
    full_values <- column  # Если прогноз не нужен
  }
  
  return(full_values)
}

# Заполнение пропусков в середине
imputed_df <- df %>%
  mutate(across(where(is.numeric) & !any_of(c("Date")), fill_missing_in_middle))

# Прогнозирование пропусков в начале
result_start <- imputed_df %>%
  mutate(across(where(is.numeric) & !any_of(c("Date")), ~ forecast_column_start(., start_year, end_year)))

# Прогнозирование пропусков в конце
result_end <- imputed_df %>%
  mutate(across(where(is.numeric) & !any_of(c("Date")), ~ forecast_column_end(., start_year, end_year)))

# Функция для объединения двух датафреймов
merge_datasets <- function(forecast_results, result, original_df) {
  # Применяем coalesce к каждому столбцу
  combined_df <- map2_dfc(forecast_results, result, coalesce)
  
  # Добавляем все неизменяемые столбцы
  combined_df <- combined_df %>%
    mutate(across(where(is.character), ~ original_df[[cur_column()]]))
  
  return(combined_df)
}

# Объединяем данные с прогнозами
final_result <- merge_datasets(result_end, result_start, df)

# Вывод результата
print(final_result)
