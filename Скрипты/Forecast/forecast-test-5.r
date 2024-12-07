library(forecast)
library(dplyr)

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

# Функция для обработки всех столбцов
process_forecast_start <- function(df, start_year, end_year) {
  # Прогнозируем все столбцы, кроме временной шкалы `Year`
  processed_columns <- df %>%
    select(-Year) %>%
    mutate(across(everything(), ~ forecast_column_start(., start_year, end_year)))
  
  # Добавляем временную шкалу обратно
  processed_df <- cbind(Year = df$Year, processed_columns)
  return(processed_df)
}

# --- Пример использования ---
# Примерный датафрейм
df <- data.frame(
  Year = 2000:2020,
  Value1 = c(NA, NA, 2.3, 3.1, 4.5, 5.2, 6.1, 7.2, 8.3, 9.1, 10.2, 11.3, 12.1, 13.2, 14.1, 15.3, 16.2, NA, NA, NA, NA),
  Value2 = c(NA, 4.2, 5.1, 6.2, 7.3, 8.1, 9.2, 10.3, 11.1, 12.2, 13.3, 14.1, 15.2, 16.3, 17.1, NA, NA, NA, NA, NA, NA)
)

# Укажите диапазон лет для прогноза
start_year <- min(df$Year)
end_year <- max(df$Year)

# Прогнозирование пропусков в начале
result <- process_forecast_start(df, start_year, end_year)

# Вывод результата
print(result)
