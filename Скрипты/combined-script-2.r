# Установка библиотеки
library(dplyr)
library(tidyr)
library(forecast)

# Пример данных
df <- data.frame(
  Year = 2000:2020,
  Value1 = c(100, NA, 110, 120, 130, NA, NA, 160, 170, 180, NA, 200, 210, NA, 240, NA, 260, 270, NA, NA, 310),
  Value2 = c(NA, NA, 210, NA, 230, 240, NA, 250, 260, 270, 280, NA, NA, 320, 330, NA, 350, 360, NA, 380, NA),
  Value3 = c(NA, NA, NA, NA, NA, NA, NA, 250, 260, 270, 280, NA, NA, NA, NA, NA, 350, 360, NA, NA, NA)
)

# Объединённая функция для обработки столбца
process_column <- function(column, start_year, end_year) {
  # 1. Интерполяция пропусков внутри столбца
  if (sum(!is.na(column)) > 1) {
    column <- approx(seq_along(column), column, seq_along(column), method = "linear", rule = 1, ties = "ordered")$y
  }
  
  # 2. Заполнение пропусков между известными значениями с использованием арифметической прогрессии
  if (any(is.na(column))) {
    first_non_na <- which(!is.na(column))[1]
    last_non_na <- which(!is.na(column))[length(which(!is.na(column)))]
    
    if (first_non_na > 1) {
      column[1:(first_non_na - 1)] <- NA
    }
    if (last_non_na < length(column)) {
      column[(last_non_na + 1):length(column)] <- NA
    }
    
    non_na_indices <- which(!is.na(column))
    for (i in seq_along(non_na_indices[-1])) {
      start_index <- non_na_indices[i]
      end_index <- non_na_indices[i + 1]
      
      if (end_index - start_index > 1) {
        step <- (column[end_index] - column[start_index]) / (end_index - start_index)
        column[(start_index + 1):(end_index - 1)] <- column[start_index] + step * seq_len(end_index - start_index - 1)
      }
    }
  }
  
  # 3. Заполнение пропусков в начале с использованием линейной экстраполяции
  if (is.na(column[1])) {
    known_indices <- which(!is.na(column))
    if (length(known_indices) >= 2) {
      model <- lm(column[known_indices] ~ known_indices)
      missing_start <- 1:(min(known_indices) - 1)
      if (length(missing_start) > 0) {
        column[missing_start] <- predict(model, newdata = data.frame(known_indices = missing_start))
      }
    }
  }
  
  # 4. Прогнозирование недостающих значений до указанного конца года
  last_known_index <- max(which(!is.na(column)))
  last_known_year <- start_year + last_known_index - 1
  full_years <- seq(start_year, end_year)
  ts_data <- ts(column[1:last_known_index], start = start_year, frequency = 1)
  
  forecast_horizon <- end_year - last_known_year
  if (forecast_horizon > 0) {
    ets_model <- ets(ts_data)
    forecast_values <- forecast(ets_model, h = forecast_horizon)$mean
    full_values <- c(column[1:last_known_index], as.numeric(forecast_values))
  } else {
    full_values <- column[1:length(full_years)]
  }
  
  # Возвращаем обработанные значения
  return(data.frame(Year = full_years, Value = full_values))
}

# Укажите последний год прогноза
end_year <- 2025

# Обработка всех числовых столбцов
df_transformed <- df %>%
  select(-Year) %>%
  summarise(across(everything(), ~ list(process_column(., min(df$Year), end_year)))) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Processed") %>%
  unnest(cols = c(Processed)) %>%
  pivot_wider(names_from = Variable, values_from = Value)

# Добавляем колонку с годами
df_transformed <- df_transformed %>%
  mutate(Year = seq(min(df$Year), end_year))

# Вывод результата
print(df_transformed)