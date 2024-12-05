# Установка библиотек
library(forecast)
library(dplyr)
library(tidyr)
library(ggplot2)

# Пример данных
df <- data.frame(
  Year = 2000:2020,
  Value1 = c(100, 110, 115, 120, 130, 135, 140, 150, 155, 160, 165, 170, 180, 190, 200, 210, 220, NA, NA, NA, NA),
  Value2 = c(200, 210, 215, 220, 230, 240, 245, 250, 255, 260, 265, 270, 280, 290, 300, NA, NA, NA, NA, NA, NA)
)

# Укажите последний год, до которого должен быть выполнен прогноз
end_year <- 2025

# Функция для прогнозирования по столбцу
forecast_column <- function(column, start_year, end_year) {
  # Находим последний известный год и данные
  last_known_index <- max(which(!is.na(column)))
  last_known_year <- start_year + last_known_index - 1
  
  # Если данные заканчиваются раньше end_year, заполняем временной ряд
  full_years <- seq(start_year, end_year)
  ts_data <- ts(column[1:last_known_index], start = start_year, frequency = 1)
  
  # Прогнозируем недостающие значения
  forecast_horizon <- end_year - last_known_year
  if (forecast_horizon > 0) {
    ets_model <- ets(ts_data)
    forecast_values <- forecast(ets_model, h = forecast_horizon)$mean
    full_values <- c(column[1:last_known_index], as.numeric(forecast_values))
  } else {
    full_values <- column[1:length(full_years)]  # Если прогноз не нужен
  }
  
  # Возвращаем значения для всех годов от start_year до end_year
  return(data.frame(Year = full_years, Value = full_values))
}

# Прогнозирование для всех столбцов, кроме `Year`
forecast_results <- df %>%
  select(-Year) %>%
  summarise(across(everything(), ~ list(forecast_column(., min(df$Year), end_year)))) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Forecast") %>%
  unnest(cols = c(Forecast)) %>%
  pivot_wider(names_from = Variable, values_from = Value)

# Объединение с `Year`
forecast_df <- forecast_results %>%
  mutate(Year = seq(min(df$Year), end_year))

# Вывод результата
print(forecast_df)

# Визуализация
forecast_df_long <- forecast_df %>%
  pivot_longer(cols = -Year, names_to = "Variable", values_to = "Value")

ggplot(forecast_df_long, aes(x = Year, y = Value, color = Variable)) +
  geom_line(size = 1) +
  labs(
    title = "Прогноз значений для нескольких столбцов",
    x = "Год",
    y = "Значение"
  ) +
  theme_minimal()
