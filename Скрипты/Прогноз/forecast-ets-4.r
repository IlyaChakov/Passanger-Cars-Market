# Установка библиотек
library(forecast)
library(dplyr)
library(tidyr)
library(ggplot2)

# Пример данных
df <- data.frame(
  Year = 2000:2020,
  Value1 = c(100, 110, 115, 120, 130, 135, 140, 150, 155, 160, 165, 170, 180, 190, 200, 210, 220, 230, 240, 250, NA),
  Value2 = c(200, 210, 215, 220, 230, 240, 245, 250, 255, 260, 265, 270, 280, 290, 300, 310, 320, 330, 340, 350, 360)
)

# Функция для прогнозирования по столбцу
forecast_column <- function(column, years, h) {
  ts_data <- ts(column, start = years[1], frequency = 1)  # Преобразование в временной ряд
  ets_model <- ets(ts_data)                                # Построение ETS модели
  forecast_result <- forecast(ets_model, h = h)            # Прогнозирование
  return(forecast_result)
}

# Количество лет для прогноза
horizon <- 5

# Вычисление последнего года в данных
last_year <- max(df$Year)

# Применение прогнозирования ко всем столбцам, кроме `Year`
forecast_results <- df %>%
  select(-Year) %>%                                       # Убираем колонку `Year` для обработки
  summarise(across(everything(), ~ list(forecast_column(., df$Year, horizon)))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Forecast") %>%
  mutate(Forecast = lapply(Forecast, function(x) {
    # Количество пропущенных лет до последнего года в данных
    missing_years <- last_year - length(x$mean)
    # Добавляем пропущенные года
    forecast_years <- seq(last_year + 1 - missing_years, last_year + horizon)
    data.frame(
      Year = forecast_years,
      Forecast = as.numeric(x$mean)
    )
  })) %>%
  unnest(cols = c(Forecast))

# Итоговый прогнозируемый датафрейм
forecast_df <- forecast_results %>%
  pivot_wider(names_from = Variable, values_from = Forecast)

# Объединение с исходными данными
combined_df <- df %>%
  bind_rows(forecast_df)

# Вывод результата
print(combined_df)

# Визуализация
combined_df_long <- combined_df %>%
  pivot_longer(cols = -Year, names_to = "Variable", values_to = "Value")

ggplot(combined_df_long, aes(x = Year, y = Value, color = Variable)) +
  geom_line(size = 1) +
  labs(
    title = "Прогноз значений для нескольких столбцов",
    x = "Год",
    y = "Значение"
  ) +
  theme_minimal()
