# Установка библиотеки
library(forecast)
library(ggplot2)

# Исходный датафрейм
df <- data.frame(
  Year = 2000:2020,
  Value = c(100, 110, 115, 120, 130, 135, 140, 150, 155, 160, 165, 170, 180, 190, 200, 210, 220, 230, 240, 250, 260)
)

# Преобразование данных в временной ряд
ts_data <- ts(df$Value, start = min(df$Year), frequency = 1)

# Построение ETS модели
ets_model <- ets(ts_data)

# Вывод информации о модели
print(summary(ets_model))

# Прогнозирование на 5 лет вперёд
forecast_ets <- forecast(ets_model, h = 5)

# Преобразование прогноза обратно в датафрейм
forecast_df <- data.frame(
  Year = (max(df$Year) + 1):(max(df$Year) + length(forecast_ets$mean)),
  Forecast = as.numeric(forecast_ets$mean),
  Lower_80 = as.numeric(forecast_ets$lower[, 1]),
  Upper_80 = as.numeric(forecast_ets$upper[, 1]),
  Lower_95 = as.numeric(forecast_ets$lower[, 2]),
  Upper_95 = as.numeric(forecast_ets$upper[, 2])
)

# Объединение с исходными данными
combined_df <- rbind(
  df,
  data.frame(Year = forecast_df$Year, Value = forecast_df$Forecast)
)

# Вывод прогнозируемых данных
print(forecast_df)

# Визуализация исходных данных и прогноза
ggplot() +
  geom_line(data = df, aes(x = Year, y = Value), color = "blue", size = 1, linetype = "solid") +
  geom_line(data = forecast_df, aes(x = Year, y = Forecast), color = "red", size = 1, linetype = "dashed") +
  geom_ribbon(data = forecast_df, aes(x = Year, ymin = Lower_80, ymax = Upper_80), alpha = 0.2, fill = "orange") +
  geom_ribbon(data = forecast_df, aes(x = Year, ymin = Lower_95, ymax = Upper_95), alpha = 0.1, fill = "grey") +
  labs(
    title = "Прогноз временного ряда с использованием ETS",
    x = "Год",
    y = "Значение"
  ) +
  theme_minimal()