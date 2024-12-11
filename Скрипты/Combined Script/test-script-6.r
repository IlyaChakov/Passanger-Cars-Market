library(forecast)
library(prophet)

# Пример временного ряда с сезонностью и разнонаправленной тенденцией
data <- ts(c(100, 120, 110, 90, 80, 85, 90, 95, 100, 110, 105, 103), frequency = 1)

# STL-декомпозиция и прогноз
stl_model <- stlm(data, s.window = "periodic")  # Используем "periodic" для регулярной сезонности
stl_forecast <- forecast(stl_model, h = 12)    # Прогноз на 12 периодов

# Вывод прогноза
print(stl_forecast)
plot(stl_forecast)

# ARIMA с автоматическим подбором параметров
arima_model <- auto.arima(data, seasonal = TRUE)
arima_forecast <- forecast(arima_model, h = 12)

# Вывод прогноза
print(arima_forecast)
plot(arima_forecast)

# Holt-Winters с сезонностью
hw_model <- HoltWinters(data)
hw_forecast <- forecast(hw_model, h = 12)

# Вывод прогноза
print(hw_forecast)
plot(hw_forecast)

# Прогноз с использованием нейронной сети
nnetar_model <- nnetar(data)
nnetar_forecast <- forecast(nnetar_model, h = 12)

# Вывод прогноза
print(nnetar_forecast)
plot(nnetar_forecast)

# Пример данных для Prophet
df <- data.frame(ds = seq.Date(as.Date("2020-01-01"), as.Date("2020-12-01"), by = "month"),
                 y = c(100, 120, 110, 150, 130, 170, 140, 180, 150, 190, 160, 200))

# Прогноз с использованием Prophet
prophet_model <- prophet(df)
future <- make_future_dataframe(prophet_model, periods = 12, freq = "month")
prophet_forecast <- predict(prophet_model, future)

# Вывод прогноза
print(tail(prophet_forecast[, c("ds", "yhat")]))
plot(prophet_model, prophet_forecast)

###

# Пример данных
df <- data.frame(
  ds = seq(as.Date("2020-01-01"), by = "month", length.out = 12),
  y = c(100, 110, 120, 115, 130, 125, 140, 135, 150, 145, 160, 155)
)

# Прогноз Prophet
prophet_model <- prophet(df)
future <- make_future_dataframe(prophet_model, periods = 12, freq = "month")
prophet_forecast <- predict(prophet_model, future)

# Вывод прогноза
print(tail(prophet_forecast[, c("ds", "yhat")]))
plot(prophet_model, prophet_forecast)

###

# Пример данных
df <- data.frame(
  x = 1:12,  # Временная шкала
  y = c(100, 110, 120, 115, 130, 125, 140, 135, 150, 145, 160, 155)
)

# Линейная регрессия
model <- lm(y ~ x, data = df)

# Прогноз на будущее
future <- data.frame(x = 13:24)  # Прогнозируем на следующие 12 точек
future$y <- predict(model, newdata = future)

# Итоговый прогноз
forecast <- rbind(df, future)
print(forecast)

# График
plot(df$x, df$y, type = "b", col = "blue", ylim = range(forecast$y), xlab = "Time", ylab = "Value")
lines(forecast$x, forecast$y, col = "red", type = "b")

###

# Пример данных
data <- ts(c(100, 110, 120, 115, 130, 125, 140, 135, 150, 145, 160, 155), frequency = 12)

# Holt-Winters
hw_model <- HoltWinters(data, gamma = FALSE)  # gamma = FALSE означает отсутствие сезонности
hw_forecast <- forecast(hw_model, h = 12)    # Прогноз на 12 периодов

# Вывод прогноза
print(hw_forecast)
plot(hw_forecast)

###

# Прогноз с использованием нейронной сети
nnetar_model <- nnetar(data)
nnetar_forecast <- forecast(nnetar_model, h = 12)

# Вывод прогноза
print(nnetar_forecast)
plot(nnetar_forecast)
