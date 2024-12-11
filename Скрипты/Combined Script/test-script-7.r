library(forecast)

# Пример данных
data <- ts(c(100, 110, 120, 115, 130, 125, 140, 135, 150, 145, 160, 155, 170), frequency = 1)

# Прогноз с использованием нейронной сети
nnetar_model <- nnetar(data)
nnetar_forecast <- forecast(nnetar_model, h = 12)

# Вывод прогноза
print(nnetar_forecast)
plot(nnetar_forecast)