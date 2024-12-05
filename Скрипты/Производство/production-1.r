# Загрузка необходимых библиотек
library(tidyr)
library(readr)  # Если вам нужна функция read_csv()
library(dplyr)
library(data.table)
library(forecast)

df <- read_csv("Исходные Данные\\Производство\\Car Production Statistics.csv")

summary <- df %>%
    select(where(~ is.character(.) | is.factor(.))) %>%
    summarise(across(everything(), ~ list(unique(.)))) %>%
    unnest(cols = everything())

df <- df %>%
    pivot_wider(
        id_cols = c(Date, Unit),
        names_from = "country",
        values_from = "Value"
    )

df <- df %>%
  mutate(across(where(is.numeric), ~ ifelse(. == 0, NA, .)))

fill_interpolation <- function(column) {
  # Проверяем, есть ли хотя бы два ненулевых значения для интерполяции
  if (sum(!is.na(column)) > 1) {
    return(approx(seq_along(column), column, seq_along(column), method = "linear", rule = 1, ties = "ordered")$y)
  } else {
    return(column) # Возвращаем как есть, если интерполяция невозможна
  }
}

df <- df %>%
  mutate(across(where(is.numeric) & !contains("Date"), ~ fill_interpolation(.)))

# Замена NA на среднее значение в определённых столбцах
fill_mean <- c("Colombia", "Kazakhstan", "Morocco")  # Укажите нужные столбцы

df <- df %>%
  mutate(across(all_of(fill_mean), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Укажите последний год, до которого должен быть выполнен прогноз
end_year <- 2025

# Функция для прогнозирования по столбцу
forecast_ets <- function(column, start_year, end_year) {
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
  summarise(across(everything(), ~ list(forecast_ets(., min(df$Year), end_year)))) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Forecast") %>%
  unnest(cols = c(Forecast)) %>%
  pivot_wider(names_from = Variable, values_from = Value)

# Объединение с `Year`
forecast_df <- forecast_results %>%
  mutate(Year = seq(min(df$Year), end_year))