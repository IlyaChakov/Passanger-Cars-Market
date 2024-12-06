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

# Функция для интерполяции значений с сохранением NA
interpolate_column <- function(column) {
  # Проверяем, есть ли хотя бы два ненулевых значения для интерполяции
  if (sum(!is.na(column)) > 1) {
    return(approx(seq_along(column), column, seq_along(column), method = "linear", rule = 1, ties = "ordered")$y)
  } else {
    return(column) # Возвращаем как есть, если интерполяция невозможна
  }
}

# Функция для заполнения пропусков арифметической прогрессией
fill_with_arithmetic_progression <- function(column) {
  # Проверяем, есть ли NA
  if (!any(is.na(column))) {
    return(column)
  }
  
  # Найти первый и последний известные значения
  first_non_na <- which(!is.na(column))[1]
  last_non_na <- which(!is.na(column))[length(which(!is.na(column)))]
  
  # Оставляем NA в начале и в конце
  if (first_non_na > 1) {
    column[1:(first_non_na - 1)] <- NA
  }
  if (last_non_na < length(column)) {
    column[(last_non_na + 1):length(column)] <- NA
  }
  
  # Заполнение пропусков между известными значениями
  non_na_indices <- which(!is.na(column))
  for (i in seq_along(non_na_indices[-1])) {
    start_index <- non_na_indices[i]
    end_index <- non_na_indices[i + 1]
    
    # Заполнение пропусков между двумя известными значениями
    if (end_index - start_index > 1) {
      step <- (column[end_index] - column[start_index]) / (end_index - start_index)
      column[(start_index + 1):(end_index - 1)] <- column[start_index] + step * seq_len(end_index - start_index - 1)
    }
  }
  
  return(column)
}

# Функция для заполнения пропусков в начале на основе линейной экстраполяции
fill_start_with_extrapolation <- function(column) {
  # Проверяем, есть ли пропуски в начале
  if (is.na(column[1])) {
    # Находим индексы известных значений
    known_indices <- which(!is.na(column))
    
    # Если есть хотя бы два известных значения, можно построить модель
    if (length(known_indices) >= 2) {
      # Создаем линейную модель на основе известных данных
      model <- lm(column[known_indices] ~ known_indices)
      
      # Находим индексы пропусков в начале
      missing_start <- 1:(min(known_indices) - 1)
      
      # Если есть пропуски в начале
      if (length(missing_start) > 0) {
        # Прогнозируем значения для пропусков
        column[missing_start] <- predict(model, newdata = data.frame(known_indices = missing_start))
      }
    }
  }
  
  return(column)
}

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

# Укажите последний год, до которого должен быть выполнен прогноз
end_year <- 2025

# Применение интерполяции ко всем числовым столбцам
df_filled <- df %>%
  mutate(across(where(is.numeric) & !contains("Year"), ~ interpolate_column(.)))

# Применение функции ко всем числовым столбцам
df_filled <- df_filled %>%
  mutate(across(where(is.numeric) & !contains("Year"), fill_with_arithmetic_progression))

# Применение функции ко всем числовым столбцам
df_filled <- df_filled %>%
  mutate(across(where(is.numeric), fill_start_with_extrapolation))

# Прогнозирование для всех столбцов, кроме `Year`
forecast_results <- df_filled %>%
  select(-Year) %>%
  summarise(across(everything(), ~ list(forecast_column(., min(df$Year), end_year)))) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Forecast") %>%
  unnest(cols = c(Forecast)) %>%
  pivot_wider(names_from = Variable, values_from = Value)