# Установка библиотек
library(forecast)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)

# Пример данных
df <- data.frame(
  Date = 2000:2020,
  Unit = "mln dollars",
  Variable = c(NA, NA, 115, 120, 130, 135, 140, NA, NA, 160, 165, 170, 180, 190, 200, 210, 220, 230, 240, NA, NA)
)

# Установка частоты данных (месячные данные)
freq <- 1

# Укажите диапазон лет для прогноза
start_year <- min(df$Date)
end_year <- max(df$Date)

# Новый датафрейм для заполненных данных
imputed_df <- df

for (col_name in names(df)[-1]) { 
  col <- df[[col_name]]
  
  # Определение индексов первого и последнего непустого значения
  first_non_na <- which(!is.na(col))[1]
  last_non_na <- which(!is.na(col))[length(which(!is.na(col)))]
  
  # Создание временного ряда только для диапазона
  ts_data <- ts(col[first_non_na:last_non_na], 
                frequency = freq)
  
  # Заполнение пропусков в пределах диапазона
  imputed_values <- na.interp(ts_data)
  
  # Объединение данных: пропуски сохраняются в начале и конце
  col_imputed <- col
  col_imputed[first_non_na:last_non_na] <- as.numeric(imputed_values)
  
  # Обновление итогового датафрейма
  imputed_df[[col_name]] <- col_imputed
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
  return(data.frame(Date = full_years, Value = full_values))
}

# Прогнозирование для всех столбцов, кроме `Date`
forecast_results <- imputed_df %>%
  select(-Date) %>%
  summarise(across(everything(), ~ list(forecast_column(., min(imputed_df$Date), end_year)))) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Forecast") %>%
  unnest(cols = c(Forecast)) %>%
  pivot_wider(names_from = Variable, values_from = Value)


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
  # Прогнозируем все столбцы, кроме временной шкалы `Date`
  processed_columns <- df %>%
    select(-Date) %>%
    mutate(across(everything(), ~ forecast_column_start(., start_year, end_year)))
  
  # Добавляем временную шкалу обратно
  processed_df <- cbind(Date = df$Date, processed_columns)
  return(processed_df)
}

###

# Прогнозирование пропусков в начале
result <- process_forecast_start(imputed_df, start_year, end_year)

# Функция для объединения двух датафреймов
merge_datasets <- function(forecast_results, result) {
  # Применяем coalesce к каждому столбцу
  combined_df <- map2_dfc(forecast_results, result, coalesce)
  return(combined_df)
}

# Объединяем два датасета
final_result <- merge_datasets(forecast_results, result)