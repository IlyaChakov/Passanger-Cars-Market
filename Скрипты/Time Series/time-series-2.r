# Установка библиотек
library(forecast)
library(dplyr)
library(tidyr)
library(purrr)

# Пример данных
df <- data.frame(
  Date = 2000:2020,
  Unit = "mln dollars",
  Variable = c(NA, NA, 115, 120, 130, 135, 140, NA, NA, 160, 165, 170, 180, 190, 200, 210, 220, 230, 240, NA, NA)
)

# Установка частоты данных (годовые данные)
freq <- 1

# Укажите диапазон лет для прогноза
start_year <- min(df$Date)
end_year <- max(df$Date)

# Новый датафрейм для заполненных данных (с заполнением пропусков в середине)
imputed_df <- df

# Заполнение пропусков в середине столбцов
for (col_name in names(df)[-c(1, 2)]) { 
  col <- df[[col_name]]
  
  # Определение индексов первого и последнего непустого значения
  first_non_na <- which(!is.na(col))[1]
  last_non_na <- which(!is.na(col))[length(which(!is.na(col)))]
  
  # Создание временного ряда только для диапазона
  ts_data <- ts(col[first_non_na:last_non_na], frequency = freq)
  
  # Заполнение пропусков в пределах диапазона
  imputed_values <- na.interp(ts_data)
  
  # Объединение данных: пропуски сохраняются в начале и конце
  col_imputed <- col
  col_imputed[first_non_na:last_non_na] <- as.numeric(imputed_values)
  
  # Обновление итогового датафрейма
  imputed_df[[col_name]] <- col_imputed
}

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

# Функция для прогнозирования пропущенных значений в конце столбца
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
  return(full_values)
}

# Прогнозирование пропусков в начале столбца
result <- imputed_df %>%
  mutate(across(-c(Date, Unit), ~ forecast_column_start(., start_year, end_year)))

# Прогнозирование пропусков в конце столбца
forecast_results <- imputed_df %>%
  mutate(across(-c(Date, Unit), ~ forecast_column(., start_year, end_year)))

# Функция для объединения двух датафреймов
merge_datasets <- function(forecast_results, result, original_df) {
  # Применяем coalesce к каждому столбцу
  combined_df <- map2_dfc(forecast_results, result, coalesce)
  
  # Добавляем столбец Unit из оригинального датафрейма
  combined_df <- combined_df %>%
    mutate(Unit = original_df$Unit)
  
  return(combined_df)
}

# Объединяем два датасета
final_result <- merge_datasets(forecast_results, result, df)

# Вывод результата
print(final_result)