# Установка библиотек
library(dplyr)

# Пример данных
df <- data.frame(
  Year = 2000:2020,
  Value1 = c(NA, NA, 110, 120, 130, NA, NA, 160, 170, 180, NA, 200, 210, NA, 240, NA, 260, 270, NA, NA, 310),
  Value2 = c(NA, 200, 210, NA, 230, 240, NA, 250, 260, 270, 280, NA, NA, 320, 330, NA, 350, 360, NA, 380, NA)
)

# Функция для заполнения пропусков в начале на основе линейной экстраполяции
fill_start_with_extrapolation <- function(column) {
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
  
  return(column)
}

# Применение функции ко всем числовым столбцам
df_filled <- df %>%
  mutate(across(where(is.numeric), fill_start_with_extrapolation))

# Вывод результата
print(df_filled)