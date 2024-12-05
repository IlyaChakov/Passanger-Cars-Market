# Установка библиотеки
library(dplyr)

# Пример данных
df <- data.frame(
  Year = 2000:2020,
  Value1 = c(NA, NA, 110, 120, 130, NA, NA, NA, 160, 170, 180, NA, NA, 210, 220, NA, 240, NA, 260, 270, NA),
  Value2 = c(NA, NA, 220, 230, 240, 250, NA, NA, 280, 290, 300, NA, NA, NA, 350, 360, NA, 380, 390, NA, 410)
)

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

# Применение функции ко всем числовым столбцам
df_filled <- df %>%
  mutate(across(where(is.numeric) & !contains("Year"), fill_with_arithmetic_progression))

# Вывод результата
print(df_filled)