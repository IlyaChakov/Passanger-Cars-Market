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
  
  # 1. Обработка пропусков в начале столбца
  first_non_na <- which(!is.na(column))[1] # Индекс первого ненулевого значения
  
  if (first_non_na > 1 && length(column) > first_non_na) { 
    # Убедимся, что есть следующее значение для расчёта шага
    next_non_na <- column[first_non_na + 1]
    if (!is.na(next_non_na)) {
      step <- (next_non_na - column[first_non_na]) / (first_non_na + 1)
      column[1:(first_non_na - 1)] <- column[first_non_na] - step * rev(seq_len(first_non_na - 1))
    }
  }
  
  # 2. Обработка пропусков внутри столбца
  non_na_indices <- which(!is.na(column))
  
  for (i in seq_along(non_na_indices[-1])) {
    start_index <- non_na_indices[i]
    end_index <- non_na_indices[i + 1]
    
    # Заполнение пропусков между двумя ненулевыми значениями
    if (end_index - start_index > 1) {
      step <- (column[end_index] - column[start_index]) / (end_index - start_index)
      column[(start_index + 1):(end_index - 1)] <- column[start_index] + step * seq_len(end_index - start_index - 1)
    }
  }
  
  # Пропуски в конце остаются NA
  return(column)
}

# Применение функции ко всем числовым столбцам
df_filled <- df %>%
  mutate(across(where(is.numeric) & !contains("Year"), fill_with_arithmetic_progression))

# Вывод результата
print(df_filled)
