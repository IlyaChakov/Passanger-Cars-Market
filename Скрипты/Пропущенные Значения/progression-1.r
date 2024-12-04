# Установка библиотеки
library(dplyr)

# Пример данных
df <- data.frame(
  Year = 2000:2020,
  Value1 = c(100, NA, NA, 120, 130, NA, NA, NA, 160, 170, 180, NA, NA, 210, 220, NA, 240, NA, 260, 270, NA),
  Value2 = c(200, NA, 220, NA, NA, 250, NA, NA, 280, 290, 300, NA, NA, NA, 350, 360, NA, 380, 390, NA, 410)
)

# Функция для заполнения пропусков арифметической прогрессией
fill_with_arithmetic_progression <- function(column) {
  # Проверяем, есть ли NA
  if (!any(is.na(column))) {
    return(column)
  }
  
  # Индексы ненулевых значений
  non_na_indices <- which(!is.na(column))
  
  # Если есть хотя бы два ненулевых значения
  for (i in seq_along(non_na_indices[-1])) {
    start_index <- non_na_indices[i]
    end_index <- non_na_indices[i + 1]
    
    # Заполнение пропусков между ними
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
