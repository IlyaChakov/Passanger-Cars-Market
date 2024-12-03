# Установка библиотеки
library(dplyr)

# Пример данных с пропущенными значениями
df <- data.frame(
  Year = 2000:2020,
  Value1 = c(NA, 110, 115, NA, 130, 135, 140, NA, 155, 160, 165, NA, 180, 190, 200, NA, 220, 230, NA, 250, 260),
  Value2 = c(200, NA, NA, 220, 230, 240, 245, 250, 255, 260, NA, NA, 280, NA, 300, 310, 320, 330, 340, 350, 360)
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

# Применение интерполяции ко всем числовым столбцам
df_filled <- df %>%
  mutate(across(where(is.numeric) & !contains("Year"), ~ interpolate_column(.)))

# Вывод результата
print(df_filled)