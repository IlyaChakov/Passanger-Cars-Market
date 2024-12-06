library(forecast)

# Создание искусственного датафрейма с пропусками
set.seed(123)
dates <- seq.Date(from = as.Date("2020-01-01"), 
                  to = as.Date("2021-12-01"), 
                  by = "month")

indicator1 <- rnorm(length(dates), mean = 10, sd = 2)
indicator2 <- rnorm(length(dates), mean = 20, sd = 5)
indicator3 <- rnorm(length(dates), mean = 100, sd = 10)

indicator1[c(3, 4, 5, 7, 15)] <- NA
indicator2[c(1, 2, 10, 20)] <- NA
indicator3[c(5, 6, 12, 23, 24)] <- NA

df <- data.frame(Date = dates, 
                 Indicator1 = indicator1, 
                 Indicator2 = indicator2, 
                 Indicator3 = indicator3)

# Установка частоты данных (месячные данные)
freq <- 12

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

# Вывод первых строк итогового датафрейма
print(imputed_df)
