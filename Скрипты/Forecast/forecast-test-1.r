# Установка и загрузка необходимых пакетов
library(forecast)

# Создадим искусственный датафрейм с месячными данными
set.seed(123) # для воспроизводимости
dates <- seq.Date(from = as.Date("2020-01-01"), 
                  to = as.Date("2021-12-01"), 
                  by = "month")

# Создадим три показателя, в некоторых местах добавим NA
indicator1 <- rnorm(length(dates), mean = 10, sd = 2)
indicator2 <- rnorm(length(dates), mean = 20, sd = 5)
indicator3 <- rnorm(length(dates), mean = 100, sd = 10)

# Вставим пропуски в случайных местах
indicator1[c(3, 4, 5, 7, 15)] <- NA
indicator2[c(1, 2, 10, 20)] <- NA
indicator3[c(5, 6, 12, 23, 24)] <- NA

# Формируем датафрейм
df <- data.frame(Date = dates, 
                 Indicator1 = indicator1, 
                 Indicator2 = indicator2, 
                 Indicator3 = indicator3)

# Посмотрим на исходные данные
head(df)

# Определим периодичность. Предположим, что данные помесячные:
freq <- 12

# Определим начальную точку временного ряда (год и месяц начала)
start_year <- as.numeric(format(min(df$Date), "%Y"))
start_month <- as.numeric(format(min(df$Date), "%m"))

# Создадим новый датафрейм для итоговых данных
imputed_df <- df

for (col_name in names(df)[-1]) {
  ts_data <- ts(df[[col_name]], 
                start = c(start_year, start_month), 
                frequency = freq)
  
  # na.interp из пакета forecast также учитывает сезонность через STL
  imputed_ts <- na.interp(ts_data)
  
  imputed_df[[col_name]] <- as.numeric(imputed_ts)
}

head(imputed_df)