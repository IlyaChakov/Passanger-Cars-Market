# Загрузка необходимых библиотек
library(tidyr)
library(readr)  # Если вам нужна функция read_csv()
library(dplyr)
library(data.table)
library(forecast)

df <- read_tsv("Исходные Данные\\Торговля\\export-unit.tsv")

# Замена названий столбцов
names(df) <- gsub("-Объемы экспорта", "", names(df))  # Убираем "экспорт " из названий столбцов

df <- df %>%
  select(-contains("Единица"))

df <- df %>%
    select(-last_col())

df <- df[df$Экспортеры != "Весь Мир", ]

df <- df %>%
    pivot_longer(
        cols = -"Экспортеры",
        names_to = "Год",
        values_to = "Объем"
    )

df$Год <- as.numeric(df$Год)
df$Экспортеры <- as.character(df$Экспортеры)
df$Объем <- as.numeric(df$Объем)

df$Объем <- df$Объем / 1000000

df$'Ед. Изм.' <- "млн штук"

df <- df %>%
  relocate(`Ед. Изм.`, .after = Год)

df <- df %>%
    pivot_wider(
        id_cols = c(Год, `Ед. Изм.`),
        names_from = "Экспортеры",
        values_from = "Объем"
    )

# Замена всех числовых ячеек с нулём на NA
df <- df %>%
  mutate(across(where(is.numeric), ~ ifelse(. == 0, NA, .)))

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
df <- df %>%
  mutate(across(where(is.numeric) & !contains("Год"), ~ interpolate_column(.)))

df <- df %>%
  mutate(across(where(is.numeric) & !contains("Год"), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Функция для прогнозирования по столбцу
forecast_ets <- function(column, years, h) {
  ts_data <- ts(column, start = min(years), frequency = 1)  # Преобразование в временной ряд
  ets_model <- ets(ts_data)                                # Построение ETS модели
  forecast(ets_model, h = h)                               # Прогнозирование
}

# Количество лет для прогноза
horizon <- 1

# Применение прогнозирования ко всем столбцам, кроме `Year`
forecast_results <- df %>%
  select(-Год, -`Ед. Изм.`) %>%                                      # Убираем колонку `Year` для обработки
  summarise(across(everything(), ~ list(forecast_ets(., df$Год, horizon)))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Forecast") %>%
  mutate(Forecast = lapply(Forecast, function(x) {
    data.frame(
      Year = max(df$Год) + seq_along(x$mean),
      Forecast = as.numeric(x$mean)
    )
  })) %>%
  unnest(cols = c(Forecast))

# Итоговый прогнозируемый датафрейм
forecast_df <- forecast_results %>%
  pivot_wider(names_from = Variable, values_from = Forecast) %>%
  rename(Год =Year) %>%
  mutate(`Ед. Изм.` = "млн штук") %>%
  relocate(`Ед. Изм.`, .after = Год)

# Объединение с исходными данными
combined_df <- df %>%
  bind_rows(forecast_df)

df_long <- combined_df %>%
  pivot_longer(
    cols = -c(Год, `Ед. Изм.`),  # Все столбцы, кроме "Год" и "Ед. Изм."
    names_to = "Страна",         # Новый столбец для названий стран
    values_to = "Объем"          # Новый столбец для значений
  )

fwrite(df_long, file = "export-countries.tsv", sep = "\t", quote = FALSE)