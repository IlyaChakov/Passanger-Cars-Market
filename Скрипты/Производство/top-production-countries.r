# Библиотеки
library(forecast)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(forecast)
library(data.table)

df <- read_tsv("Обработанные Данные\\Производство\\production-countries.tsv")

df <- df %>%
    mutate(across(where(is.numeric), ~ ifelse(. < 0, 0, .)))

top_10 <- df %>%
    filter(Date == 2023) %>%
    arrange(desc(Объем))

top_10 <- top_10 %>%
  slice_max(order_by = Объем, n = 10)

countries <- top_10$Страна

df_top_10 <- df %>%
    filter(Date >= 2010, Страна %in% countries) %>%
    group_by(Страна)

table_top_10 <- df_top_10 %>%
    pivot_wider(
        names_from = "Страна",
        values_from = "Объем"
    )

df_others <- df %>%
    filter(Date >= 2010, !Страна %in% countries) %>%
    group_by(Date) %>%
    summarise(total_value = sum(Объем, na.rm = TRUE)) %>%
    arrange(Date)

table_top_10$Остальные <- df_others$total_value

fwrite(table_top_10, file = "Итоговые Таблицы\\Производство\\top-production-countries.tsv", sep = "\t", quote = FALSE)
