# Библиотеки
library(forecast)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(forecast)
library(data.table)

df <- read_tsv("Обработанные Данные\\Продажи\\sales-countries.tsv")

top_10 <- df %>%
    filter(Date == 2023) %>%
    arrange(desc(Volume))

top_10 <- top_10 %>%
  slice_max(order_by = Volume, n = 10)

countries <- top_10$Country

df_top_10 <- df %>%
    filter(Date >= 2010, Country %in% countries) %>%
    group_by(Country)

table_top_10 <- df_top_10 %>%
    pivot_wider(
        names_from = "Country",
        values_from = "Volume"
    )

df_others <- df %>%
    filter(Date >= 2010, !Country %in% countries) %>%
    group_by(Date) %>%
    summarise(total_value = sum(Volume, na.rm = TRUE)) %>%
    arrange(Date)

table_top_10$Остальные <- df_others$total_value

fwrite(table_top_10, file = "Итоговые Таблицы\\Продажи\\top-sales-countries.tsv", sep = "\t", quote = FALSE)
