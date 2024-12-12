# Библиотеки
library(forecast)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(forecast)
library(data.table)

df <- read_csv("Обработанные Данные\\Регулирование\\automobile-tariffs-by-country-2024.csv")

top_5 <- df %>%
    arrange(AutomobileTariffsDutyRate) %>%
    slice_max(order_by = AutomobileTariffsDutyRate, n = 5)

bottom_5 <- df %>%
    arrange(AutomobileTariffsDutyRate) %>%
    slice_min(order_by = AutomobileTariffsDutyRate, n = 5)

top_countries <- top_5$country
bottom_countries <- bottom_5$country

df_merge <- rbind(top_5, bottom_5)

df_merge <- df_merge %>%
    rename(Tariff = AutomobileTariffsDutyRate)

df_merge$AutomobileTariffsSalesTax <- NULL

fwrite(df_merge, file = "Итоговые Таблицы\\Регулирование\\top-tariffs.tsv", sep = "\t", quote = FALSE)
