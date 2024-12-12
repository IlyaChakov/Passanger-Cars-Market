# Библиотеки
library(forecast)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(forecast)
library(data.table)

df <- read_csv("Обработанные Данные\\OEC\\Relatedness-vs-Country-Complexity-2022.csv")

df <- df %>%
    select(c("Relatedness", "Trade Value ECI", "Trade Value"))

df$`Trade Value` <- df$`Trade Value` / 1000000000

fwrite(df, file = "Итоговые Таблицы\\OEC\\OEC-plot.csv", sep = "\t", quote = FALSE)
