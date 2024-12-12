# Установка пакета для работы с данными
library(readr)

# Функция для генерации LaTeX таблицы
convert_tsv_to_latex <- function(input_file, output_file) {
  # Считывание TSV файла
  data <- read_tsv(input_file, show_col_types = FALSE)
  
  # Получение количества столбцов
  num_cols <- ncol(data)
  
  # Формирование начала таблицы
  latex_table <- c(
    "\\rowcolors{2}{gray!10}{white}",
    paste0("\\begin{tabularx}{\\textwidth}{|", paste(rep("c|", num_cols), collapse = ""), "}"),
    "    \\hline"
  )
  
  # Формирование строки с заголовками
  header_row <- paste0(
    "\\rowcolor{black} ",
    paste(
      paste0("\\textcolor{white}{\\textbf{", names(data), "}}"),
      collapse = " & "
    ),
    " \\\\ \\hline"
  )
  latex_table <- c(latex_table, header_row)
  
  # Формирование строк данных
  for (i in 1:nrow(data)) {
    row <- data[i, ]
    data_row <- paste(paste(row, collapse = " & "), "\\\\ \\hline")
    latex_table <- c(latex_table, data_row)
  }
  
  # Закрытие таблицы
  latex_table <- c(latex_table, "\\end{tabularx}")
  
  # Запись LaTeX таблицы в файл
  writeLines(latex_table, output_file, useBytes = TRUE)
}

# Использование функции
convert_tsv_to_latex("Обработанные Данные\\Россия\\table-6.tsv", "Презентация\\Media\\Tables\\table-6.tex")
