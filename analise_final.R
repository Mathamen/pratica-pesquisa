add_column_to_dataframe <- function(df1, df2, column_name, new_column_name = NULL) {
  # Verifica se os dataframes têm o mesmo número de linhas
  if (nrow(df1) != nrow(df2)) {
    stop("Os dataframes devem ter o mesmo número de linhas.")
  }
  
  # Verifica se a coluna existe no primeiro dataframe
  if (!(column_name %in% colnames(df1))) {
    stop(paste("A coluna", column_name, "não existe no primeiro dataframe."))
  }
  
  # Usa o nome original ou renomeia a nova coluna
  new_column_name <- ifelse(is.null(new_column_name), column_name, new_column_name)
  
  # Adiciona a coluna ao segundo dataframe
  df2[[new_column_name]] <- df1[[column_name]]
  
  return(df2)
}



calculate_accuracy_by_year <- function(df, year_column, accuracy_column) {
  # Verifica se as colunas existem no dataframe
  if (!(year_column %in% colnames(df))) {
    stop(paste("A coluna", year_column, "não existe no dataframe."))
  }
  if (!(accuracy_column %in% colnames(df))) {
    stop(paste("A coluna", accuracy_column, "não existe no dataframe."))
  }
  
  # Agrupa por ano e calcula a porcentagem de acertos
  result <- df %>%
    group_by(!!sym(year_column)) %>%
    summarize(
      total = n(),
      acertos = sum(!!sym(accuracy_column), na.rm = TRUE),
      porcentagem_acerto = (acertos / total) * 100
    ) %>%
    arrange(!!sym(year_column))
  
  return(result)
}

report7 <- add_column_to_dataframe(teste, report007, "date")
report6 <- add_column_to_dataframe(teste, report006, "date")
report5 <- add_column_to_dataframe(teste, report005, "date")
report4 <- add_column_to_dataframe(teste, report004, "date")
report3 <- add_column_to_dataframe(teste, report003, "date")
report2 <- add_column_to_dataframe(teste, report002, "date")

r7 <- calculate_accuracy_by_year(report7,"date","Acerto")
r6 <- calculate_accuracy_by_year(report6,"date","Acerto")
r5 <- calculate_accuracy_by_year(report5,"date","Acerto")
r4 <- calculate_accuracy_by_year(report4,"date","Acerto")
r3 <- calculate_accuracy_by_year(report3,"date","Acerto")
r2 <- calculate_accuracy_by_year(report2,"date","Acerto")