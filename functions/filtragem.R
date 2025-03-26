filtrar_por_1 <- function(df, coluna) {
  resultado <- df[df[[coluna]] == 1, ]
  return(resultado)
}