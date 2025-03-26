separar_colunas_partido <- function(fun_data) {
  colunas_partidos <- c("PMDB", "PRN", "PSL", "PSDB", "PT")
  fun_data$partido <- apply(fun_data[, colunas_partidos], 1, function(x) {
    if (x["PMDB"] == 1) {
      return("PMDB")
    } else if (x["PRN"] == 1) {
      return("PRN")
    } else if (x["PSL"] == 1) {
      return("PSL")
    } else if (x["PSDB"] == 1) {
      return("PSDB")
    } else if (x["PT"] == 1) {
      return("PT")
    } else {
      return(NA)
    }
  })
  
  return(fun_data)
}

reduzir_para_modelo <- function(fun_data){
  fun_data <- fun_data[, !names(fun_data) %in% c("number", "date", "text", "length", "wordfish", "partido")]
  
  return (fun_data)
}