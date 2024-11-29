load_libraries <- function() {
  packages <- c("dplyr", "ggplot2", "gridExtra", "tidyr", "arules", "daltoolbox")
  # Lista de pacotes necessários
  
  # Verificar e instalar pacotes
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package, dependencies = TRUE) # Instala o pacote se necessário
      library(package, character.only = TRUE)        # Carrega o pacote após instalar
    }
  }
}

carregar_arquivos <- function(padrao = "\\.R$", isolado = FALSE) {
  diretorio <- "functions"
  arquivos <- list.files(path = diretorio, pattern = padrao, full.names = TRUE)
  
  for (arquivo in arquivos) {
    message("Carregando arquivo: ", arquivo)
    source(arquivo, local = isolado)
  }
  
  message("Todos os arquivos foram carregados.")
}