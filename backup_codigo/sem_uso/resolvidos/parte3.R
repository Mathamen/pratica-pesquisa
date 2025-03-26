library(caret)

# FEITO

criar_e_salvar_treino_teste <- function(fun_data, coluna_alvo = "mandato") {
  caminho_arquivo <- "./dados_treino_teste.Rdata"
  set.seed(42)  # Garantir reprodutibilidade
  
  # Verificar se a coluna alvo existe
  if (!coluna_alvo %in% colnames(fun_data)) {
    stop("Coluna alvo não encontrada no dataset!")
  }
  
  # Divisão estratificada
  indices_treino <- createDataPartition(fun_data[[coluna_alvo]], p = 0.8, list = FALSE)
  
  discursos_train <- fun_data[indices_treino, ]
  discursos_test <- fun_data[-indices_treino, ]
  
  # Salvar no arquivo .RData
  save(discursos_train, discursos_test, file = caminho_arquivo)
  cat("Conjuntos de treino e teste salvos em", caminho_arquivo, "\n")
}

criar_e_salvar_treino_teste(data_fase2)



carregar_treino_teste <- function(caminho_arquivo) {
  if (!file.exists(caminho_arquivo)) {
    stop("Arquivo não encontrado: ", caminho_arquivo)
  }
  load(caminho_arquivo)  # Carrega discursos_train e discursos_test
  return(list(train = discursos_train, test = discursos_test))
}


treino_teste <- carregar_treino_teste("./dados_treino_teste.Rdata")
