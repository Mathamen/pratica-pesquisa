#criar_e_salvar_treino_teste <- function(fun_data, coluna_alvo = "mandato") {
#  caminho_arquivo <- "./dados_treino_teste.Rdata"
 # set.seed(42)  # Garantir reprodutibilidade
  
  # Verificar se a coluna alvo existe
#  if (!coluna_alvo %in% colnames(fun_data)) {
 #   stop("Coluna alvo não encontrada no dataset!")
#  }
  
  # Divisão estratificada
  #indices_treino <- createDataPartition(fun_data[[coluna_alvo]], p = 0.8, list = FALSE)
  
  #discursos_train <- fun_data[indices_treino, ]
  #discursos_test <- fun_data[-indices_treino, ]
  
  # Salvar no arquivo .RData
  #save(discursos_train, discursos_test, file = caminho_arquivo)
 # cat("Conjuntos de treino e teste salvos em", caminho_arquivo, "\n")
#}

#criar_e_salvar_treino_teste(data_fase2)



#carregar_treino_teste <- function(caminho_arquivo) {
#  if (!file.exists(caminho_arquivo)) {
#    stop("Arquivo não encontrado: ", caminho_arquivo)
#  }
#  load(caminho_arquivo)  # Carrega discursos_train e discursos_test
#  return(list(train = discursos_train, test = discursos_test))
#}
salvar_csv <- function(dataframe, caminho) {
  if (!grepl("\\.csv$", caminho)) {
    caminho <- paste0(caminho, ".csv")
  }
  write.csv(dataframe, file = caminho, row.names = FALSE)
  cat("Dataframe salvo como CSV em:", caminho, "\n")
}

renomear_colunas <- function(df_original, df_novas_colunas) {
  # Verifica se ambos os dataframes têm o mesmo número de colunas
  if (ncol(df_original) != ncol(df_novas_colunas)) {
    stop("Os dataframes devem ter o mesmo número de colunas.")
  }
  
  # Renomeia as colunas do dataframe original
  colnames(df_original) <- colnames(df_novas_colunas)
  
  return(df_original)
}




#salvar_rdata <- function(objeto, caminho) {
  # Verifica se o caminho termina com .RData
 # if (!grepl("\\.RData$", caminho)) {
  #  caminho <- paste0(caminho, ".RData")
  #}
  
  # Salva o objeto no caminho especificado
  #save(objeto, file = caminho)
  #cat("Arquivo salvo em:", caminho, "\n")
#}




#carregar_rdata <- function(caminho) {
  # Verifica se o arquivo existe
 # if (!file.exists(caminho)) {
#    stop("O arquivo não foi encontrado:", caminho)
#  }
  
  # Carrega o conteúdo do arquivo em um ambiente temporário
 # env_temp <- new.env()
#  load(caminho, envir = env_temp)
  
  # Retorna os objetos carregados como uma lista
 # return(as.list(env_temp))
#}

