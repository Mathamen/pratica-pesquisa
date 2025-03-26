criar_colunas_dummies_por_indice <- function(dataset, indice_coluna) {
  # Verifica se o índice é válido
  if (indice_coluna < 1 || indice_coluna > ncol(dataset)) {
    stop("O índice especificado é inválido.")
  }
  
  # Obtém o nome da coluna com base no índice
  coluna <- colnames(dataset)[indice_coluna]
  
  # Gera as colunas dummies
  dummies <- model.matrix(~ 0 + get(coluna), data = dataset)
  
  # Renomeia as colunas dummies para um formato mais limpo
  colnames(dummies) <- sub("get\\(coluna\\)", coluna, colnames(dummies))
  
  # Adiciona as colunas dummies ao dataset original
  dataset <- cbind(dataset, dummies)
  
  return(dataset)
}

data_fase3 <- criar_colunas_dummies_por_indice(data_fase2, 84)



calcular_percentual_1 <- function(dataset, indice_coluna) {
  # Verifica se o índice é válido
  if (indice_coluna < 1 || indice_coluna > ncol(dataset)) {
    stop("O índice especificado é inválido.")
  }
  
  # Obtém a coluna com base no índice
  coluna <- dataset[[indice_coluna]]
  
  # Verifica se a coluna contém apenas valores binários (0 e 1)
  if (!all(coluna %in% c(0, 1))) {
    stop("A coluna deve conter apenas valores binários (0 e 1).")
  }
  
  # Calcula o percentual de valores 1
  percentual_1 <- (sum(coluna == 1) / length(coluna)) 
  
  return(percentual_1)
}




separar_treino_teste <- function(dataset){
  sr <- sample_random()
  sr <- train_test(sr, dataset)
  return(sr)
}

reduzir_treino_mandato <- function(parte_treino, mandato, regras_mandato) {
  # Verifica se a coluna "date" existe no conjunto de treino
  if (!"date" %in% colnames(parte_treino)) {
    stop("A coluna 'date' não foi encontrada em parte_treino.")
  }
  
  # Encontra a regra correspondente ao mandato fornecido
  regra <- Filter(function(r) r$mandato == mandato, regras_mandato)
  
  # Verifica se o mandato foi encontrado
  if (length(regra) == 0) {
    stop("O mandato especificado não foi encontrado nas regras.")
  }
  
  # Obtém os anos de início e fim do mandato
  ano_inicio_mandato <- regra[[1]]$inicio
  ano_fim_mandato <- regra[[1]]$fim
  
  # Filtra os discursos que ocorreram até o final do mandato
  parte_treino_filtrada <- parte_treino[parte_treino$date <= ano_fim_mandato, ]
  
  return(parte_treino_filtrada)
}


#ALTEREI PARA O DS COMPLETO
gerar_classificador_parte4 <- function(fun_data, partido_coluna, classificador,discursos_train,discursos_test,  should_print = FALSE){
  set.seed(42)
  
  # Embaralhar os dados
  fun_data <- fun_data[sample(nrow(fun_data)), ]
  
  fun_data[[partido_coluna]] <- factor(fun_data[[partido_coluna]])
  fun_data <- fun_data[sample(nrow(fun_data)), ]
  slevels <- levels(fun_data[[partido_coluna]])
  
  
  discursos <- cbind(as.matrix(fun_data[, 11:83]), Parties = fun_data[[partido_coluna]])
  discursos_train <- cbind(as.matrix(discursos_train[, 11:83]), Parties = fun_data[[partido_coluna]])
  discursos_test <- cbind(as.matrix(discursos_test[, 11:83]), Parties = fun_data[[partido_coluna]])
  
  set.seed(5)
  #sr <- sample_random()
  #sr <- train_test(sr, discursos)
  #discursos_train <- sr$train
  #discursos_test <- sr$test
  
  tbl <- rbind(
    table(discursos[,"Parties"]),
    table(discursos_train[,"Parties"]),
    table(discursos_test[,"Parties"])
  )
  rownames(tbl) <- c("dataset", "training", "test")
  if (should_print) {
    print(tbl)
  }
  
  
  # Sempre que quiser colocar mais um modelo, só alterar aqui!
  model <- switch(classificador,
                  "knn" = cla_knn("Parties", slevels, k=5),
                  "decision_tree" = cla_dtree("Parties", slevels),
                  "random_forest" = cla_rf("Parties", slevels, mtry=3, ntree=5),
                  "mlp" = cla_mlp("Parties", slevels, size = 5, decay = 0.03),
                  stop("Sem esse modelo"))
  
  
  model <- fit(model, discursos_train)
  
  train_prediction <- predict(model, discursos_train)
  discursos_train_predictand <- adjust_class_label(discursos_train[,"Parties"])
  train_eval <- evaluate(model, discursos_train_predictand, train_prediction)
  if (should_print) {
    print(train_eval$metrics)
  }
  
  discursos_prediction <- predict(model, discursos_test)
  discursos_test_predictand <- adjust_class_label(discursos_test[,"Parties"])
  test_eval <- evaluate(model, discursos_test_predictand, discursos_prediction)
  if (should_print) {
    print(test_eval$metrics)
  }
  return (test_eval$metrics$accuracy)
}



















result_class <- gerar_classificador(data_fase3,"mandatoLula1",classificador = "mlp")
percent <- calcular_percentual_1(data_fase3,92)
final <- result_class - percent
print(final)


#-----------------------
sample <- separar_treino_teste(data_fase3)
treino <- sample$train
teste <- sample$test

treino_reduzido <- reduzir_treino_mandato(treino,"Lula1",regras_mandato) #obter em partidos.R

result2 <- gerar_classificador_parte4(data_fase3,"mandatoLula1",classificador="random_forest",treino_reduzido,teste)
print(result2)
final <- result2 - percent
print(final)

result2 <- gerar_classificador_parte4(data_fase3,"mandatoLula1",classificador="knn",treino_reduzido,teste)
print(result2)
final <- result2 - percent
print(final)

result2 <- gerar_classificador_parte4(data_fase3,"mandatoLula1",classificador="mlp",treino_reduzido,teste)
print(result2)
final <- result2 - percent
print(final)

result2 <- gerar_classificador_parte4(data_fase3,"mandatoLula1",classificador="decision_tree",treino_reduzido,teste)
print(result2)
final <- result2 - percent
print(final)







print(percent)
final <- result2 - percent
print(final)


