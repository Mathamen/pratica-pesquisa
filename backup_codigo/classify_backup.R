gerar_modelo_kmeans <- function(data, data_novo, should_eval =TRUE){
  # um cluster para cada partido
  model <- cluster_kmeans(k=5)
  model <- fit(model, data[,11:20])
  clu <- cluster(model, data[,11:20])
  table(clu)
  eval <- evaluate(model, clu, data_novo$partido)
  
  if (should_eval == TRUE)
    print(eval)
  
  
  # agora com o min max 
  data_novo_minmax <- transform(fit(minmax(), data_novo), data_novo)
  model <- cluster_kmeans(k=5)
  model <- fit(model, data_novo_minmax[,11:20])
  clu <- cluster(model, data_novo_minmax[,11:20])
  table(clu)
  eval <- evaluate(model, clu, data_novo_minmax$partido)
  
  if (should_eval == TRUE)
    print(eval)   
  
  
  return (model)
}


gerar_mlp_uncut <- function(fun_data, partido_coluna, should_print = TRUE, seed = 42) {
  # Definir a seed para reprodutibilidade
  set.seed(seed)
  
  # Embaralhar os dados
  fun_data <- fun_data[sample(nrow(fun_data)), ]
  
  fun_data[[partido_coluna]] <- factor(fun_data[[partido_coluna]])
  
  fun_data <- fun_data %>% select(-date)
  fun_data <- fun_data[sample(nrow(fun_data)), ]
  slevels <- levels(fun_data[[partido_coluna]])
  
  discursos <- cbind(as.matrix(fun_data[, 10:19]), Parties = fun_data[[partido_coluna]])
  
  set.seed(5)
  sr <- sample_random()
  sr <- train_test(sr, discursos)
  discursos_train <- sr$train
  discursos_test <- sr$test
  
  tbl <- rbind(
    table(discursos[,"Parties"]),
    table(discursos_train[,"Parties"]),
    table(discursos_test[,"Parties"])
  )
  rownames(tbl) <- c("dataset", "training", "test")
  if (should_print) {
    print(tbl)
  }
  
  model <- cla_mlp("Parties", slevels, size = 5, decay = 0.03)
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
  #return(model)
}


gerar_modelo_mlp <- function(data_novo,eval = TRUE){
  #Questão 4
  
  data_novo$partido <- factor(data_novo$partido)
  data_novo <- data_novo %>% select(-date)
  
  # Agora você pode obter os níveis corretamente
  slevels <- levels(data_novo$partido)
  #print(slevels)
  
  
  discursos <- cbind(as.matrix(data_novo[,11:20]), Parties=data_novo$partido)
  
  set.seed(1)
  sr <- sample_random()
  sr <- train_test(sr, discursos)
  discursos_train <- sr$train
  discursos_test <- sr$test
  
  
  tbl <- rbind(table(discursos[,"Parties"]), 
               table(discursos_train[,"Parties"]), 
               table(discursos_test[,"Parties"]))
  rownames(tbl) <- c("dataset", "training", "test")
  head(tbl)
  
  
  model <- cla_mlp("Parties", slevels, size=5,decay=0.03)
  model <- fit(model, discursos_train)
  train_prediction <- predict(model, discursos_train)
  
  
  discursos_train_predictand <- adjust_class_label(discursos_train[,"Parties"])
  train_eval <- evaluate(model, discursos_train_predictand, train_prediction)
  print(train_eval$metrics)
  
  
  # Test  
  discursos_prediction <- predict(model, discursos_test)
  
  discursos_test_predictand <- adjust_class_label(discursos_test[,"Parties"])
  test_eval <- evaluate(model, discursos_test_predictand, discursos_prediction)
  print(test_eval$metrics)
  
  return (model)
  
  
}



gerar_mlp_global <-function(fun_data, should_print = TRUE){
  fun_data$partido <- factor(fun_data$partido)
  fun_data <- fun_data %>% select(-date)
  
  fun_data <- fun_data[sample(nrow(fun_data)), ]
  
  
  # Agora você pode obter os níveis corretamente
  slevels <- levels(fun_data$partido)
  #print(slevels)
  
  
  discursos <- cbind(as.matrix(fun_data[,10:19]), Parties=fun_data$partido)
  
  set.seed(5)
  sr <- sample_random()
  sr <- train_test(sr, discursos)
  discursos_train <- sr$train
  discursos_test <- sr$test
  
  
  tbl <- rbind(table(discursos[,"Parties"]), 
               table(discursos_train[,"Parties"]), 
               table(discursos_test[,"Parties"]))
  rownames(tbl) <- c("dataset", "training", "test")
  head(tbl)
  
  
  model <- cla_mlp("Parties", slevels, size=5,decay=0.03)
  model <- fit(model, discursos_train)
  train_prediction <- predict(model, discursos_train)
  
  
  discursos_train_predictand <- adjust_class_label(discursos_train[,"Parties"])
  train_eval <- evaluate(model, discursos_train_predictand, train_prediction)
  print(train_eval$metrics)
  
  
  # Test  
  discursos_prediction <- predict(model, discursos_test)
  
  discursos_test_predictand <- adjust_class_label(discursos_test[,"Parties"])
  test_eval <- evaluate(model, discursos_test_predictand, discursos_prediction)
  print(test_eval$metrics)
  
  return (model)
  
}




gerar_mlp <- function(fun_data, partido_coluna, should_print = TRUE, seed = 42) {
  # Definir a seed para reprodutibilidade
  set.seed(seed)
  
  # Embaralhar os dados
  fun_data <- fun_data[sample(nrow(fun_data)), ]
  
  fun_data[[partido_coluna]] <- factor(fun_data[[partido_coluna]])
  fun_data <- fun_data[sample(nrow(fun_data)), ]
  slevels <- levels(fun_data[[partido_coluna]])
  
  discursos <- cbind(as.matrix(fun_data[, 6:15]), Parties = fun_data[[partido_coluna]])
  
  set.seed(5)
  sr <- sample_random()
  sr <- train_test(sr, discursos)
  discursos_train <- sr$train
  discursos_test <- sr$test
  
  tbl <- rbind(
    table(discursos[,"Parties"]),
    table(discursos_train[,"Parties"]),
    table(discursos_test[,"Parties"])
  )
  rownames(tbl) <- c("dataset", "training", "test")
  if (should_print) {
    print(tbl)
  }
  
  model <- cla_mlp("Parties", slevels, size = 5, decay = 0.03)
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
  #return(model)
}

gerar_decision_tree <- function(fun_data, partido_coluna, should_print = TRUE, seed = 42){
  set.seed(seed)
  
  # Embaralhar os dados
  fun_data <- fun_data[sample(nrow(fun_data)), ]
  
  fun_data[[partido_coluna]] <- factor(fun_data[[partido_coluna]])
  fun_data <- fun_data[sample(nrow(fun_data)), ]
  slevels <- levels(fun_data[[partido_coluna]])
  
  discursos <- cbind(as.matrix(fun_data[, 6:15]), Parties = fun_data[[partido_coluna]])
  
  set.seed(5)
  sr <- sample_random()
  sr <- train_test(sr, discursos)
  discursos_train <- sr$train
  discursos_test <- sr$test
  
  tbl <- rbind(
    table(discursos[,"Parties"]),
    table(discursos_train[,"Parties"]),
    table(discursos_test[,"Parties"])
  )
  rownames(tbl) <- c("dataset", "training", "test")
  if (should_print) {
    print(tbl)
  }
  
  model <- cla_dtree("Parties", slevels)
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

gerar_knn <- function(fun_data, partido_coluna, should_print = TRUE, seed = 42){
  set.seed(seed)
  
  # Embaralhar os dados
  fun_data <- fun_data[sample(nrow(fun_data)), ]
  
  fun_data[[partido_coluna]] <- factor(fun_data[[partido_coluna]])
  fun_data <- fun_data[sample(nrow(fun_data)), ]
  slevels <- levels(fun_data[[partido_coluna]])
  
  discursos <- cbind(as.matrix(fun_data[, 6:15]), Parties = fun_data[[partido_coluna]])
  
  set.seed(5)
  sr <- sample_random()
  sr <- train_test(sr, discursos)
  discursos_train <- sr$train
  discursos_test <- sr$test
  
  tbl <- rbind(
    table(discursos[,"Parties"]),
    table(discursos_train[,"Parties"]),
    table(discursos_test[,"Parties"])
  )
  rownames(tbl) <- c("dataset", "training", "test")
  if (should_print) {
    print(tbl)
  }
  
  model <-cla_knn("Parties", slevels, k=5)
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

gerar_random_forest <- function(fun_data, partido_coluna, should_print = TRUE, seed = 42){
  set.seed(seed)
  
  # Embaralhar os dados
  fun_data <- fun_data[sample(nrow(fun_data)), ]
  
  fun_data[[partido_coluna]] <- factor(fun_data[[partido_coluna]])
  fun_data <- fun_data[sample(nrow(fun_data)), ]
  slevels <- levels(fun_data[[partido_coluna]])
  
  discursos <- cbind(as.matrix(fun_data[, 6:15]), Parties = fun_data[[partido_coluna]])
  
  set.seed(5)
  sr <- sample_random()
  sr <- train_test(sr, discursos)
  discursos_train <- sr$train
  discursos_test <- sr$test
  
  tbl <- rbind(
    table(discursos[,"Parties"]),
    table(discursos_train[,"Parties"]),
    table(discursos_test[,"Parties"])
  )
  rownames(tbl) <- c("dataset", "training", "test")
  if (should_print) {
    print(tbl)
  }
  
  model <-cla_rf("Parties", slevels, mtry=3, ntree=5)
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







gerar_classificador <- function(fun_data, partido_coluna, should_print = FALSE, classificador){
  set.seed(42)
  
  # Embaralhar os dados
  fun_data <- fun_data[sample(nrow(fun_data)), ]
  
  fun_data[[partido_coluna]] <- factor(fun_data[[partido_coluna]])
  fun_data <- fun_data[sample(nrow(fun_data)), ]
  slevels <- levels(fun_data[[partido_coluna]])
  
  discursos <- cbind(as.matrix(fun_data[, 6:15]), Parties = fun_data[[partido_coluna]])
  
  set.seed(5)
  sr <- sample_random()
  sr <- train_test(sr, discursos)
  discursos_train <- sr$train
  discursos_test <- sr$test
  
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




batch_classificador <- function(fun_data,classificador) {
  partidos <- c("PMDB", "PRN", "PSL", "PSDB", "PT")
  somaDiscursos <- 5888  # Número total de discursos
  
  # Calcular a soma de cada partido
  somas <- sapply(partidos, function(partido) sum(fun_data[[partido]] == 1))
  
  cat("---------------------\n")
  
  # Loop sobre os partidos
  for (partido in partidos) {
    cat(partido, "\n")
    temp <- gerar_classificador(fun_data, partido, FALSE,classificador)
    cat(temp, "\n")
    resultado <- temp - (somas[partido] / somaDiscursos)
    cat("Resultado:", resultado, "\n")
    cat("---------------------\n")
  }
}





# Print mais bonito
batch_mlp <- function(fun_data) {
  partidos <- c("PMDB", "PRN", "PSL", "PSDB", "PT")
  somaDiscursos <- 5888  # Número total de discursos
  
  # Calcular a soma de cada partido
  somas <- sapply(partidos, function(partido) sum(fun_data[[partido]] == 1))
  
  cat("---------------------\n")
  
  # Loop sobre os partidos
  for (partido in partidos) {
    cat(partido, "\n")
    temp <- gerar_mlp(fun_data, partido, FALSE)
    cat(temp, "\n")
    resultado <- temp - (somas[partido] / somaDiscursos)
    cat("Resultado:", resultado, "\n")
    cat("---------------------\n")
  }
}


batch_knn <- function(fun_data) {
  partidos <- c("PMDB", "PRN", "PSL", "PSDB", "PT")
  somaDiscursos <- 5888  # Número total de discursos
  
  # Calcular a soma de cada partido
  somas <- sapply(partidos, function(partido) sum(fun_data[[partido]] == 1))
  
  cat("---------------------\n")
  
  # Loop sobre os partidos
  for (partido in partidos) {
    cat(partido, "\n")
    temp <- gerar_knn(fun_data, partido, FALSE)
    cat(temp, "\n")
    resultado <- temp - (somas[partido] / somaDiscursos)
    cat("Resultado:", resultado, "\n")
    cat("---------------------\n")
  }
}


batch_decision_tree <- function(fun_data) {
  partidos <- c("PMDB", "PRN", "PSL", "PSDB", "PT")
  somaDiscursos <- 5888  # Número total de discursos
  
  # Calcular a soma de cada partido
  somas <- sapply(partidos, function(partido) sum(fun_data[[partido]] == 1))
  
  cat("---------------------\n")
  
  # Loop sobre os partidos
  for (partido in partidos) {
    cat(partido, "\n")
    temp <- gerar_decision_tree(fun_data, partido, FALSE)
    cat(temp, "\n")
    resultado <- temp - (somas[partido] / somaDiscursos)
    cat("Resultado:", resultado, "\n")
    cat("---------------------\n")
  }
}


batch_random_forest <- function(fun_data) {
  partidos <- c("PMDB", "PRN", "PSL", "PSDB", "PT")
  somaDiscursos <- 5888  # Número total de discursos
  
  # Calcular a soma de cada partido
  somas <- sapply(partidos, function(partido) sum(fun_data[[partido]] == 1))
  
  cat("---------------------\n")
  
  # Loop sobre os partidos
  for (partido in partidos) {
    cat(partido, "\n")
    temp <- gerar_random_forest(fun_data, partido, FALSE)
    cat(temp, "\n")
    resultado <- temp - (somas[partido] / somaDiscursos)
    cat("Resultado:", resultado, "\n")
    cat("---------------------\n")
  }
}

















































# TODO generalizar clusterizadores
gerar_modelo_kmeans <- function(data, data_novo, should_eval =TRUE){
  # um cluster para cada partido
  model <- cluster_kmeans(k=5)
  model <- fit(model, data[,11:20])
  clu <- cluster(model, data[,11:20])
  table(clu)
  eval <- evaluate(model, clu, data_novo$partido)
  
  if (should_eval == TRUE)
    print(eval)
  
  
  # agora com o min max 
  data_novo_minmax <- transform(fit(minmax(), data_novo), data_novo)
  model <- cluster_kmeans(k=5)
  model <- fit(model, data_novo_minmax[,11:20])
  clu <- cluster(model, data_novo_minmax[,11:20])
  table(clu)
  eval <- evaluate(model, clu, data_novo_minmax$partido)
  
  if (should_eval == TRUE)
    print(eval)   
  
  
  return (model)
}


# TODO deixar genérico esta função
gerar_mlp_global <-function(fun_data, should_print = TRUE){
  fun_data$partido <- factor(fun_data$partido)
  fun_data <- fun_data %>% select(-date)
  
  fun_data <- fun_data[sample(nrow(fun_data)), ]
  
  
  # Agora você pode obter os níveis corretamente
  slevels <- levels(fun_data$partido)
  #print(slevels)
  
  
  discursos <- cbind(as.matrix(fun_data[,10:19]), Parties=fun_data$partido)
  
  set.seed(5)
  sr <- sample_random()
  sr <- train_test(sr, discursos)
  discursos_train <- sr$train
  discursos_test <- sr$test
  
  
  tbl <- rbind(table(discursos[,"Parties"]), 
               table(discursos_train[,"Parties"]), 
               table(discursos_test[,"Parties"]))
  rownames(tbl) <- c("dataset", "training", "test")
  head(tbl)
  
  
  model <- cla_mlp("Parties", slevels, size=5,decay=0.03)
  model <- fit(model, discursos_train)
  train_prediction <- predict(model, discursos_train)
  
  
  discursos_train_predictand <- adjust_class_label(discursos_train[,"Parties"])
  train_eval <- evaluate(model, discursos_train_predictand, train_prediction)
  print(train_eval$metrics)
  
  
  # Test  
  discursos_prediction <- predict(model, discursos_test)
  
  discursos_test_predictand <- adjust_class_label(discursos_test[,"Parties"])
  test_eval <- evaluate(model, discursos_test_predictand, discursos_prediction)
  print(test_eval$metrics)
  
  return (model)
  
}



#ALTEREI PARA O DS COMPLETO
gerar_classificador <- function(fun_data, partido_coluna, should_print = FALSE, classificador){
  set.seed(42)
  
  # Embaralhar os dados
  fun_data <- fun_data[sample(nrow(fun_data)), ]
  
  fun_data[[partido_coluna]] <- factor(fun_data[[partido_coluna]])
  fun_data <- fun_data[sample(nrow(fun_data)), ]
  slevels <- levels(fun_data[[partido_coluna]])
  
  
  discursos <- cbind(as.matrix(fun_data[, 11:83]), Parties = fun_data[[partido_coluna]])
  
  set.seed(5)
  sr <- sample_random()
  sr <- train_test(sr, discursos)
  discursos_train <- sr$train
  discursos_test <- sr$test
  
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



# Criação de um batch para facilitar os prints
batch_classificador <- function(fun_data,classificador,global) {
  partidos <- c("PMDB", "PRN", "PSL", "PSDB", "PT")
  somaDiscursos <- 5888  # Número total de discursos
  
  # Calcular a soma de cada partido
  somas <- sapply(partidos, function(partido) sum(fun_data[[partido]] == 1))
  
  cat("---------------------\n")
  
  # Loop sobre os partidos
  for (partido in partidos) {
    cat(partido, "\n")
    temp <- gerar_classificador(fun_data, partido, classificador, should_print = FALSE)
    cat(temp, "\n")
    resultado <- temp - (somas[partido] / somaDiscursos)
    cat("Resultado:", resultado, "\n")
    cat("---------------------\n")
  }
}






















batch_classificador_mandato <- function(fun_data, classificador, global, treino_teste) {
  # Lista de mandatos com suas regras
  regras_mandato <- list(
    list(partido = "PSL", inicio = 2019, fim = 2020, mandato = "Bolsonaro"),
    list(partido = "PMDB", inicio = 2016, fim = 2018, mandato = "Temer"),
    list(partido = "PT", inicio = 2015, fim = 2016, mandato = "Dilma2"),
    list(partido = "PT", inicio = 2011, fim = 2014, mandato = "Dilma1"),
    list(partido = "PT", inicio = 2007, fim = 2010, mandato = "Lula2"),
    list(partido = "PT", inicio = 2003, fim = 2006, mandato = "Lula1"),
    list(partido = "PSDB", inicio = 1999, fim = 2002, mandato = "FHC2"),
    list(partido = "PSDB", inicio = 1995, fim = 1998, mandato = "FHC1"),
    list(partido = "PMDB", inicio = 1992, fim = 1994, mandato = "Itamar"),
    list(partido = "PRN", inicio = 1990, fim = 1992, mandato = "Collor"),
    list(partido = "PMDB", inicio = 1985, fim = 1990, mandato = "Sarney")
  )
  
  cat("---------------------\n")
  
  for (regra in regras_mandato) {
    mandato <- regra$mandato
    ano_inicio <- regra$inicio
    ano_fim <- regra$fim
    
    cat("Treinando classificador para o mandato:", mandato, "\n")
    
    # Separar dados do treino (anos <= ano_fim do mandato atual)
    treino <- treino_teste$train[treino_teste$train$date <= ano_fim, ]
    
    # Separar dados do teste (anos dentro do mandato atual)
    teste <- treino_teste$test[treino_teste$test$date >= ano_inicio & treino_teste$test$date <= ano_fim, ]
    
    if (nrow(treino) == 0 || nrow(teste) == 0) {
      cat("Dados insuficientes para o mandato:", mandato, "\n")
      next
    }
    
    # Garantir que os dados de treino incluem os discursos dos mandatos passados
    treino <- treino_teste$train[treino_teste$train$date <= ano_fim, ]
    
    # Treinar e avaliar o classificador
    temp <- gerar_classificador2(treino, teste, mandato_escolhido = mandato, classificador = classificador)
    cat("Acurácia:", temp, "\n")
    cat("---------------------\n")
  }
}


gerar_classificador2 <- function(discursos_train, discursos_test, should_print = FALSE, classificador) {
  # Obter os níveis das classes
  slevels <- levels(discursos_train[, "Parties"])
  
  # Exibir tabelas de distribuição se necessário
  tbl <- rbind(
    table(discursos_train[, "Parties"]),
    table(discursos_test[, "Parties"])
  )
  rownames(tbl) <- c("training", "test")
  if (should_print) {
    print(tbl)
  }
  
  # Criar e treinar o modelo
  model <- switch(classificador,
                  "knn" = cla_knn("Parties", slevels, k = 5),
                  "decision_tree" = cla_dtree("Parties", slevels),
                  "random_forest" = cla_rf("Parties", slevels, mtry = 3, ntree = 5),
                  "mlp" = cla_mlp("Parties", slevels, size = 5, decay = 0.03),
                  stop("Sem esse modelo")
  )
  
  model <- fit(model, discursos_train)
  
  # Predições no treino
  train_prediction <- predict(model, discursos_train)
  discursos_train_predictand <- adjust_class_label(discursos_train[, "Parties"])
  train_eval <- evaluate(model, discursos_train_predictand, train_prediction)
  if (should_print) {
    print(train_eval$metrics)
  }
  
  # Predições no teste
  discursos_prediction <- predict(model, discursos_test)
  discursos_test_predictand <- adjust_class_label(discursos_test[, "Parties"])
  test_eval <- evaluate(model, discursos_test_predictand, discursos_prediction)
  
  # Calcular acurácia ajustada
  calcular_acuracia_ajustada <- function(predicoes, reais) {
    matriz_confusao <- table(Predito = predicoes, Real = reais)
    precisao_por_classe <- diag(matriz_confusao) / rowSums(matriz_confusao)
    proporcoes_classes <- table(reais) / length(reais)
    sum(precisao_por_classe * proporcoes_classes, na.rm = TRUE)
  }
  
  acuracia_crua <- test_eval$metrics$accuracy
  acuracia_ajustada <- calcular_acuracia_ajustada(discursos_prediction, discursos_test_predictand)
  
  cat("Acurácia Crua:", acuracia_crua, "\n")
  cat("Acurácia Ajustada:", acuracia_ajustada, "\n")
  
  return(acuracia_ajustada)  # Retorna a acurácia ajustada por padrão
}
































batch_classificador_mandato_novo <- function(fun_data, classificador, global, treino_teste) {
  # Lista de mandatos com suas regras
  regras_mandato <- list(
    list(partido = "PSL", inicio = 2019, fim = 2020, mandato = "Bolsonaro"),
    list(partido = "PMDB", inicio = 2016, fim = 2018, mandato = "Temer"),
    list(partido = "PT", inicio = 2015, fim = 2016, mandato = "Dilma2"),
    list(partido = "PT", inicio = 2011, fim = 2014, mandato = "Dilma1"),
    list(partido = "PT", inicio = 2007, fim = 2010, mandato = "Lula2"),
    list(partido = "PT", inicio = 2003, fim = 2006, mandato = "Lula1"),
    list(partido = "PSDB", inicio = 1999, fim = 2002, mandato = "FHC2"),
    list(partido = "PSDB", inicio = 1995, fim = 1998, mandato = "FHC1"),
    list(partido = "PMDB", inicio = 1992, fim = 1994, mandato = "Itamar"),
    list(partido = "PRN", inicio = 1990, fim = 1992, mandato = "Collor"),
    list(partido = "PMDB", inicio = 1985, fim = 1990, mandato = "Sarney")
  )
  
  cat("---------------------\n")
  
  for (regra in regras_mandato) {
    mandato_a <- regra$mandato
    ano_inicio <- regra$inicio
    ano_fim <- regra$fim
    
    cat("Treinando classificador para o mandato:", mandato_a, "\n")
    
    # Separar dados de treino (anos <= ano_fim do mandato atual)
    treino <- treino_teste$train[treino_teste$train$date <= ano_fim, ]
    
    # Separar dados de teste (anos > ano_fim do mandato atual)
    teste <- treino_teste$test[treino_teste$test$date > ano_fim, ]
    
    # Garantir que há dados em treino e teste
    if (nrow(treino) == 0 || nrow(teste) == 0) {
      cat("Sem dados suficientes para treino ou teste para o mandato:", mandato_a, "\n")
      next
    }
    
    # Treinar e avaliar o classificador
    temp <- gerar_classificador_partido_novo_novo(treino, teste, classificador = classificador, mandato_escolhido = mandato_a)
    cat("Acurácia:", temp, "\n")
    cat("---------------------\n")
  }
}



gerar_classificador_partido_novo <- function(discursos_train, discursos_test, should_print = FALSE, classificador,mandato_escolhido) {
  set.seed(42)
  
  slevels <- levels(discursos_train[mandato]==mandato_escolhido)
  
  
  discursos <- cbind(as.matrix(fun_data[, 11:83]), Parties = fun_data[mandato]==mandato_escolhido)
  
  
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
  
  
  model <- fit(model, treino)
  
  train_prediction <- predict(model, treino)
  discursos_train_predictand <- adjust_class_label(treino[,"Parties"])
  train_eval <- evaluate(model, discursos_train_predictand, train_prediction)
  if (should_print) {
    print(train_eval$metrics)
  }
  
  discursos_prediction <- predict(model, teste)
  discursos_test_predictand <- adjust_class_label(teste[,"Parties"])
  test_eval <- evaluate(model, discursos_test_predictand, discursos_prediction)
  if (should_print) {
    print(test_eval$metrics)
  }
  return (test_eval$metrics$accuracy)
}





















gerar_classificador_partido_novo_novo <- function(discursos_train, discursos_test, should_print = FALSE, classificador, mandato_escolhido) {
  set.seed(42)
  
  # Define os níveis como "igual" e "diferente"
  slevels <- c("igual", "diferente")
  
  # Gera os labels com base na coluna 84 (mandato)
  discursos_train_labels <- factor(
    ifelse(discursos_train[, 84] == mandato_escolhido, "igual", "diferente"),
    levels = slevels
  )
  discursos_test_labels <- factor(
    ifelse(discursos_test[, 84] == mandato_escolhido, "igual", "diferente"),
    levels = slevels
  )
  
  # Gera a tabela de frequências para inspeção
  tbl <- rbind(
    table(discursos_train_labels),
    table(discursos_test_labels)
  )
  rownames(tbl) <- c("training", "test")
  if (should_print) {
    print(tbl)
  }
  
  # Escolha do modelo
  model <- switch(classificador,
                  "knn" = cla_knn("Parties", slevels, k = 5),
                  "decision_tree" = cla_dtree("Parties", slevels),
                  "random_forest" = cla_rf("Parties", slevels, mtry = 3, ntree = 5),
                  "mlp" = cla_mlp("Parties", slevels, size = 5, decay = 0.03),
                  stop("Sem esse modelo"))
  
  # Treinamento
  model <- fit(model, discursos_train, discursos_train_labels)
  
  # Avaliação no treinamento
  train_prediction <- predict(model, discursos_train)
  train_eval <- evaluate(model, discursos_train_labels, train_prediction)
  if (should_print) {
    print(train_eval$metrics)
  }
  
  # Avaliação no teste
  test_prediction <- predict(model, discursos_test)
  test_eval <- evaluate(model, discursos_test_labels, test_prediction)
  if (should_print) {
    print(test_eval$metrics)
  }
  
  # Retorna a acurácia final
  return (test_eval$metrics$accuracy)
}







