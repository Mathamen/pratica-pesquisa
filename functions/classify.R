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




































# Print mais bonito

batch_mlp <- function(fun_data){
  somaPMDB <- as.numeric(sum(fun_data$PMDB == 1))
  somaPRN <- as.numeric(sum(fun_data$PRN == 1))
  somaPSL <- as.numeric(sum(fun_data$PSL == 1))
  somaPSDB <- as.numeric(sum(fun_data$PSDB == 1))
  somaPT <- as.numeric(sum(fun_data$PT == 1))
  somaDiscursos <- 5888 # esse daí eu sei kkkkkkk
  
  cat("---------------------\n")
  cat("PMDB\n")
  temp <- gerar_mlp(fun_data, "PMDB", FALSE)
  cat(temp,"\n")
  cat("Resultado:", temp - (somaPMDB / somaDiscursos), "\n")
  
  cat("---------------------\n")
  cat("PRN\n")
  temp <- gerar_mlp(fun_data, "PRN", FALSE)
  cat(temp,"\n")
  cat("Resultado:", temp - (somaPRN / somaDiscursos), "\n")
  
  cat("---------------------\n")
  cat("PSL\n")
  temp <- gerar_mlp(fun_data, "PSL", FALSE)
  cat(temp,"\n")
  cat("Resultado:", temp - (somaPSL / somaDiscursos), "\n")
  
  cat("---------------------\n")
  cat("PSDB\n")
  temp <- gerar_mlp(fun_data, "PSDB", FALSE)
  cat(temp,"\n")
  cat("Resultado:", temp - (somaPSDB / somaDiscursos), "\n")
  
  cat("---------------------\n")
  cat("PT\n")
  temp <- gerar_mlp(fun_data, "PT", FALSE)
  cat(temp,"\n")
  cat("Resultado:", temp - (somaPT / somaDiscursos), "\n")
}