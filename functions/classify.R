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