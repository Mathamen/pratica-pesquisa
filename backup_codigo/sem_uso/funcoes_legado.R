gerar_classificador_tunado <- function(fun_data,partido_coluna, classificador,discursos_train,discursos_test,  should_print = FALSE){
  # set.seed(42)
  # Embaralhar os dados
  #fun_data <- fun_data[sample(nrow(fun_data)), ]
  
  fun_data[[partido_coluna]] <- factor(fun_data[[partido_coluna]])
  #fun_data <- fun_data[sample(nrow(fun_data)), ]
  slevels <- levels(fun_data[[partido_coluna]])
  #print(slevels)
  #return (slevels)
  
  
  discursos <- cbind(as.matrix(fun_data[, 11:83]), Parties = fun_data[[partido_coluna]])
  discursos_train <- cbind(as.matrix(discursos_train[, 11:83]), Parties = fun_data[[partido_coluna]])
  discursos_test <- cbind(as.matrix(discursos_test[, 11:83]), Parties = fun_data[[partido_coluna]])
  sr <- sample_random()
  sr <- train_test(sr, discursos_train)
  discursos_train <- sr$test
  
  sr <- sample_random()
  sr <- train_test(sr, discursos_train)
  discursos_train <- sr$test
  
  cat(length(discursos_train))
  
  
  #return(discursos_train)
  set.seed(5)
  #sr <- sample_random()
  #sr <- train_test(sr, discursos)
  #discursos_train <- sr$train
  #discursos_test <- sr$test
  
  #tbl <- rbind(
  #  table(discursos[,"Parties"]),
  # table(discursos_train[,"Parties"]),
  # table(discursos_test[,"Parties"])
  #)
  # rownames(tbl) <- c("dataset", "training", "test")
  # if (should_print) {
  #  print(tbl)
  # }
  
  
  # Sempre que quiser colocar mais um modelo, só alterar aqui!
  
  #svm
  #tune <- cla_tune(cla_svm("Parties", slevels))
  ranges <- list(epsilon=seq(0,1,0.1), cost=seq(10,100,10), kernel = c("polynomial"))
  
  # knn
  #tune <- cla_tune(cla_knn("Parties", slevels))
  #ranges <- list(k=1:20)
  
  
  #mlp
  # tune <- cla_tune(cla_mlp("Parties", slevels))
  ranges <- list(size=3:12, decay=seq(0, 1, 0.1))
  
  #rf
  tune <- cla_tune(cla_rf("Parties", slevels))
  ranges <- list(mtry=1:10, ntree=seq(10,100,10))
  
  
  
  
  
  
  
  
  
  model <- fit(tune, discursos_train, ranges)
  
  
  
  train_prediction <- predict(model, discursos_train)
  discursos_train_predictand <- adjust_class_label(discursos_train[,"Parties"])
  train_eval <- evaluate(model, discursos_train_predictand, train_prediction)
  
  
  discursos_prediction <- predict(model, discursos_test)
  discursos_test_predictand <- adjust_class_label(discursos_test[,"Parties"])
  test_eval <- evaluate(model, discursos_test_predictand, discursos_prediction)
  
  return (model)
}

modelito <- gerar_classificador_tunado(tudo,"mandatoBolsonaro","a",treino,teste)

















