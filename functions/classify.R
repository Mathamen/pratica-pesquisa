gerar_classificador_mandato <- function(
    fun_data,partido_coluna, classificador,discursos_train,discursos_test,  should_print = FALSE,
    ntree_mandato =10,
    decay_mandato = 0.7,
    epsilon_mandato = 1,
    cost_mandato = 80.000
    
){
  #set.seed(42)
  # Embaralhar os dados
  #fun_data <- fun_data[sample(nrow(fun_data)), ]
  
  fun_data[[partido_coluna]] <- factor(fun_data[[partido_coluna]], levels = rev(unique(fun_data[[partido_coluna]])))
  #fun_data <- fun_data[sample(nrow(fun_data)), ]
  slevels <- levels(fun_data[[partido_coluna]])
  print(slevels)
  #return (slevels)
  
  
  discursos <- cbind(as.matrix(fun_data[, 11:83]), Parties = fun_data[[partido_coluna]])
  discursos_train <- cbind(as.matrix(discursos_train[, 11:83]), Parties = fun_data[[partido_coluna]])
  print(colnames(discursos_train))
  discursos_test <- cbind(as.matrix(discursos_test[, 11:83]), Parties = fun_data[[partido_coluna]])
  #print(colnames(discursos_test))
  # print(colnames(discursos_train))
  
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
  model <- switch(classificador,
                  "knn" = cla_knn("Parties", slevels, k=1),
                  "decision_tree" = cla_dtree("Parties", slevels),                  
                  "random_forest" = cla_rf("Parties", slevels, mtry=9, ntree=ntree_mandato), 
                  "mlp" = cla_mlp("Parties", slevels, size = 9, decay = decay_mandato),
                  "majority" = cla_majority("Parties", slevels),                                       
                  "nb" = cla_nb("Parties", slevels),                                
                  "svm" = cla_svm("Parties", slevels, epsilon=epsilon_mandato , cost=cost_mandato, kernel= "polynomial"),
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
  return (test_eval$metrics)
}






batch_classificador <- function(data_toda,fun_treino,fun_teste,mandato_escolhido_funcao){
  header <- c("Classifier","Precision", "Recall", "Sensitivity", "Specificity", "F1", "Model Accuracy", "TP","FP","TN","FN")
  
  cat("---------------------\n")
  
  # Inicializa um data.frame vazio com as colunas especificadas no header
  retorno <- data.frame(matrix(ncol = length(header), nrow = 0))
  colnames(retorno) <- header
  
  temp_ntree <- switch(
    mandato_escolhido_funcao,
    "mandatoSarney" =80,
    "mandatoCollor" = 10,
    "mandatoItamar" = 10,
    "mandatoFHC1" = 40,
    "mandatoFHC2" = 100,
    "mandatoLula1" =60,
    "mandatoLula2" = 60,
    "mandatoDilma1" = 70,
    "mandatoDilma2" = 10,
    "mandatoTemer" = 40,
    "mandatoBolsonaro" =20
  )
  temp_decay <- switch(
    mandato_escolhido_funcao,
    "mandatoSarney" =0.8,
    "mandatoCollor" = 0,
    "mandatoItamar" = 0,
    "mandatoFHC1" = 0,
    "mandatoFHC2" = 0,
    "mandatoLula1" =0,
    "mandatoLula2" = 0,
    "mandatoDilma1" = 0,
    "mandatoDilma2" = 0,
    "mandatoTemer" = 0.6,
    "mandatoBolsonaro" =0.5
  )
  
  temp_epsilon <- switch(
    mandato_escolhido_funcao,
    "mandatoSarney" =0.1,
    "mandatoCollor" = 0,
    "mandatoItamar" = 0,
    "mandatoFHC1" = 0,
    "mandatoFHC2" = 0.2,
    "mandatoLula1" =1,
    "mandatoLula2" = 1,
    "mandatoDilma1" = 0.4,
    "mandatoDilma2" = 0,
    "mandatoTemer" = 0.9,
    "mandatoBolsonaro" =0
  )
  
  temp_cost <- switch(
    mandato_escolhido_funcao,
    "mandatoSarney" =10,
    "mandatoCollor" = 10,
    "mandatoItamar" = 10,
    "mandatoFHC1" = 40,
    "mandatoFHC2" = 10,
    "mandatoLula1" =20,
    "mandatoLula2" = 20,
    "mandatoDilma1" = 10,
    "mandatoDilma2" = 10,
    "mandatoTemer" = 10,
    "mandatoBolsonaro" =10
  )
  
  
  
  
  # Decision Tree
  resultado <- gerar_classificador_mandato(tudo, mandato_escolhido_funcao, classificador = "decision_tree", fun_treino, teste)
  results <- data.frame(
    Classifier = "dtree",
    Precision = resultado$precision,
    Recall = resultado$recall,
    Sensitivity = resultado$sensitivity,
    Specificity = resultado$specificity,
    F1 = resultado$f1,
    `Model Accuracy` = resultado$accuracy,
    TP = resultado$TP,
    FP = resultado$FP,
    TN = resultado$TN,
    FN = resultado$FN
  )
  
  retorno <- rbind(retorno, results)
  
  
  # KNN
  resultado <- gerar_classificador_mandato(tudo, mandato_escolhido_funcao, classificador = "knn", fun_treino, teste)
  results <- data.frame(
    Classifier = "knn",
    Precision = resultado$precision,
    Recall = resultado$recall,
    Sensitivity = resultado$sensitivity,
    Specificity = resultado$specificity,
    F1 = resultado$f1,
    `Model Accuracy` = resultado$accuracy,
    TP = resultado$TP,
    FP = resultado$FP,
    TN = resultado$TN,
    FN = resultado$FN
  )
  
  retorno <- rbind(retorno, results)
  
  
  # Majority
  resultado <- gerar_classificador_mandato(tudo, mandato_escolhido_funcao, classificador = "majority", fun_treino, teste)
  results <- data.frame(
    Classifier = "majority",
    Precision = resultado$precision,
    Recall = resultado$recall,
    Sensitivity = resultado$sensitivity,
    Specificity = resultado$specificity,
    F1 = resultado$f1,
    `Model Accuracy` = resultado$accuracy,
    TP = resultado$TP,
    FP = resultado$FP,
    TN = resultado$TN,
    FN = resultado$FN
  )
  
  retorno <- rbind(retorno, results)
  
  
  # MLP
  resultado <- gerar_classificador_mandato(tudo, mandato_escolhido_funcao, classificador = "mlp", fun_treino, teste, decay_mandato = temp_decay)
  results <- data.frame(
    Classifier = "mlp",
    Precision = resultado$precision,
    Recall = resultado$recall,
    Sensitivity = resultado$sensitivity,
    Specificity = resultado$specificity,
    F1 = resultado$f1,
    `Model Accuracy` = resultado$accuracy,
    TP = resultado$TP,
    FP = resultado$FP,
    TN = resultado$TN,
    FN = resultado$FN
  )
  
  retorno <- rbind(retorno, results)
  
  
  
  # Naive Bayes
  resultado <- gerar_classificador_mandato(tudo, mandato_escolhido_funcao, classificador = "nb", fun_treino, teste)
  results <- data.frame(
    Classifier = "nb",
    Precision = resultado$precision,
    Recall = resultado$recall,
    Sensitivity = resultado$sensitivity,
    Specificity = resultado$specificity,
    F1 = resultado$f1,
    `Model Accuracy` = resultado$accuracy,
    TP = resultado$TP,
    FP = resultado$FP,
    TN = resultado$TN,
    FN = resultado$FN
  )
  
  retorno <- rbind(retorno, results)
  
  
  
  # Random Forest
  resultado <- gerar_classificador_mandato(tudo, mandato_escolhido_funcao, classificador = "random_forest", fun_treino, teste, ntree_mandato = temp_ntree)
  results <- data.frame(
    Classifier = "rf",
    Precision = resultado$precision,
    Recall = resultado$recall,
    Sensitivity = resultado$sensitivity,
    Specificity = resultado$specificity,
    F1 = resultado$f1,
    `Model Accuracy` = resultado$accuracy,
    TP = resultado$TP,
    FP = resultado$FP,
    TN = resultado$TN,
    FN = resultado$FN
  )
  
  retorno <- rbind(retorno, results)
  
  
  
  
  # SVM
  resultado <- gerar_classificador_mandato(tudo, mandato_escolhido_funcao, classificador = "svm", fun_treino, teste,epsilon_mandato = temp_epsilon, cost_mandato = temp_cost)
  results <- data.frame(
    Classifier = "svm",
    Precision = resultado$precision,
    Recall = resultado$recall,
    Sensitivity = resultado$sensitivity,
    Specificity = resultado$specificity,
    F1 = resultado$f1,
    `Model Accuracy` = resultado$accuracy,
    TP = resultado$TP,
    FP = resultado$FP,
    TN = resultado$TN,
    FN = resultado$FN
  )
  
  retorno <- rbind(retorno, results)
  
  
  
  
  
  # Retorna o data.frame com os resultados
  return(retorno)
}




gerar_classificador_mandato_global <- function(fun_data, classificador,discursos_train,discursos_test,  should_print = FALSE){
  set.seed(42)
  
  fun_data$mandato <- factor(fun_data$mandato)
  slevels <- levels(fun_data$mandato)
  print(slevels)
  
  
  discursos <- cbind(as.matrix(fun_data[, 11:83]), Parties = fun_data$mandato)
  discursos_train <- cbind(as.matrix(discursos_train[, 11:83]), Parties = fun_data$mandato)
  discursos_test <- cbind(as.matrix(discursos_test[, 11:83]), Parties = fun_data$mandato)
  
  set.seed(5)
  
  
  # Sempre que quiser colocar mais um modelo, só alterar aqui!
  model <- switch(classificador,
                  "knn" = cla_knn("Parties", slevels, k=1),
                  "decision_tree" = cla_dtree("Parties", slevels),                  #TUNED
                  "random_forest" = cla_rf("Parties", slevels, mtry=9, ntree=4),
                  "mlp" = cla_mlp("Parties", slevels, size = 9, decay = 0.00),
                  "majority" = cla_majority("Parties", slevels),                    #TUNED                    
                  "nb" = cla_nb("Parties", slevels),                                #TUNED
                  "svm" = cla_svm("Parties", slevels, epsilon=0.0,cost=20.000),
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
  return (test_eval)
}








retorna_modelo <- function(fun_data,partido_coluna, classificador,discursos_train,
                           ntree_mandato =10,
                           decay_mandato = 0.7,
                           epsilon_mandato = 1,
                           cost_mandato = 80.000
){
  fun_data[[partido_coluna]] <- factor(fun_data[[partido_coluna]], levels = rev(unique(fun_data[[partido_coluna]])))
  slevels <- levels(fun_data[[partido_coluna]])
  print(slevels)
  
  
  discursos <- cbind(as.matrix(fun_data[, 11:83]), Parties = fun_data[[partido_coluna]])
  discursos_train <- cbind(as.matrix(discursos_train[, 11:83]), Parties = fun_data[[partido_coluna]])
  
  set.seed(5)
  
  model <- switch(classificador,
                  "knn" = cla_knn("Parties", slevels, k=1),
                  "decision_tree" = cla_dtree("Parties", slevels),                  #TUNED
                  "random_forest" = cla_rf("Parties", slevels, mtry=9, ntree=ntree_mandato),
                  "mlp" = cla_mlp("Parties", slevels, size = 9, decay = decay_mandato),
                  "majority" = cla_majority("Parties", slevels),                    #TUNED                    
                  "nb" = cla_nb("Parties", slevels),                                #TUNED
                  "svm" = cla_svm("Parties", slevels, epsilon=epsilon_mandato,cost=cost_mandato, kernel= "polynomial"),
                  stop("Sem esse modelo"))
  
  
  model <- fit(model, discursos_train)
  return(model)
}



retorna_modelo <- function(fun_data,partido_coluna, classificador,discursos_train,
                           ntree_mandato =10,
                           decay_mandato = 0.7,
                           epsilon_mandato = 1,
                           cost_mandato = 80.000
){
  fun_data[[partido_coluna]] <- factor(fun_data[[partido_coluna]], levels = rev(unique(fun_data[[partido_coluna]])))
  slevels <- levels(fun_data[[partido_coluna]])
  print(slevels)
  
  
  discursos <- cbind(as.matrix(fun_data[, 11:83]), Parties = fun_data[[partido_coluna]])
  discursos_train <- cbind(as.matrix(discursos_train[, 11:83]), Parties = fun_data[[partido_coluna]])
  
  set.seed(5)
  
  model <- switch(classificador,
                  "knn" = cla_knn("Parties", slevels, k=1),
                  "decision_tree" = cla_dtree("Parties", slevels),                  #TUNED
                  "random_forest" = cla_rf("Parties", slevels, mtry=9, ntree=ntree_mandato),
                  "mlp" = cla_mlp("Parties", slevels, size = 9, decay = decay_mandato),
                  "majority" = cla_majority("Parties", slevels),                    #TUNED                    
                  "nb" = cla_nb("Parties", slevels),                                #TUNED
                  "svm" = cla_svm("Parties", slevels, epsilon=epsilon_mandato,cost=cost_mandato, kernel= "polynomial"),
                  stop("Sem esse modelo"))
  
  
  model <- fit(model, discursos_train)
  return(model)
}





retorna_nb <- function(fun_data,partido_coluna,discursos_train){
  fun_data[[partido_coluna]] <- factor(fun_data[[partido_coluna]], levels = rev(unique(fun_data[[partido_coluna]])))
  slevels <- levels(fun_data[[partido_coluna]])
  #print(slevels)
  
  
  discursos <- cbind(as.matrix(fun_data[, 11:83]), Parties = fun_data[[partido_coluna]])
  discursos_train <- cbind(as.matrix(discursos_train[, 11:83]), Parties = fun_data[[partido_coluna]])
  
  set.seed(5)
  
  model <- cla_nb("Parties", slevels)
  
  
  
  model <- fit(model, discursos_train)
  return(model)
}







