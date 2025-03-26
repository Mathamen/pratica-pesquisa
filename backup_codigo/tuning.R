


#Função para a criação de um modelo com os hiper parâmetros tunados
#Retorna o modelo. Pegue o modelo e leia quais são os hiperparâmetros
tune_classficador <- function(fun_data, classificador,discursos_train,discursos_test, partido_coluna){
  set.seed(42)
 
  fun_data[[partido_coluna]] <- factor(fun_data[[partido_coluna]])
  slevels <- levels(fun_data[[partido_coluna]])
  
  
  discursos <- cbind(as.matrix(fun_data[, 12:84]), Parties = fun_data[[partido_coluna]])
  discursos_train <- cbind(as.matrix(discursos_train[, 12:84]), Parties = fun_data[[partido_coluna]])
  discursos_test <- cbind(as.matrix(discursos_test[, 12:84]), Parties = fun_data[[partido_coluna]])
  

  set.seed(5)
  
  
  #reduzindo dataset para 20% (sr divide 80/20)
  sr <- sample_random()
  sr <- train_test(sr, discursos_train)
  discursos_train <- sr$train
  
  
  if (classificador == "knn"){
    tune<- cla_tune(cla_knn("Parties", slevels))
    ranges <- list(k=1:20)
  }
  
  if (classificador == "mlp"){
    tune<- cla_tune(cla_mlp("Parties", slevels))
    ranges <- list(size=3:12, decay=seq(0, 1, 0.1))
  }
  
  if (classificador == "random_forest"){
    tune<- cla_tune(cla_rf("Parties", slevels))
    ranges <- list(mtry=1:10, ntree=seq(10,100,10))
  }
  
  if (classificador == "svm"){
    tune <- cla_tune(cla_svm("Parties", slevels))
    ranges <- list(epsilon=seq(0,1,0.1), cost=seq(10,100,10), kernel = c("polynomial"))
  
  }

  
  
  model <- fit(tune, discursos_train, ranges)
  
  return (model)
}

treino_holdout <- read.csv("dados/treino_holdout.csv")
validacao_holdout <- read.csv("dados/validacao_holdout.csv")

#SARNEY
print(Sys.time())
cla_sarney_knn <- tune_classficador(tudo,"knn",treino_holdout,validacao_holdout,"mandatoSarney")
cla_sarney_mlp <- tune_classficador(tudo,"mlp",treino_holdout,validacao_holdout,"mandatoSarney")
cla_sarney_rf <- tune_classficador(tudo,"random_forest",treino_holdout,validacao_holdout,"mandatoSarney")
cla_sarney_svm <- tune_classficador(tudo,"svm",treino_holdout,validacao_holdout,"mandatoSarney")
print(Sys.time())

#COLLOR
print(Sys.time())
cla_collor_knn <- tune_classficador(tudo,"knn",treino_holdout,validacao_holdout,"mandatoCollor")
cla_collor_mlp <- tune_classficador(tudo,"mlp",treino_holdout,validacao_holdout,"mandatoCollor")
cla_collor_rf <- tune_classficador(tudo,"random_forest",treino_holdout,validacao_holdout,"mandatoCollor")
cla_collor_svm <- tune_classficador(tudo,"svm",treino_holdout,validacao_holdout,"mandatoCollor")
print(Sys.time())

#ITAMAR
print(Sys.time())
cla_itamar_knn <- tune_classficador(tudo,"knn",treino_holdout,validacao_holdout,"mandatoItamar")
cla_itamar_mlp <- tune_classficador(tudo,"mlp",treino_holdout,validacao_holdout,"mandatoItamar")
cla_itamar_rf <- tune_classficador(tudo,"random_forest",treino_holdout,validacao_holdout,"mandatoItamar")
cla_itamar_svm <- tune_classficador(tudo,"svm",treino_holdout,validacao_holdout,"mandatoItamar")
print(Sys.time())

#FHC1
print(Sys.time())
cla_fhc1_knn <- tune_classficador(tudo,"knn",treino_holdout,validacao_holdout,"mandatoFHC1")
cla_fhc1_mlp <- tune_classficador(tudo,"mlp",treino_holdout,validacao_holdout,"mandatoFHC1")
cla_fhc1_rf <- tune_classficador(tudo,"random_forest",treino_holdout,validacao_holdout,"mandatoFHC1")
cla_fhc1_svm <- tune_classficador(tudo,"svm",treino_holdout,validacao_holdout,"mandatoFHC1")
print(Sys.time())

#FHC2
print(Sys.time())
cla_fhc2_knn <- tune_classficador(tudo,"knn",treino_holdout,validacao_holdout,"mandatoFHC2")
cla_fhc2_mlp <- tune_classficador(tudo,"mlp",treino_holdout,validacao_holdout,"mandatoFHC2")
cla_fhc2_rf <- tune_classficador(tudo,"random_forest",treino_holdout,validacao_holdout,"mandatoFHC2")
cla_fhc2_svm <- tune_classficador(tudo,"svm",treino_holdout,validacao_holdout,"mandatoFHC2")
print(Sys.time())

#LULA1
print(Sys.time())
cla_lula1_knn <- tune_classficador(tudo,"knn",treino_holdout,validacao_holdout,"mandatoLula1")
cla_lula1_mlp <- tune_classficador(tudo,"mlp",treino_holdout,validacao_holdout,"mandatoLula1")
cla_lula1_rf <- tune_classficador(tudo,"random_forest",treino_holdout,validacao_holdout,"mandatoLula1")
cla_lula1_svm <- tune_classficador(tudo,"svm",treino_holdout,validacao_holdout,"mandatoLula1")
print(Sys.time())

#LULA2
print(Sys.time())
cla_lula2_knn <- tune_classficador(tudo,"knn",treino_holdout,validacao_holdout,"mandatoLula2")
cla_lula2_mlp <- tune_classficador(tudo,"mlp",treino_holdout,validacao_holdout,"mandatoLula2")
cla_lula2_rf <- tune_classficador(tudo,"random_forest",treino_holdout,validacao_holdout,"mandatoLula2")
cla_lula2_svm <- tune_classficador(tudo,"svm",treino_holdout,validacao_holdout,"mandatoLula2")
print(Sys.time())

#DILMA1
print(Sys.time())
cla_dilma1_knn <- tune_classficador(tudo,"knn",treino_holdout,validacao_holdout,"mandatoDilma1")
cla_dilma1_mlp <- tune_classficador(tudo,"mlp",treino_holdout,validacao_holdout,"mandatoDilma1")
cla_dilma1_rf <- tune_classficador(tudo,"random_forest",treino_holdout,validacao_holdout,"mandatoDilma1")
cla_dilma1_svm <- tune_classficador(tudo,"svm",treino_holdout,validacao_holdout,"mandatoDilma1")
print(Sys.time())

#DILMA2
print(Sys.time())
cla_dilma2_knn <- tune_classficador(tudo,"knn",treino_holdout,validacao_holdout,"mandatoDilma2")
cla_dilma2_mlp <- tune_classficador(tudo,"mlp",treino_holdout,validacao_holdout,"mandatoDilma2")
cla_dilma2_rf <- tune_classficador(tudo,"random_forest",treino_holdout,validacao_holdout,"mandatoDilma2")
cla_dilma2_svm <- tune_classficador(tudo,"svm",treino_holdout,validacao_holdout,"mandatoDilma2")
print(Sys.time())

#TEMER
print(Sys.time())
cla_temer_knn <- tune_classficador(tudo,"knn",treino_holdout,validacao_holdout,"mandatoTemer")
cla_temer_mlp <- tune_classficador(tudo,"mlp",treino_holdout,validacao_holdout,"mandatoTemer")
cla_temer_rf <- tune_classficador(tudo,"random_forest",treino_holdout,validacao_holdout,"mandatoTemer")
cla_temer_svm <- tune_classficador(tudo,"svm",treino_holdout,validacao_holdout,"mandatoTemer")
print(Sys.time())

#BOLSONARO
print(Sys.time())
cla_bolsonaro_knn <- tune_classficador(tudo,"knn",treino_holdout,validacao_holdout,"mandatoBolsonaro")
cla_bolsonaro_mlp <- tune_classficador(tudo,"mlp",treino_holdout,validacao_holdout,"mandatoBolsonaro")
cla_bolsonaro_rf <- tune_classficador(tudo,"random_forest",treino_holdout,validacao_holdout,"mandatoBolsonaro")
cla_bolsonaro_svm <- tune_classficador(tudo,"svm",treino_holdout,validacao_holdout,"mandatoBolsonaro")
print(Sys.time())



