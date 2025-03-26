separar_dados_holdout <- function(train_data){
  set.seed(5)
  sr <- sample_random()
  sr <- train_test(sr, train_data)
  treino_holdout <- sr$train
  validacao_holdout <- sr$test
  
  write.csv(treino_holdout, "treino_holdout.csv", row.names = TRUE)
  write.csv(validacao_holdout, "validicao_holdout.csv", row.names = TRUE)
  
}

separar_dados_holdout(teste)