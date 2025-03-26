source("./library_handler.R") # Auxilia com o load das funções
load_libraries()
carregar_arquivos()

treino <- read.csv("./dados/dados_treino.csv")
teste <- read.csv("./dados/dados_teste.csv")
tudo <- rbind(treino,teste)



treinob <- read.csv("./dados/treino_holdout.csv")
testeb <- read.csv("./dados/validacao_holdout.csv")


remove_columns_by_index <- function(df, start_index, end_index) {
  # Remove columns from start_index to end_index
  df <- df[, -seq(start_index, end_index)]
  return(df)
}


treinob <- remove_columns_by_index(treinob, 1, 11)
testeb <- remove_columns_by_index(testeb, 1, 11)



sr <- sample_random()
sr <- train_test(sr, treino)
treinob <- sr$train
testeb <- sr$test





resultSarney <- batch_classificador(tudo,treinob,testeb,"mandatoSarney")
resultCollor <- batch_classificador(tudo,treinob,testeb,"mandatoCollor")
resultItamar <- batch_classificador(tudo,treinob,testeb,"mandatoItamar")
resultFHC1 <- batch_classificador(tudo,treinob,testeb,"mandatoFHC1")
resultFHC2 <- batch_classificador(tudo,treinob,testeb,"mandatoFHC2")
resultLula1 <- batch_classificador(tudo,treinob,testeb,"mandatoLula1")
resultLula2 <- batch_classificador(tudo,treinob,testeb,"mandatoLula2")
resultDilma1 <- batch_classificador(tudo,treinob,testeb,"mandatoDilma1")
resultDilma2 <- batch_classificador(tudo,treinob,testeb,"mandatoDilma2")
resultTemer <- batch_classificador(tudo,treinob,testeb,"mandatoTemer")
resultBolsonaro <- batch_classificador(tudo,treinob,testeb,"mandatoBolsonaro")




#Pegando os modelos que mais acertam dentro do mandato de cada um
#Lembrando que foi feito com base em TODOS os discursos
cla_sarney <- retorna_modelo(tudo,"mandatoSarney","knn",treino_novo)
cla_sarney <- retorna_modelo(tudo,"mandatoSarney","knn",treino_novo)
cla_sarney <- retorna_modelo(tudo,"mandatoSarney","knn",treino_novo)
teste_final_reduzido <- cbind(as.matrix(teste_final[, 11:83]), Parties = fun_data$mandato)



#report <-fim_do_experimento(tudo,treino,teste,0.1)
report <-fim_do_experimento(tudo,treino,teste,0.00)

report <-fim_do_experimento(tudo,treino,teste,0.05)
report <-fim_do_experimento(tudo,treino,teste,0.04)
report <-fim_do_experimento(tudo,treino,teste,0.03)
report <-fim_do_experimento(tudo,treino,teste,0.02)
report <-fim_do_experimento(tudo,treino,teste,0.01)
report <-fim_do_experimento(tudo,treino,teste,0.00)


report <-fim_do_experimento(tudo,treino,teste,0.01)
calcular_porcentagem_acerto(report)


report <-fim_do_experimento(tudo,treino,teste,0.001)
report <-fim_do_experimento(tudo,treino,teste,0.0001)
report <-fim_do_experimento(tudo,treino,teste,0.00001)
report <-fim_do_experimento(tudo,treino,teste,0.000001)





report0 <-fim_do_experimento2(tudo,treino,teste,0)
report001 <-fim_do_experimento2(tudo,treino,teste,0.0001)

report002 <-fim_do_experimento2(tudo,treino,teste,0.00009)

report003 <-fim_do_experimento2(tudo,treino,teste,0.00008)

report007 <-fim_do_experimento2(tudo,treino,teste,0.00007)
report006 <-fim_do_experimento2(tudo,treino,teste,0.00006)
report005 <-fim_do_experimento2(tudo,treino,teste,0.00005)
report004 <-fim_do_experimento2(tudo,treino,teste,0.00004)
report003 <-fim_do_experimento2(tudo,treino,teste,0.00003)


report005 <-fim_do_experimento2(tudo,treino,teste,0.00002)
report001 <-fim_do_experimento2(tudo,treino,teste,0.00001)
write.csv(resultado,"cruzamento.csv")




calcula_acertos_por_mandato <- function(dataframe) {
  # Agrupa os dados por 'Classe_Real' e calcula métricas
  resultado <- dataframe %>%
    group_by(Classe_Real) %>%
    summarize(
      total_discursos = n(),                      # Conta o número total de discursos
      total_acertos = sum(Acerto, na.rm = TRUE),  # Soma os valores de acerto (TRUE = 1)
      perc_acerto = mean(Acerto, na.rm = TRUE) * 100 # Calcula a % de acerto
    )
  
  return(resultado)
}


finallll <- calcula_acertos_por_mandato(report005)