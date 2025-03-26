# Teste de Classificador

model <- gerar_classificador_mandato(tudo,"mandatoLula1",classificador="mlp",treino,teste)
print(result2)
#final <- result2 - percent
#print(final)

result2 <- gerar_classificador_mandato(tudo,"mandatoSarney",classificador="knn",treino,teste)
print(result2)
#final <- result2 - percent
#print(final)


result2 <- gerar_classificador_mandato(tudo,"mandatoLula1",classificador="mlp",treino,teste)
print(result2)
final <- result2 - percent
print(final)

result2 <- gerar_classificador_mandato(tudo,"mandatoLula1",classificador="decision_tree",treino,teste)
print(result2)
final <- result2 - percent
print(final)









eval_global <- gerar_classificador_mandato_global(tudo,classificador="majority",treino,teste)
print(eval_global$metrics)
































#-----------------------------------------------------------------------------
#Segunda parte aqui. Focado na parte de fazer a divisão treino validação teste
#Além disso, esta parte aqui não reduz a visibilidade do treino até o mandato

sr <- sample_random()
sr<- train_test(sr,tudo,perc =0.7)
treino_novo <- sr$train
temp <- sr$test
sr <- sample_random()
sr <- train_test(sr,tudo,perc = 0.5)
validacao<- sr$train
teste_final <- sr$test
rm(sr)
rm(temp)

resultSarney <- batch_classificador(tudo,treino_novo,validacao,"mandatoSarney")
resultCollor <- batch_classificador(tudo,treino_novo,validacao,"mandatoCollor")
resultItamar <- batch_classificador(tudo,treino_novo,validacao,"mandatoItamar")
resultFHC1 <- batch_classificador(tudo,treino_novo,validacao,"mandatoFHC1")
resultFHC2 <- batch_classificador(tudo,treino_novo,validacao,"mandatoFHC2")
resultLula1 <- batch_classificador(tudo,treino_novo,validacao,"mandatoLula1")
resultLula2 <- batch_classificador(tudo,treino_novo,validacao,"mandatoLula2")
resultDilma1 <- batch_classificador(tudo,treino_novo,validacao,"mandatoDilma1")
resultDilma2 <- batch_classificador(tudo,treino_novo,validacao,"mandatoDilma2")
resultTemer <- batch_classificador(tudo,treino_novo,validacao,"mandatoTemer")
resultBolsonaro <- batch_classificador(tudo,treino_novo,validacao,"mandatoBolsonaro")




#Pegando os modelos que mais acertam dentro do mandato de cada um
#Lembrando que foi feito com base em TODOS os discursos
cla_sarney <- retorna_modelo(tudo,"mandatoSarney","knn",treino_novo)
cla_sarney <- retorna_modelo(tudo,"mandatoSarney","knn",treino_novo)
cla_sarney <- retorna_modelo(tudo,"mandatoSarney","knn",treino_novo)
teste_final_reduzido <- cbind(as.matrix(teste_final[, 11:83]), Parties = fun_data$mandato)