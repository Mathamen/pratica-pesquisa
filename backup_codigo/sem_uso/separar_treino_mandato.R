treinos <- retornar_mandatos_reduzidos(treino)

# Precisa rodar isso para consertar os nomes das colunas se estiverem objeto. tal coisa
#treino<- renomear_colunas(treino,teste)

teste_a_usar <- treinos[1]
teste_df <- as.data.frame(teste_a_usar)




treino_sarney <- filtrar_por_1(treino,"mandatoSarney")


treino_collor <- filtrar_por_1(treino,"mandatoCollor")
treino_collor <- rbind(treino_sarney,treino_collor)


treino_itamar <- filtrar_por_1(treino,"mandatoItamar")
treino_itamar <- rbind(treino_collor,treino_itamar)


treino_fhc1 <- filtrar_por_1(treino,"mandatoFHC1")
treino_fhc1 <- rbind(treino_itamar,treino_fhc1)


treino_fhc2 <- filtrar_por_1(treino,"mandatoFHC2")
treino_fhc2 <- rbind(treino_fhc1,treino_fhc2)


treino_lula1 <- filtrar_por_1(treino,"mandatoLula1")
treino_lula1 <- rbind(treino_fhc2,treino_lula1)


treino_lula2 <- filtrar_por_1(treino,"mandatoLula2")
treino_lula2 <- rbind(treino_lula1,treino_lula2)


treino_dilma1 <- filtrar_por_1(treino,"mandatoDilma1")
treino_dilma1 <- rbind(treino_lula2,treino_dilma1)


treino_dilma2 <- filtrar_por_1(treino,"mandatoDilma2")
treino_dilma2 <- rbind(treino_dilma1,treino_dilma2)


treino_temer <- filtrar_por_1(treino,"mandatoTemer")
treino_temer <- rbind(treino_dilma2,treino_temer)



treino_bolsonaro <- filtrar_por_1(treino,"mandatoBolsonaro")
treino_bolsonaro <- rbind(treino_temer,treino_bolsonaro)











resultSarney <- batch_classificador(tudo,treino_sarney,teste,"mandatoSarney")
resultCollor <- batch_classificador(tudo,treino_collor,teste,"mandatoCollor")

resultItamar <- batch_classificador(tudo,treino_itamar,teste,"mandatoItamar")
resultFHC1 <- batch_classificador(tudo,treino_fhc1,teste,"mandatoFHC1")

resultFHC2 <- batch_classificador(tudo,treino_fhc2,teste,"mandatoFHC2")
resultLula1 <- batch_classificador(tudo,treino_lula1,teste,"mandatoLula1")

resultLula2 <- batch_classificador(tudo,treino_lula2,teste,"mandatoLula2")
resultDilma1 <- batch_classificador(tudo,treino_dilma1,teste,"mandatoDilma1")

resultDilma2 <- batch_classificador(tudo,treino_dilma2,teste,"mandatoDilma2")
resultTemer <- batch_classificador(tudo,treino_temer,teste,"mandatoTemer")

resultBolsonaro <- batch_classificador(tudo,treino_bolsonaro,teste,"mandatoBolsonaro")







