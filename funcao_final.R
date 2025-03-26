fim_do_experimento <- function(completo, training_data, test_data, epsilon) {
  # Lista de classificadores e mandatos
  mandatos <- c("Sarney", "Collor", "Itamar", "FHC1", "FHC2", 
                "Lula1", "Lula2", "Dilma1", "Dilma2", "Temer", "Bolsonaro")
  classificadores <- lapply(mandatos, function(m) retorna_nb(completo, paste0("mandato", m), training_data))
  names(classificadores) <- mandatos
  
  # Variáveis de contagem
  acertos <- 0
  casos_empate <- 0
  
  # Lista para armazenar o relatório
  report <- data.frame(Discurso = integer(0), 
                       Classe_Real = character(0), 
                       Predicao = character(0), 
                       Acerto = logical(0))
  
  # Loop sobre os discursos de teste
  for (i in 1:nrow(test_data)) {
    linha <- test_data[i, ]
    mandato_real <- linha$mandato
    
    # Armazena as certezas preditas para cada mandato
    certezas <- numeric(length(mandatos))
    names(certezas) <- mandatos
    
    # Verifica os classificadores
    for (mandato in mandatos) {
      partido_coluna <- paste0("mandato", mandato)
      test_temp <- cbind(as.matrix(linha[, 11:83]), Parties = completo[[partido_coluna]])
      test_prediction <- predict(classificadores[[mandato]], test_temp)
      certeza_atual <- test_prediction[1, 1]
      
      if (partido_coluna == "mandatoCollor") {
        certeza_atual <- test_prediction[1, 2]
      }
      
      certezas[mandato] <- certeza_atual
    }
    
    # Encontra o mandato com maior certeza
    maior_certeza <- max(certezas)
    mandatos_candidatos <- names(certezas[certezas >= maior_certeza - epsilon & certezas <= maior_certeza + epsilon])
    
    # Verifica se houve empate
    empate <- length(mandatos_candidatos) > 1
    
    # Verifica se acertou
    acertou <- mandato_real %in% mandatos_candidatos
    predicao <- ifelse(length(mandatos_candidatos) == 1, mandatos_candidatos[1], paste(mandatos_candidatos, collapse = ", "))
    
    # Incrementa o contador de acertos e empates
    if (acertou) {
      acertos <- acertos + 1
      if (empate) {
        casos_empate <- casos_empate + 1
      }
    }
    
    # Adiciona ao relatório
    report <- rbind(report, data.frame(Discurso = i, 
                                       Classe_Real = mandato_real, 
                                       Predicao = predicao, 
                                       Acerto = acertou))
  }
  
  # Exibe métricas finais
  cat("Total de acertos:", acertos, "de", nrow(test_data), "\n")
  cat("Casos de empate (com acerto):", casos_empate, "\n")
  
  # Retorna o relatório
  return(report)
}





calcular_porcentagem_acerto <- function(dataframe) {
  # Agrupa por 'Classe_Real' e calcula a porcentagem de acerto
  porcentagem_acerto <- dataframe %>%
    group_by(Classe_Real) %>%
    summarise(
      Total = n(),
      Acertos = sum(Acerto == TRUE),
      Porcentagem_Acerto = (Acertos / Total) * 100
    )
  
  return(porcentagem_acerto)
}
    























fim_do_experimento2 <- function(completo, training_data, test_data, epsilon) {
  # Lista de classificadores e mandatos
  mandatos <- c("Sarney", "Collor", "Itamar", "FHC1", "FHC2", 
                "Lula1", "Lula2", "Dilma1", "Dilma2", "Temer", "Bolsonaro")
  classificadores <- lapply(mandatos, function(m) retorna_nb(completo, paste0("mandato", m), training_data))
  names(classificadores) <- mandatos
  
  # Variáveis de contagem
  acertos <- 0
  casos_empate <- 0
  
  # Lista para armazenar o relatório
  report <- data.frame(Discurso = integer(0), 
                       Classe_Real = character(0), 
                       Predicao = character(0), 
                       Acerto = logical(0))
  
  # Loop sobre os discursos de teste
  for (i in 1:nrow(test_data)) {
    maior_certeza <- 0
    mandatos_certos <- c()
    linha <- test_data[i, ]
    mandato_real <- linha$mandato
    
    # Verifica os classificadores
    for (mandato in mandatos) {
      partido_coluna <- paste0("mandato", mandato)
      test_temp <- cbind(as.matrix(linha[, 11:83]), Parties = completo[[partido_coluna]])
      test_prediction <- predict(classificadores[[mandato]], test_temp)
      certeza_atual <- test_prediction[1, 1]
      if (partido_coluna == "mandatoCollor") {
        certeza_atual <- test_prediction[1, 2]
      }
      
      # Lógica ajustada para considerar empates dentro do intervalo de epsilon
      if (certeza_atual >= maior_certeza - epsilon) {
        if (certeza_atual > maior_certeza) {
          maior_certeza <- certeza_atual
          mandatos_certos <- c(mandato) # Substitui pelo mandato com maior certeza
        } else {
          mandatos_certos <- c(mandatos_certos, mandato) # Adiciona ao vetor de mandatos
        }
      }
    }
    
    # Checa se houve empate
    
    
    # Verifica se acertou
    acertou <- mandato_real %in% mandatos_certos
    predicao <- ifelse(length(mandatos_certos) == 1, mandatos_certos[1], paste(mandatos_certos, collapse = ", "))
    
    # Adiciona ao relatório
    report <- rbind(report, data.frame(Discurso = i, 
                                       Classe_Real = mandato_real, 
                                       Predicao = predicao, 
                                       Acerto = acertou))
    
    # Incrementa o contador de acertos
    if (acertou) {
      acertos <- acertos + 1
    }
    if (length(mandatos_certos) > 1 && acertou) {
      casos_empate <- casos_empate + 1
    }
    
  }
  
  
  # Exibe métricas finais
  cat("Total de acertos:", acertos, "de", nrow(test_data), "\n")
  cat("Casos de empate:", casos_empate, "\n")
  
  # Retorna o relatório
  return(report)
}
    
    
 
  
 