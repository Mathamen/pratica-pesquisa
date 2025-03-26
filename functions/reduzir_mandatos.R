
  reduzir_mandatos <- function(df, mandato_interessante) {
  # Verifica se a coluna 'mandato' existe no dataframe
  if (!"mandato" %in% colnames(df)) {
    stop("O dataframe não contém uma coluna chamada 'mandato'.")
  }
  
  # Inicializa o dataframe de retorno vazio
  resultado <- data.frame()
  
  # Loop para ir pegando todos os discursos até o mandato interessante
  for (i in 1:nrow(df)) {
    if (df$mandato[i] <= mandato_interessante) {
      # Adiciona as linhas que têm o mandato menor ou igual ao mandato interessante
      resultado <- rbind(resultado, df[i, , drop = FALSE])
    }
  }
  
  # Retorna o dataframe com os discursos até o mandato interessante
  return(resultado)
  }


retornar_mandatos_reduzidos <- function(fun_data){
  mandatos_sarney <- reduzir_mandatos(fun_data, "Sarney")
  mandatos_collor <- rbind(reduzir_mandatos(fun_data, "Collor"),mandatos_sarney)
  mandatos_itamar <- rbind(reduzir_mandatos(fun_data, "Itamar"),mandatos_collor)
  mandatos_fhc1 <- rbind(reduzir_mandatos(fun_data, "FHC1"),mandatos_itamar)
  mandatos_fhc2 <- rbind(reduzir_mandatos(fun_data, "FHC2"),mandatos_fhc1)
  mandatos_lula1 <- rbind(reduzir_mandatos(fun_data, "Lula1"),mandatos_fhc2)
  mandatos_lula2 <- rbind(reduzir_mandatos(fun_data, "Lula2"),mandatos_lula1)
  mandatos_dilma1 <- rbind(reduzir_mandatos(fun_data, "Dilma1"),mandatos_lula2)
  mandatos_dilma2 <- rbind(reduzir_mandatos(fun_data, "Dilma2"),mandatos_dilma1)
  mandatos_temer <- rbind(reduzir_mandatos(fun_data, "Temer"),mandatos_dilma2)
  mandatos_bolsonaro <- rbind(reduzir_mandatos(fun_data, "Temer"),mandatos_temer)
  
  
  
  mandatos <- list(mandatos_sarney,mandatos_collor,mandatos_itamar,mandatos_fhc1,mandatos_fhc2,mandatos_lula1,mandatos_lula2,mandatos_dilma1,mandatos_dilma2,mandatos_temer,mandatos_bolsonaro)

  return(mandatos)
  
  
  
  
  
  # Aqui vai o código que você já possui para reduzir o dataset, sem alteração
  # (só estamos mantendo o código da sua função original)
  
  # Criação da lista de mandatos (o mesmo código que você tem)
  mandatos <- list()
  
  # Exemplo para colocar cada conjunto de mandatos na lista
  mandatos[["Sarney"]] <- reduzir_mandatos(df, "Sarney")
  mandatos[["Collor"]] <- rbind(reduzir_mandatos(df, "Collor"), mandatos[["Sarney"]])
  mandatos[["Itamar"]] <- rbind(reduzir_mandatos(df, "Itamar"), mandatos[["Collor"]])
  mandatos[["FHC1"]] <- rbind(reduzir_mandatos(df, "FHC1"), mandatos[["Itamar"]])
  mandatos[["FHC2"]] <- rbind(reduzir_mandatos(df, "FHC2"), mandatos[["FHC1"]])
  mandatos[["Lula1"]] <- rbind(reduzir_mandatos(df, "Lula1"), mandatos[["FHC2"]])
  mandatos[["Lula2"]] <- rbind(reduzir_mandatos(df, "Lula2"), mandatos[["Lula1"]])
  mandatos[["Dilma1"]] <- rbind(reduzir_mandatos(df, "Dilma1"), mandatos[["Lula2"]])
  mandatos[["Dilma2"]] <- rbind(reduzir_mandatos(df, "Dilma2"), mandatos[["Dilma1"]])
  mandatos[["Temer"]] <- rbind(reduzir_mandatos(df, "Temer"), mandatos[["Dilma2"]])
  mandatos[["Bolsonaro"]] <- rbind(reduzir_mandatos(df, "Bolsonaro"), mandatos[["Temer"]])
  
  # Retorna a lista contendo todos os mandatos
  return(mandatos)
  
  }