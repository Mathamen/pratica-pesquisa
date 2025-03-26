adicionar_coluna_mandato <- function(dataset) {
  # Regras de mandato
  regras_mandato <- list(
    list(partido = "PSL", inicio = 2019, fim = 2020, mandato = "Bolsonaro"),
    list(partido = "PMDB", inicio = 2016, fim = 2018, mandato = "Temer"),
    list(partido = "PT", inicio = 2015, fim = 2016, mandato = "Dilma2"),
    list(partido = "PT", inicio = 2011, fim = 2014, mandato = "Dilma1"),
    list(partido = "PT", inicio = 2007, fim = 2010, mandato = "Lula2"),
    list(partido = "PT", inicio = 2003, fim = 2006, mandato = "Lula1"),
    list(partido = "PSDB", inicio = 1999, fim = 2002, mandato = "FHC2"),
    list(partido = "PSDB", inicio = 1995, fim = 1998, mandato = "FHC1"),
    list(partido = "PMDB", inicio = 1992, fim = 1994, mandato = "Itamar"),
    list(partido = "PRN", inicio = 1990, fim = 1992, mandato = "Collor"),
    list(partido = "PMDB", inicio = 1985, fim = 1990, mandato = "Sarney")
  )
  
  # Casos especiais
  sem_partido <- list()
  ambiguidades <- list()
  
  # Criação coluna mandato
  dataset$mandato <- apply(dataset, 1, function(linha) {
    ano <- as.numeric(linha["date"])
    
    # Caso base, deve passar por aqui antes
    for (regra in regras_mandato) {
      if (ano >= regra$inicio && ano <= regra$fim) {
        return(regra$mandato)
      }
    }
    
    # Iteração para casos como impeachment
    partidos <- which(as.numeric(linha[-1]) == 1)
    if (length(partidos) == 0) {
      # Teste sem partido
      sem_partido <<- append(sem_partido, list(linha))
      return("Sem partido")
    }
    
    if (length(partidos) > 1) {
      # Teste ambiguidade
      ambiguidades <<- append(ambiguidades, list(linha))
      return("Ambiguidade")
    }
    
    nome_partido <- colnames(dataset)[partidos + 1]
    for (regra in regras_mandato) {
      if (ano >= regra$inicio && ano <= regra$fim && nome_partido == regra$partido) {
        return(regra$mandato)
      }
    }
    
    
    sem_partido <<- append(sem_partido, list(linha))
    return("Sem mandato")
  })
  
  # Controle, não foi utilizado
  if (length(sem_partido) > 0) {
    cat("Casos 'Sem partido':\n")
    print(do.call(rbind, sem_partido))
  }
  
  if (length(ambiguidades) > 0) {
    cat("Casos 'Ambiguidade':\n")
    print(do.call(rbind, ambiguidades))
  }
  
  return(dataset)
}