gerar_metricas_partidarias <- function(data){
  dados_pmdb <- data[data$PMDB == 1, ]
  media_pmdb <- list(
    media_anos = mean(dados_pmdb$date),
    media_length = mean(dados_pmdb$length),
    media_wordfish = mean(dados_pmdb$wordfish),
    media_analytic = mean(dados_pmdb$Analytic),
    media_tone = mean(dados_pmdb$Tone),
    media_clout = mean(dados_pmdb$Clout),
    media_authentic = mean(dados_pmdb$Authentic),
    media_posemo = mean(dados_pmdb$Posemo),
    media_negemo = mean(dados_pmdb$Negemo),
    media_past_focus = mean(dados_pmdb$Past.Focus),
    media_present_focus = mean(dados_pmdb$Present.Focus),
    media_future_focus = mean(dados_pmdb$Future.Focus)
  )
  
  desvio_padrao_pmdb <- list(
    desvio_padrao_anos = sd(dados_pmdb$date),
    desvio_padrao_length = sd(dados_pmdb$length),
    desvio_padrao_wordfish = sd(dados_pmdb$wordfish),
    desvio_padrao_analytic = sd(dados_pmdb$Analytic),
    desvio_padrao_tone = sd(dados_pmdb$Tone),
    desvio_padrao_clout = sd(dados_pmdb$Clout),
    desvio_padrao_authentic = sd(dados_pmdb$Authentic),
    desvio_padrao_posemo = sd(dados_pmdb$Posemo),
    desvio_padrao_negemo = sd(dados_pmdb$Negemo),
    desvio_padrao_past_focus = sd(dados_pmdb$Past.Focus),
    desvio_padrao_present_focus = sd(dados_pmdb$Present.Focus),
    desvio_padrao_future_focus = sd(dados_pmdb$Future.Focus)
  )
  
  # Cálculo para o partido PRN
  dados_prn <- data[data$PRN == 1, ]
  media_prn <- list(
    media_anos = mean(dados_prn$date),
    media_length = mean(dados_prn$length),
    media_wordfish = mean(dados_prn$wordfish),
    media_analytic = mean(dados_prn$Analytic),
    media_tone = mean(dados_prn$Tone),
    media_clout = mean(dados_prn$Clout),
    media_authentic = mean(dados_prn$Authentic),
    media_posemo = mean(dados_prn$Posemo),
    media_negemo = mean(dados_prn$Negemo),
    media_past_focus = mean(dados_prn$Past.Focus),
    media_present_focus = mean(dados_prn$Present.Focus),
    media_future_focus = mean(dados_prn$Future.Focus)
  )
  
  desvio_padrao_prn <- list(
    desvio_padrao_anos = sd(dados_prn$date),
    desvio_padrao_length = sd(dados_prn$length),
    desvio_padrao_wordfish = sd(dados_prn$wordfish),
    desvio_padrao_analytic = sd(dados_prn$Analytic),
    desvio_padrao_tone = sd(dados_prn$Tone),
    desvio_padrao_clout = sd(dados_prn$Clout),
    desvio_padrao_authentic = sd(dados_prn$Authentic),
    desvio_padrao_posemo = sd(dados_prn$Posemo),
    desvio_padrao_negemo = sd(dados_prn$Negemo),
    desvio_padrao_past_focus = sd(dados_prn$Past.Focus),
    desvio_padrao_present_focus = sd(dados_prn$Present.Focus),
    desvio_padrao_future_focus = sd(dados_prn$Future.Focus)
  )
  
  # Cálculo para o partido PSL
  dados_psl <- data[data$PSL == 1, ]
  media_psl <- list(
    media_anos = mean(dados_psl$date),
    media_length = mean(dados_psl$length),
    media_wordfish = mean(dados_psl$wordfish),
    media_analytic = mean(dados_psl$Analytic),
    media_tone = mean(dados_psl$Tone),
    media_clout = mean(dados_psl$Clout),
    media_authentic = mean(dados_psl$Authentic),
    media_posemo = mean(dados_psl$Posemo),
    media_negemo = mean(dados_psl$Negemo),
    media_past_focus = mean(dados_psl$Past.Focus),
    media_present_focus = mean(dados_psl$Present.Focus),
    media_future_focus = mean(dados_psl$Future.Focus)
  )
  
  desvio_padrao_psl <- list(
    desvio_padrao_anos = sd(dados_psl$date),
    desvio_padrao_length = sd(dados_psl$length),
    desvio_padrao_wordfish = sd(dados_psl$wordfish),
    desvio_padrao_analytic = sd(dados_psl$Analytic),
    desvio_padrao_tone = sd(dados_psl$Tone),
    desvio_padrao_clout = sd(dados_psl$Clout),
    desvio_padrao_authentic = sd(dados_psl$Authentic),
    desvio_padrao_posemo = sd(dados_psl$Posemo),
    desvio_padrao_negemo = sd(dados_psl$Negemo),
    desvio_padrao_past_focus = sd(dados_psl$Past.Focus),
    desvio_padrao_present_focus = sd(dados_psl$Present.Focus),
    desvio_padrao_future_focus = sd(dados_psl$Future.Focus)
  )
  
  # Cálculo para o partido PSDB
  dados_psdb <- data[data$PSDB == 1, ]
  media_psdb <- list(
    media_anos = mean(dados_psdb$date),
    media_length = mean(dados_psdb$length),
    media_wordfish = mean(dados_psdb$wordfish),
    media_analytic = mean(dados_psdb$Analytic),
    media_tone = mean(dados_psdb$Tone),
    media_clout = mean(dados_psdb$Clout),
    media_authentic = mean(dados_psdb$Authentic),
    media_posemo = mean(dados_psdb$Posemo),
    media_negemo = mean(dados_psdb$Negemo),
    media_past_focus = mean(dados_psdb$Past.Focus),
    media_present_focus = mean(dados_psdb$Present.Focus),
    media_future_focus = mean(dados_psdb$Future.Focus)
  )
  
  desvio_padrao_psdb <- list(
    desvio_padrao_anos = sd(dados_psdb$date),
    desvio_padrao_length = sd(dados_psdb$length),
    desvio_padrao_wordfish = sd(dados_psdb$wordfish),
    desvio_padrao_analytic = sd(dados_psdb$Analytic),
    desvio_padrao_tone = sd(dados_psdb$Tone),
    desvio_padrao_clout = sd(dados_psdb$Clout),
    desvio_padrao_authentic = sd(dados_psdb$Authentic),
    desvio_padrao_posemo = sd(dados_psdb$Posemo),
    desvio_padrao_negemo = sd(dados_psdb$Negemo),
    desvio_padrao_past_focus = sd(dados_psdb$Past.Focus),
    desvio_padrao_present_focus = sd(dados_psdb$Present.Focus),
    desvio_padrao_future_focus = sd(dados_psdb$Future.Focus)
  )
  
  # Cálculo para o partido PT
  dados_pt <- data[data$PT == 1, ]
  media_pt <- list(
    media_anos = mean(dados_pt$date),
    media_length = mean(dados_pt$length),
    media_wordfish = mean(dados_pt$wordfish),
    media_analytic = mean(dados_pt$Analytic),
    media_tone = mean(dados_pt$Tone),
    media_clout = mean(dados_pt$Clout),
    media_authentic = mean(dados_pt$Authentic),
    media_posemo = mean(dados_pt$Posemo),
    media_negemo = mean(dados_pt$Negemo),
    media_past_focus = mean(dados_pt$Past.Focus),
    media_present_focus = mean(dados_pt$Present.Focus),
    media_future_focus = mean(dados_pt$Future.Focus)
  )
  
  desvio_padrao_pt <- list(
    desvio_padrao_anos = sd(dados_pt$date),
    desvio_padrao_length = sd(dados_pt$length),
    desvio_padrao_wordfish = sd(dados_pt$wordfish),
    desvio_padrao_analytic = sd(dados_pt$Analytic),
    desvio_padrao_tone = sd(dados_pt$Tone),
    desvio_padrao_clout = sd(dados_pt$Clout),
    desvio_padrao_authentic = sd(dados_pt$Authentic),
    desvio_padrao_posemo = sd(dados_pt$Posemo),
    desvio_padrao_negemo = sd(dados_pt$Negemo),
    desvio_padrao_past_focus = sd(dados_pt$Past.Focus),
    desvio_padrao_present_focus = sd(dados_pt$Present.Focus),
    desvio_padrao_future_focus = sd(dados_pt$Future.Focus)
  )
  
  # Exibir resultados
  metricas_partidarias <-list(
    PMDB = list(Media = media_pmdb, Desvio_Padrao = desvio_padrao_pmdb),
    PRN = list(Media = media_prn, Desvio_Padrao = desvio_padrao_prn),
    PSL = list(Media = media_psl, Desvio_Padrao = desvio_padrao_psl),
    PSDB = list(Media = media_psdb, Desvio_Padrao = desvio_padrao_psdb),
    PT = list(Media = media_pt, Desvio_Padrao = desvio_padrao_pt)
  )
  return(metricas_partidarias)
}