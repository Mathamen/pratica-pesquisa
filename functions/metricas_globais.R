
gerar_metricas_globais <- function(data){
  metricas_globais <- list(
    medias_globais = list(
      media_anos = mean(data$date),
      media_length = mean(data$length),
      media_wordfish = mean(data$wordfish),
      media_analytic = mean(data$Analytic),
      media_tone = mean(data$Tone),
      media_clout = mean(data$Clout),
      media_authentic = mean(data$Authentic),
      media_posemo = mean(data$Posemo),
      media_negemo = mean(data$Negemo),
      media_past_focus = mean(data$Past.Focus),
      media_present_focus = mean(data$Present.Focus),
      media_future_focus = mean(data$Future.Focus)
    ),
    desvios_globais = list(
      desvio_padrao_anos = sd(data$date),
      desvio_padrao_length = sd(data$length),
      desvio_padrao_wordfish = sd(data$wordfish),
      desvio_padrao_analytic = sd(data$Analytic),
      desvio_padrao_tone = sd(data$Tone),
      desvio_padrao_clout = sd(data$Clout),
      desvio_padrao_authentic = sd(data$Authentic),
      desvio_padrao_posemo = sd(data$Posemo),
      desvio_padrao_negemo = sd(data$Negemo),
      desvio_padrao_past_focus = sd(data$Past.Focus),
      desvio_padrao_present_focus = sd(data$Present.Focus),
      desvio_padrao_future_focus = sd(data$Future.Focus)
    )
  )
  return(metricas_globais)
}