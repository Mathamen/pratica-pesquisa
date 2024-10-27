# Carregar o pacote necessário
#install.packages("dplyr") # Descomente se você não tiver o pacote 'dplyr' instalado
# install.packages("ggplot2") # Descomente se você não tiver o pacote 'ggplot2' instalado
library(ggplot2)
library(dplyr)
library(tidyr)

# Carregar o arquivo CSV
data <- read.csv("dados_para_pratica.csv")


#Cálculo de todas as médias (global)
media_anos <- mean(data$date, na.rm = TRUE)
media_length <- mean(data$length, na.rm = TRUE)
media_wordfish <- mean(data$wordfish, na.rm = TRUE)
media_analytic <- mean(data$Analytic, na.rm = TRUE)
media_tone <- mean(data$Tone, na.rm = TRUE)
media_clout <- mean(data$Clout, na.rm = TRUE)
media_authentic <- mean(data$Authentic, na.rm = TRUE)
media_posemo <- mean(data$Posemo, na.rm = TRUE)
media_negemo <- mean(data$Negemo, na.rm = TRUE)
media_past_focus <- mean(data$Past.Focus, na.rm = TRUE)
media_present_focus <- mean(data$Present.Focus, na.rm = TRUE)
media_future_focus <- mean(data$Future.Focus, na.rm = TRUE)

#Cálculo dos Desvios padrões (global)
desvio_padrao_anos <- sd(data$date, na.rm = TRUE)
desvio_padrao_length <- sd(data$length, na.rm = TRUE)
desvio_padrao_wordfish <- sd(data$wordfish, na.rm = TRUE)
desvio_padrao_analytic <- sd(data$Analytic, na.rm = TRUE)
desvio_padrao_tone <- sd(data$Tone, na.rm = TRUE)
desvio_padrao_clout <- sd(data$Clout, na.rm = TRUE)
desvio_padrao_authentic <- sd(data$Authentic, na.rm = TRUE)
desvio_padrao_posemo <- sd(data$Posemo, na.rm = TRUE)
desvio_padrao_negemo <- sd(data$Negemo, na.rm = TRUE)
desvio_padrao_past_focus <- sd(data$Past.Focus, na.rm = TRUE)
desvio_padrao_present_focus <- sd(data$Present.Focus, na.rm = TRUE)
desvio_padrao_future_focus <- sd(data$Future.Focus, na.rm = TRUE)




# -----------------------------------------------------------------
# Por partido agora


# Cálculo para o partido PMDB
dados_pmdb <- data[data$PMDB == 1, ]
media_pmdb <- list(
  media_anos = mean(dados_pmdb$date, na.rm = TRUE),
  media_length = mean(dados_pmdb$length, na.rm = TRUE),
  media_wordfish = mean(dados_pmdb$wordfish, na.rm = TRUE),
  media_analytic = mean(dados_pmdb$Analytic, na.rm = TRUE),
  media_tone = mean(dados_pmdb$Tone, na.rm = TRUE),
  media_clout = mean(dados_pmdb$Clout, na.rm = TRUE),
  media_authentic = mean(dados_pmdb$Authentic, na.rm = TRUE),
  media_posemo = mean(dados_pmdb$Posemo, na.rm = TRUE),
  media_negemo = mean(dados_pmdb$Negemo, na.rm = TRUE),
  media_past_focus = mean(dados_pmdb$Past.Focus, na.rm = TRUE),
  media_present_focus = mean(dados_pmdb$Present.Focus, na.rm = TRUE),
  media_future_focus = mean(dados_pmdb$Future.Focus, na.rm = TRUE)
)

desvio_padrao_pmdb <- list(
  desvio_padrao_anos = sd(dados_pmdb$date, na.rm = TRUE),
  desvio_padrao_length = sd(dados_pmdb$length, na.rm = TRUE),
  desvio_padrao_wordfish = sd(dados_pmdb$wordfish, na.rm = TRUE),
  desvio_padrao_analytic = sd(dados_pmdb$Analytic, na.rm = TRUE),
  desvio_padrao_tone = sd(dados_pmdb$Tone, na.rm = TRUE),
  desvio_padrao_clout = sd(dados_pmdb$Clout, na.rm = TRUE),
  desvio_padrao_authentic = sd(dados_pmdb$Authentic, na.rm = TRUE),
  desvio_padrao_posemo = sd(dados_pmdb$Posemo, na.rm = TRUE),
  desvio_padrao_negemo = sd(dados_pmdb$Negemo, na.rm = TRUE),
  desvio_padrao_past_focus = sd(dados_pmdb$Past.Focus, na.rm = TRUE),
  desvio_padrao_present_focus = sd(dados_pmdb$Present.Focus, na.rm = TRUE),
  desvio_padrao_future_focus = sd(dados_pmdb$Future.Focus, na.rm = TRUE)
)

# Cálculo para o partido PRN
dados_prn <- data[data$PRN == 1, ]
media_prn <- list(
  media_anos = mean(dados_prn$date, na.rm = TRUE),
  media_length = mean(dados_prn$length, na.rm = TRUE),
  media_wordfish = mean(dados_prn$wordfish, na.rm = TRUE),
  media_analytic = mean(dados_prn$Analytic, na.rm = TRUE),
  media_tone = mean(dados_prn$Tone, na.rm = TRUE),
  media_clout = mean(dados_prn$Clout, na.rm = TRUE),
  media_authentic = mean(dados_prn$Authentic, na.rm = TRUE),
  media_posemo = mean(dados_prn$Posemo, na.rm = TRUE),
  media_negemo = mean(dados_prn$Negemo, na.rm = TRUE),
  media_past_focus = mean(dados_prn$Past.Focus, na.rm = TRUE),
  media_present_focus = mean(dados_prn$Present.Focus, na.rm = TRUE),
  media_future_focus = mean(dados_prn$Future.Focus, na.rm = TRUE)
)

desvio_padrao_prn <- list(
  desvio_padrao_anos = sd(dados_prn$date, na.rm = TRUE),
  desvio_padrao_length = sd(dados_prn$length, na.rm = TRUE),
  desvio_padrao_wordfish = sd(dados_prn$wordfish, na.rm = TRUE),
  desvio_padrao_analytic = sd(dados_prn$Analytic, na.rm = TRUE),
  desvio_padrao_tone = sd(dados_prn$Tone, na.rm = TRUE),
  desvio_padrao_clout = sd(dados_prn$Clout, na.rm = TRUE),
  desvio_padrao_authentic = sd(dados_prn$Authentic, na.rm = TRUE),
  desvio_padrao_posemo = sd(dados_prn$Posemo, na.rm = TRUE),
  desvio_padrao_negemo = sd(dados_prn$Negemo, na.rm = TRUE),
  desvio_padrao_past_focus = sd(dados_prn$Past.Focus, na.rm = TRUE),
  desvio_padrao_present_focus = sd(dados_prn$Present.Focus, na.rm = TRUE),
  desvio_padrao_future_focus = sd(dados_prn$Future.Focus, na.rm = TRUE)
)

# Cálculo para o partido PSL
dados_psl <- data[data$PSL == 1, ]
media_psl <- list(
  media_anos = mean(dados_psl$date, na.rm = TRUE),
  media_length = mean(dados_psl$length, na.rm = TRUE),
  media_wordfish = mean(dados_psl$wordfish, na.rm = TRUE),
  media_analytic = mean(dados_psl$Analytic, na.rm = TRUE),
  media_tone = mean(dados_psl$Tone, na.rm = TRUE),
  media_clout = mean(dados_psl$Clout, na.rm = TRUE),
  media_authentic = mean(dados_psl$Authentic, na.rm = TRUE),
  media_posemo = mean(dados_psl$Posemo, na.rm = TRUE),
  media_negemo = mean(dados_psl$Negemo, na.rm = TRUE),
  media_past_focus = mean(dados_psl$Past.Focus, na.rm = TRUE),
  media_present_focus = mean(dados_psl$Present.Focus, na.rm = TRUE),
  media_future_focus = mean(dados_psl$Future.Focus, na.rm = TRUE)
)

desvio_padrao_psl <- list(
  desvio_padrao_anos = sd(dados_psl$date, na.rm = TRUE),
  desvio_padrao_length = sd(dados_psl$length, na.rm = TRUE),
  desvio_padrao_wordfish = sd(dados_psl$wordfish, na.rm = TRUE),
  desvio_padrao_analytic = sd(dados_psl$Analytic, na.rm = TRUE),
  desvio_padrao_tone = sd(dados_psl$Tone, na.rm = TRUE),
  desvio_padrao_clout = sd(dados_psl$Clout, na.rm = TRUE),
  desvio_padrao_authentic = sd(dados_psl$Authentic, na.rm = TRUE),
  desvio_padrao_posemo = sd(dados_psl$Posemo, na.rm = TRUE),
  desvio_padrao_negemo = sd(dados_psl$Negemo, na.rm = TRUE),
  desvio_padrao_past_focus = sd(dados_psl$Past.Focus, na.rm = TRUE),
  desvio_padrao_present_focus = sd(dados_psl$Present.Focus, na.rm = TRUE),
  desvio_padrao_future_focus = sd(dados_psl$Future.Focus, na.rm = TRUE)
)

# Cálculo para o partido PSDB
dados_psdb <- data[data$PSDB == 1, ]
media_psdb <- list(
  media_anos = mean(dados_psdb$date, na.rm = TRUE),
  media_length = mean(dados_psdb$length, na.rm = TRUE),
  media_wordfish = mean(dados_psdb$wordfish, na.rm = TRUE),
  media_analytic = mean(dados_psdb$Analytic, na.rm = TRUE),
  media_tone = mean(dados_psdb$Tone, na.rm = TRUE),
  media_clout = mean(dados_psdb$Clout, na.rm = TRUE),
  media_authentic = mean(dados_psdb$Authentic, na.rm = TRUE),
  media_posemo = mean(dados_psdb$Posemo, na.rm = TRUE),
  media_negemo = mean(dados_psdb$Negemo, na.rm = TRUE),
  media_past_focus = mean(dados_psdb$Past.Focus, na.rm = TRUE),
  media_present_focus = mean(dados_psdb$Present.Focus, na.rm = TRUE),
  media_future_focus = mean(dados_psdb$Future.Focus, na.rm = TRUE)
)

desvio_padrao_psdb <- list(
  desvio_padrao_anos = sd(dados_psdb$date, na.rm = TRUE),
  desvio_padrao_length = sd(dados_psdb$length, na.rm = TRUE),
  desvio_padrao_wordfish = sd(dados_psdb$wordfish, na.rm = TRUE),
  desvio_padrao_analytic = sd(dados_psdb$Analytic, na.rm = TRUE),
  desvio_padrao_tone = sd(dados_psdb$Tone, na.rm = TRUE),
  desvio_padrao_clout = sd(dados_psdb$Clout, na.rm = TRUE),
  desvio_padrao_authentic = sd(dados_psdb$Authentic, na.rm = TRUE),
  desvio_padrao_posemo = sd(dados_psdb$Posemo, na.rm = TRUE),
  desvio_padrao_negemo = sd(dados_psdb$Negemo, na.rm = TRUE),
  desvio_padrao_past_focus = sd(dados_psdb$Past.Focus, na.rm = TRUE),
  desvio_padrao_present_focus = sd(dados_psdb$Present.Focus, na.rm = TRUE),
  desvio_padrao_future_focus = sd(dados_psdb$Future.Focus, na.rm = TRUE)
)

# Cálculo para o partido PT
dados_pt <- data[data$PT == 1, ]
media_pt <- list(
  media_anos = mean(dados_pt$date, na.rm = TRUE),
  media_length = mean(dados_pt$length, na.rm = TRUE),
  media_wordfish = mean(dados_pt$wordfish, na.rm = TRUE),
  media_analytic = mean(dados_pt$Analytic, na.rm = TRUE),
  media_tone = mean(dados_pt$Tone, na.rm = TRUE),
  media_clout = mean(dados_pt$Clout, na.rm = TRUE),
  media_authentic = mean(dados_pt$Authentic, na.rm = TRUE),
  media_posemo = mean(dados_pt$Posemo, na.rm = TRUE),
  media_negemo = mean(dados_pt$Negemo, na.rm = TRUE),
  media_past_focus = mean(dados_pt$Past.Focus, na.rm = TRUE),
  media_present_focus = mean(dados_pt$Present.Focus, na.rm = TRUE),
  media_future_focus = mean(dados_pt$Future.Focus, na.rm = TRUE)
)

desvio_padrao_pt <- list(
  desvio_padrao_anos = sd(dados_pt$date, na.rm = TRUE),
  desvio_padrao_length = sd(dados_pt$length, na.rm = TRUE),
  desvio_padrao_wordfish = sd(dados_pt$wordfish, na.rm = TRUE),
  desvio_padrao_analytic = sd(dados_pt$Analytic, na.rm = TRUE),
  desvio_padrao_tone = sd(dados_pt$Tone, na.rm = TRUE),
  desvio_padrao_clout = sd(dados_pt$Clout, na.rm = TRUE),
  desvio_padrao_authentic = sd(dados_pt$Authentic, na.rm = TRUE),
  desvio_padrao_posemo = sd(dados_pt$Posemo, na.rm = TRUE),
  desvio_padrao_negemo = sd(dados_pt$Negemo, na.rm = TRUE),
  desvio_padrao_past_focus = sd(dados_pt$Past.Focus, na.rm = TRUE),
  desvio_padrao_present_focus = sd(dados_pt$Present.Focus, na.rm = TRUE),
  desvio_padrao_future_focus = sd(dados_pt$Future.Focus, na.rm = TRUE)
)

# Exibir resultados
list(
  PMDB = list(Media = media_pmdb, Desvio_Padrao = desvio_padrao_pmdb),
  PRN = list(Media = media_prn, Desvio_Padrao = desvio_padrao_prn),
  PSL = list(Media = media_psl, Desvio_Padrao = desvio_padrao_psl),
  PSDB = list(Media = media_psdb, Desvio_Padrao = desvio_padrao_psdb),
  PT = list(Media = media_pt, Desvio_Padrao = desvio_padrao_pt)
)

#-------------------------------------------------------------
# Criação dos gráficos box-plot e dispersão

criar_graficos_partido <- function(dados_partido, partido_nome) {
  
  # Aqui estou transformando e escolhendo os dados. Não estou fazendo com date, wordfish e length.
  # Não escolhi os 3 pois são dados que não considerei interessantes, além de estragar o plot.
  dados_long <- dados_partido %>%
    pivot_longer(cols = c(Analytic, Tone, Clout, Authentic, Posemo, Negemo, Past.Focus, Present.Focus, Future.Focus),
                 names_to = "atributo",
                 values_to = "valor")
  
  # Criação do Box Plot
  box_plot <- ggplot(dados_long, aes(x = atributo, y = valor)) +
    geom_boxplot(fill = "lightblue", outlier.colour = "red", outlier.size = 2) +
    theme_minimal() +
    labs(title = paste("Boxplot de Atributos -", partido_nome), x = "Atributos", y = "Valor") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Criação do dispersão
  dispersao_plot <- ggplot(dados_long, aes(x = atributo, y = valor)) +
    geom_point(alpha = 0.5, color = "blue") +
    theme_minimal() +
    labs(title = paste("Gráfico de Dispersão -", partido_nome), x = "Atributos", y = "Valor") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Feedback Visual dos gráficos
  print(box_plot)
  print(dispersao_plot)
}

# Gerando gráficos por partido
# Estas funções DEPENDEM da geração dos dados por partido, criado algumas linhas anteriormente.
criar_graficos_partido(dados_pmdb, "PMDB")
criar_graficos_partido(dados_prn, "PRN")
criar_graficos_partido(dados_psl, "PSL")
criar_graficos_partido(dados_psdb, "PSDB")
criar_graficos_partido(dados_pt, "PT")
