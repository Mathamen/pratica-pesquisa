# Carregar o pacote necessário
# install.packages("dplyr") # Descomente se você não tiver o pacote 'dplyr' instalado
library(dplyr)

# Carregar o arquivo CSV
data <- read.csv("dados_para_pratica.csv")


# Calcular a média de um atributo específico (por exemplo, "atributo1")
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
#desvio_padrao_atributo <- sd(data$atributo1, na.rm = TRUE)
