# Carregar o pacote necessário
install.packages("dplyr") # Descomente se você não tiver o pacote 'dplyr'
install.packages("ggplot2") # Descomente se você não tiver o pacote 'ggplot2'
install.packages("gridExtra")
load_library("gridExtra")
library(ggplot2)
library(dplyr)
library(tidyr)

# Carregar o arquivo CSV
data <- read.csv("dados_para_pratica.csv")


#Cálculo de todas as médias (global)
media_anos <- mean(data$date, na.rm = TRUE)
media_length <- mean(data$length, na.rm = TRUE)
media_wordfish <- mean(data$wordfish, na.rm = TRUE)
#LIWC
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
#LIWC
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

#---------------------------------------------------------------
# Gráficos de densidade para cada atributo
# Para obter os dados, rode o código que começa na linha 311 (data_novo)
ggplot(data_novo, aes(x = date, fill = partido)) +
  geom_density(alpha = 0.5) + 
  labs(title = "Gráfico de Densidade por Classe", x = "Date", y = "Densidade") +
  theme_minimal()

ggplot(data_novo, aes(x = length, fill = partido)) +
  geom_density(alpha = 0.5) + 
  labs(title = "Gráfico de Densidade por Classe", x = "Length", y = "Densidade") +
  theme_minimal()

ggplot(data_novo, aes(x = Analytic, fill = partido)) +
  geom_density(alpha = 0.5) + 
  labs(title = "Gráfico de Densidade por Classe", x = "Analytic", y = "Densidade") +
  theme_minimal()

ggplot(data_novo, aes(x = Tone, fill  = partido)) +
  geom_density(alpha = 0.5) + 
  labs(title = "Gráfico de Densidade por Classe", x = "Tone", y = "Densidade") +
  theme_minimal()

ggplot(data_novo, aes(x = Clout, fill = partido)) +
  geom_density(alpha = 0.5) + 
  labs(title = "Gráfico de Densidade por Classe", x = "Clout", y = "Densidade") +
  theme_minimal()

ggplot(data_novo, aes(x = Authentic, fill = partido)) +
  geom_density(alpha = 0.5) + 
  labs(title = "Gráfico de Densidade por Classe", x = "Authentic", y = "Densidade") +
  theme_minimal()

ggplot(data_novo, aes(x = Posemo, fill = partido)) +
  geom_density(alpha = 0.5) + 
  labs(title = "Gráfico de Densidade por Classe", x = "Posemo", y = "Densidade") +
  theme_minimal()

ggplot(data_novo, aes(x = Negemo, fill = partido)) +
  geom_density(alpha = 0.5) + 
  labs(title = "Gráfico de Densidade por Classe", x = "Negemo", y = "Densidade") +
  theme_minimal()


ggplot(data_novo, aes(x = Past.Focus, fill = partido)) +
  geom_density(alpha = 0.5) + 
  labs(title = "Gráfico de Densidade por Classe", x = "Past Focus", y = "Densidade") +
  theme_minimal()

ggplot(data_novo, aes(x = Present.Focus, fill = partido)) +
  geom_density(alpha = 0.5) + 
  labs(title = "Gráfico de Densidade por Classe", x = "Present Focus", y = "Densidade") +
  theme_minimal()

ggplot(data_novo, aes(x = Future.Focus, fill = partido)) +
  geom_density(alpha = 0.5) + 
  labs(title = "Gráfico de Densidade por Classe", x = "Future Focus", y = "Densidade") +
  theme_minimal()

ggplot(data_novo, aes(x = Compare, fill = partido)) +
  geom_density(alpha = 0.5) + 
  labs(title = "Gráfico de Densidade por Classe", x = "Compare", y = "Densidade") +
  theme_minimal()

#-------------------------------------------------------------
# Criação dos gráficos box-plot e dispersão

criar_graficos_partido <- function(dados_partido, partido_nome) {
  
  # Aqui estou transformando e escolhendo os dados. 
  #Não estou fazendo com date, wordfish e length.
  # Não escolhi os 3 pois são dados que não considerei interessantes, além de estragar o plot.
  dados_long <- dados_partido %>%
    pivot_longer(cols = c(Analytic, Tone, Clout, Authentic, Posemo, Negemo, Past.Focus, Present.Focus, Future.Focus),
                 names_to = "atributo",
                 values_to = "valor")
  
  # Criação do Box Plot - TODO : Separar em 2 gráficos?
  box_plot <- ggplot(dados_long, aes(x = atributo, y = valor)) +
    geom_boxplot(fill = "lightblue", outlier.colour = "red", outlier.size = 2) +
    theme_minimal() +
    labs(title = paste("Boxplot de Atributos -", partido_nome), x = "Atributos", y = "Valor") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  
  # Criação do dispersão TODO : Levar para o Ogasawara
  dispersao_plot <- ggplot(dados_long, aes(x = atributo, y = valor)) +
    geom_point(alpha = 0.5, color = "blue") +
    theme_minimal() +
    labs(title = paste("Gráfico de Dispersão -", partido_nome), x = "Atributos", y = "Valor") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Feedback Visual dos gráficos
  print(box_plot)
  print(dispersao_plot)
}

criar_scatter_date <- function() {
  p1 <- plot_scatter(data_novo |> select(x = date, value = length, variable = partido), 
                      label_x = "date", label_y = "length")
  
  p2 <- plot_scatter(data_novo |> select(x = date, value = wordfish, variable = partido), 
                      label_x = "date", label_y = "wordfish")

  p3 <- plot_scatter(data_novo |> select(x = date, value = Analytic, variable = partido), 
                      label_x = "date", label_y = "Analytic")

  p4 <- plot_scatter(data_novo |> select(x = date, value = Tone, variable = partido), 
                      label_x = "date", label_y = "Tone")

  p5 <- plot_scatter(data_novo |> select(x = date, value = Clout, variable = partido), 
                      label_x = "date", label_y = "Clout")

  p6 <- plot_scatter(data_novo |> select(x = date, value = Authentic, variable = partido), 
                     label_x = "date", label_y = "Authentic")
  
  p7 <- plot_scatter(data_novo |> select(x = date, value = Posemo, variable = partido), 
                      label_x = "date", label_y = "Posemo")
  
  p8 <- plot_scatter(data_novo |> select(x = date, value = Negemo, variable = partido), 
                     label_x = "date", label_y = "Negemo")
  
  p9 <- plot_scatter(data_novo |> select(x = date, value = Compare, variable = partido), 
                     label_x = "date", label_y = "Compare")
  
  p10 <- plot_scatter(data_novo |> select(x = date, value = Past.Focus, variable = partido), 
                     label_x = "date", label_y = "Past")
  
  p11 <- plot_scatter(data_novo |> select(x = date, value = Present.Focus, variable = partido), 
                     label_x = "date", label_y = "Present")
  
  p12 <- plot_scatter(data_novo |> select(x = date, value = Future.Focus, variable = partido), 
                     label_x = "date", label_y = "Future")

  grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, ncol = 4)
}

criar_scatter_date()


criar_graficos_partido(dados_pmdb, "PMDB")
criar_graficos_partido(dados_prn, "PRN")
criar_graficos_partido(dados_psl, "PSL")
criar_graficos_partido(dados_psdb, "PSDB")
criar_graficos_partido(dados_pt, "PT")


#----------------------------------------------
# Solução da questão 2:
# Selecionar apenas as colunas numéricas, excluindo as colunas dos partidos 
#(substitua os nomes dos partidos conforme necessário)
atributos_numericos <- data[, sapply(data, is.numeric)]
atributos_numericos <- atributos_numericos[, !(names(atributos_numericos) %in% c("PMDB", "PRN", "PSL", "PSDB", "PT"))]

# Discretizar cada coluna numérica em "Baixo", "Médio" e "Alto"
atributos_discretizados <- data.frame(lapply(atributos_numericos, function(coluna) {
  cut(coluna,
      breaks = quantile(coluna, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
      labels = c("Baixo", "Médio", "Alto"),
      include.lowest = TRUE)
}))

# Renomear as colunas discretizadas com o sufixo "_discretizado"
names(atributos_discretizados) <- paste0(names(atributos_discretizados), "_discretizado")

# Combinar o dataset original com as colunas discretizadas
data_novo_discretizado <- cbind(data, atributos_discretizados)



# A b não está codificada pois o dataset já se encontra em mapeamento categórico

# ---------------------------------------------------------------
# questão 3, k-means

# fazendo a classificação com os nomes dos atributos para o K means
# Criar uma cópia do dataset original
data_novo <- data

# Adicionar a nova coluna 'partido' na cópia com base nos valores das colunas de partidos
data_novo$partido <- apply(data_novo[, c("PMDB", "PRN", "PSL", "PSDB", "PT")], 1, function(x) {
  if (x["PMDB"] == 1) {
    return("PMDB")
  } else if (x["PRN"] == 1) {
    return("PRN")
  } else if (x["PSL"] == 1) {
    return("PSL")
  } else if (x["PSDB"] == 1) {
    return("PSDB")
  } else if (x["PT"] == 1) {
    return("PT")
  } else {
    return(NA)  # Caso nenhuma coluna seja 1, define como NA
  }
})




# DAL ToolBox
# version 1.0.767

source("https://raw.githubusercontent.com/cefet-rj-dal/daltoolbox/main/jupyter.R")

#loading DAL
load_library("daltoolbox")

# um cluster para cada partido
model <- cluster_kmeans(k=5)
model <- fit(model, data[,11:20])
clu <- cluster(model, data[,11:20])
table(clu)
eval <- evaluate(model, clu, data_novo$partido)
eval


# agora com o min max 
data_novo_minmax <- transform(fit(minmax(), data_novo), data_novo)
model <- cluster_kmeans(k=5)
model <- fit(model, data_novo_minmax[,11:20])
clu <- cluster(model, data_novo_minmax[,11:20])
table(clu)
eval <- evaluate(model, clu, data_novo_minmax$partido)
eval   


#-----------------------------------------------------------------------
#Questão 4


source("https://raw.githubusercontent.com/cefet-rj-dal/daltoolbox/main/jupyter.R")

#loading DAL
load_library("daltoolbox")


data_novo$partido <- factor(data_novo$partido)

# Agora você pode obter os níveis corretamente
slevels <- levels(data_novo$partido)
print(slevels)


discursos <- cbind(as.matrix(data_novo[,11:20]), Parties=data_novo$partido)

set.seed(1)
sr <- sample_random()
sr <- train_test(sr, discursos)
discursos_train <- sr$train
discursos_test <- sr$test


tbl <- rbind(table(discursos[,"Parties"]), 
             table(discursos_train[,"Parties"]), 
             table(discursos_test[,"Parties"]))
rownames(tbl) <- c("dataset", "training", "test")
head(tbl)


model <- cla_mlp("Parties", slevels, size=5,decay=0.03)
model <- fit(model, discursos_train)
train_prediction <- predict(model, discursos_train)


discursos_train_predictand <- adjust_class_label(discursos_train[,"Parties"])
train_eval <- evaluate(model, discursos_train_predictand, train_prediction)
print(train_eval$metrics)


# Test  
discursos_prediction <- predict(model, discursos_test)

discursos_test_predictand <- adjust_class_label(discursos_test[,"Parties"])
test_eval <- evaluate(model, discursos_test_predictand, discursos_prediction)
print(test_eval$metrics)




#-------------------------------------------------------------------------
#Questão 5
# Padrões frequentes observados manualmente:
# Na densidade, o PSL possui discursos mais breves
# Em analytic, o PRN possui pontuações maiores, e PSDB, PSL e PT possui menores
# Em tone, PSDB possui discursos mais neutros, e PSL os mais positivos
# Em clout, PSDB possui discursos menos influenciadores
# Em authentic, os discursos possuem poucos valores no geral, bem baixo. E também
# Diferenças em outliers.
# Em posemo, o PSL possui, no geral, discursos levemente mais positivos
# Em past focus, PRN possui menor foco no passado, e PSL varia mais


# Instalar e carregar o pacote 'arules'
install.packages("arules")
library(arules)
library(dplyr)
data_arules <- atributos_discretizados %>% select(-number_discretizado)
data_arules <- cbind(data_arules, data_novo$partido)
# Converter o dataset em transações
# Transformar o dataframe em um formato transacional
data_transacoes <- as(data_arules, "transactions")

# Executar o Apriori
rules <- apriori(data_transacoes, parameter = list(supp = 0.01, conf = 0.8))

# Filtrar as regras para incluir o partido no RHS
target_rules <- subset(rules, rhs %pin% "partido")

#100 primeiras pra não fritar CPU
top_rules <- head(target_rules, 100)
inspect(top_rules)