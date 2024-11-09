source("./library_handler.R") # Atualize o caminho conforme necessário

# Lista de pacotes a serem carregados
packages <- c("dplyr", "ggplot2", "gridExtra", "tidyr", "arules")
# Carregar os pacotes
load_libraries(packages) # handler das libraries, para melhorar o código
rm(packages)

# Arquivos separados, só usar o source aqui
source("./functions/plots.R")
source("./functions/classify.R")
source("./functions/metricas_globais.R")
source("./functions/metricas_partidarias.R")
source("./functions/apriori.R")

# DAL TOOLBOX
source("https://raw.githubusercontent.com/cefet-rj-dal/daltoolbox/main/jupyter.R")
load_library("daltoolbox")

# Carregar o arquivo CSV
data <- read.csv("dados_para_pratica.csv")


#métricas globais (média e desvio padrão)
metricas_globais <- gerar_metricas_globais(data)

# Por partido agora
metricas_partidarias <- gerar_metricas_partidarias(data)



# Para obter o data_novo, rode a parte do código que gera ele
# Gráficos de densidade para cada atributo
criar_density_plot(data_novo)
# Scatter plot de todos os atributos
criar_scatter_date(data_novo)




#-------------------------------------------------------------
# Criação dos gráficos box-plot e dispersão
criar_graficos_partido(metricas_partidarias$PMDB, "PMDB")
criar_graficos_partido(metricas_partidarias$PRN, "PRN")
criar_graficos_partido(metricas_partidarias$PSL, "PSL")
criar_graficos_partido(metricas_partidarias$PSDB, "PSDB")
criar_graficos_partido(metricas_partidarias$PT, "PT")


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

model <- gerar_modelo_kmeans(data, data_novo)

#-----------------------------------------------------------------------
#Questão 4

data_novo$partido <- factor(data_novo$partido)
cz <-data_novo %>% select(-date)

gerar_modelo_mlp(cz)


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

gerar_top_rules(atributos_discretizados,100,data_novo)
