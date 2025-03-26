# Cálculo de entropia

data_smoothing <- treino

obj <- smoothing_freq(n = 10)  
obj <- fit(obj, data_smoothing$X1.function..Function.Words.)
sl.bi <- transform(obj, data_smoothing$X1.function..Function.Words.)
print(table(sl.bi))
obj$interval
entro <- evaluate(obj, as.factor(names(sl.bi)), data_smoothing$mandato)
print(entro$entropy)





renomear_colunas <- function(dataset, col_inicio, col_fim) {
  # Pega os nomes das colunas do dataset
  nomes_colunas <- colnames(dataset)
  
  # Remove o prefixo 'X' e espaços
  nomes_colunas <- gsub("^X", "", nomes_colunas)
  nomes_colunas <- gsub("\\s+", "", nomes_colunas)  # Remove espaços
  
  # Atualiza somente as colunas no intervalo especificado
  nomes_colunas[col_inicio:col_fim] <- paste0(
    "X", 
    sub("^([0-9]+).*", "\\1", nomes_colunas[col_inicio:col_fim])
  )
  
  # Atribui os novos nomes de volta ao dataset
  colnames(dataset) <- nomes_colunas
  return(dataset)
}


# COORDENADAS PARALELAS
library(RColorBrewer)

colors <- brewer.pal(100, 'Set1')
font <- theme(text = element_text(size=8))


data_plot <- renomear_colunas(treino,11,83)

grf <- ggparcoord(data = data_reordenado, columns = c(11:83), group=84) + 
  theme_bw(base_size = 10) + font + 
theme(axis.text.x = element_text(angle = 90, hjust = 1))

options(repr.plot.width = 10, repr.plot.height = 5) # Tamanho maior para melhorar a renderização
grDevices::png("grafico_alta_qualidade.png", width = 10, height = 5, units = "in", res = 300) # Exporta em alta resolução
plot(grf)
grDevices::dev.off() # Fecha o dispositivo gráfico




# Esta função calcula a entropia de cada coluna no intervalo especificado, e depois retorna uma lista com as entropias calculadas.
calcular_entropia <- function(data, col_inicio, col_fim, mandato_col, n = 10) {
  entropias <- list()
  for (col in col_inicio:col_fim) {
    coluna_atual <- data[[col]]
    obj <- smoothing_freq(n = n)
    obj <- fit(obj, coluna_atual)
    sl.bi <- transform(obj, coluna_atual)
    print(table(sl.bi))
    entro <- evaluate(obj, as.factor(names(sl.bi)), data[[mandato_col]])
    entropias[[colnames(data)[col]]] <- entro$entropy
  }
  return(entropias)
}

# Exemplo de uso:
# Supondo que data_smoothing seja seu dataset e "mandato" seja a variável de referência
entropias_resultado <- calcular_entropia(
  data = data_plot, 
  col_inicio = 11,  # Coluna inicial
  col_fim = 83,    # Coluna final
  mandato_col = "mandato"  # Nome da coluna de mandato
)

print(entropias_resultado)






reordenar_colunas <- function(data, entropias, col_inicio, col_fim) {
  # Extraindo os nomes das colunas no intervalo especificado
  colunas_intervalo <- colnames(data)[col_inicio:col_fim]
  
  # Filtrando as entropias relevantes e convertendo para vetor
  entropias_filtradas <- unlist(entropias[colunas_intervalo])
  
  # Ordenando os nomes das colunas pelo valor das entropias
  colunas_ordenadas <- names(sort(entropias_filtradas))
  
  # Criando a nova ordem do dataset
  colunas_antes <- colnames(data)[1:(col_inicio - 1)]  # Colunas antes do intervalo
  colunas_depois <- colnames(data)[(col_fim + 1):ncol(data)]  # Colunas depois do intervalo
  
  # Reordenando o dataset somente dentro do intervalo
  data_reordenado <- data[, c(colunas_antes, colunas_ordenadas, colunas_depois), drop = FALSE]
  
  return(data_reordenado)
}


# Exemplo de uso:
# Supondo que data_smoothing seja seu dataset
# E entropias_resultado contém as entropias calculadas
data_reordenado <- reordenar_colunas(
  data = data_plot,
  entropias = entropias_resultado,
  col_inicio =11,  # Coluna inicial do intervalo
  col_fim = 83     # Coluna final do intervalo
)







grf <- ggparcoord(data = data_reordenado_plot, columns = c(2:74), group = 1) + 
  theme_bw(base_size = 20) + 
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), # Texto do eixo X na vertical
    legend.position = "bottom", # Legenda abaixo do gráfico
    legend.box = "horizontal",  # Configura a legenda como horizontal
    legend.text = element_text(size = 10) # Ajusta o tamanho do texto na legenda
  ) +
  guides(color = guide_legend(nrow = 1)) +  # Limita a legenda a uma única linha
  font

options(repr.plot.width = 10, repr.plot.height = 5)
plot(grf)
options(repr.plot.width = 4, repr.plot.height = 4)









data_reordenado_plot <- agrupar_calcular_media(data_reordenado,"mandato",11,83)






dados_para_plot <- treino
sr <- sample_random()
sr <- train_test(sr, treino)
dados_para_plot <- sr$test
sr <- sample_random()
sr <- train_test(sr, dados_para_plot)
dados_para_plot <- sr$test
sr <- sample_random()
sr <- train_test(sr, dados_para_plot)
dados_para_plot <- sr$test

dados_para_plot <- reordenar_colunas(
  data = dados_para_plot,
  entropias = entropias_resultado,
  col_inicio =11,  # Coluna inicial do intervalo
  col_fim = 83     # Coluna final do intervalo
)






i <- sample(nrow(dados_para_plot))
dados_para_plot <- dados_para_plot[i,]

grf <- ggparcoord(data = dados_para_plot, columns = c(11:83), group=84) + 
  theme_bw(base_size = 10) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  font

options(repr.plot.width=10, repr.plot.height=5)
plot(grf)
options(repr.plot.width=4, repr.plot.height=4)




library(data.table)


entropias <- data.frame(entropias_resultado)
entropias <- transpose(entropias)


