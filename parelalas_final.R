calcula_media <- function(data, col_inicio, col_fim) {
  # Validações básicas
  if (!is.data.frame(data)) {
    stop("O objeto fornecido não é um data.frame.")
  }
  if (col_inicio < 1 || col_fim > ncol(data) || col_inicio > col_fim) {
    stop("Intervalo de colunas inválido.")
  }
  
  # Seleção das colunas no intervalo
  subset_dados <- data[, col_inicio:col_fim]
  
  # Verificação se as colunas são numéricas
  if (!all(sapply(subset_dados, is.numeric))) {
    stop("Nem todas as colunas no intervalo são numéricas.")
  }
  
  # Cálculo das médias
  medias <- colMeans(subset_dados, na.rm = TRUE)
  
  return(medias)
}



agrupar_calcular_media <- function(data, coluna_grupo, col_inicio, col_fim) {
  # Validações básicas
  if (!is.data.frame(data)) {
    stop("O objeto fornecido não é um data.frame.")
  }
  if (!coluna_grupo %in% colnames(data)) {
    stop("A coluna de grupo fornecida não existe no data.frame.")
  }
  if (col_inicio < 1 || col_fim > ncol(data) || col_inicio > col_fim) {
    stop("Intervalo de colunas inválido.")
  }
  
  # Seleção das colunas numéricas no intervalo
  subset_dados <- data[, col_inicio:col_fim]
  if (!all(sapply(subset_dados, is.numeric))) {
    stop("Nem todas as colunas no intervalo são numéricas.")
  }
  
  # Agrupamento e cálculo das médias
  resultado <- data %>%
    group_by(!!sym(coluna_grupo)) %>%
    summarise(across(all_of(colnames(subset_dados)), ~ mean(.x, na.rm = TRUE)))
  
  return(resultado)
}

# Exemplo de uso
dados <- data.frame(
  mandato = c("Mandato1", "Mandato2", "Mandato1", "Mandato2", "Mandato1"),
  col1 = c(1, 2, 3, 4, 5),
  col2 = c(6, 7, 8, NA, 10),
  col3 = c(11, 12, 13, 14, 15)
)


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


renomear_colunas <- function(data) {
  # Verifica se o objeto fornecido é um data.frame
  if (!is.data.frame(data)) {
    stop("O objeto fornecido não é um data.frame.")
  }
  
  # Renomeia as colunas usando gsub para remover o ponto
  colnames(data) <- gsub("\\.(.*)$", "", colnames(data))
  
  return(data)
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


d_plot <- treino
entropias_plot <- calcular_entropia(d_plot,11,83,"mandato")
d_plot <- reordenar_colunas(d_plot,entropias_plot,11,83)
d_plot <- renomear_colunas(d_plot)






# NAO RODAR ATÉ O OUTRO COMENTARIO

library(data.table)


entropias <- data.frame(entropias_plot)
entropias <- transpose(entropias)




ordenar_colunas_por_entropia <- function(df, entropias) {
  # Converte a lista de entropias para um vetor numérico
  if (is.list(entropias)) {
    entropias <- sapply(entropias, function(x) x[[1]])
  }
  
  # Verifica se o número de colunas corresponde ao número de entropias fornecidas
  if (ncol(df) != length(entropias)) {
    stop("O número de colunas no data frame não corresponde ao número de entropias fornecidas.")
  }
  
  # Obter os nomes das colunas
  nomes_colunas <- colnames(df)
  
  # Criar uma tabela temporária para associar colunas e entropias
  tabela_temp <- data.frame(nome = nomes_colunas, entropia = entropias)
  
  # Ordenar a tabela pela entropia
  tabela_temp <- tabela_temp[order(tabela_temp$entropia), ]
  
  # Reorganizar o data frame com base na nova ordem
  df_ordenado <- df[, tabela_temp$nome]
  
  return(df_ordenado)
}




intervalo_colunas <- d_plot[, 11:83]  # Seleciona as colunas 3 a 6

intervalo_colunas<- ordenar_colunas_por_entropia(intervalo_colunas,entropias_plot)

coluna_para_mover <- d_plot[["mandato"]]

d_plot <- cbind(mandate = coluna_para_mover, intervalo_colunas)



d_plot <- ordenar_colunas_por_entropia(d_plot)


# COMENTARIO AQUI RODE A PARTIR DAQUI, ORDENACAO NA MAO


d_plt <- d_plot[, "mandato", drop = FALSE]
d_plt<- cbind(d_plt, d_plot[, "X20", drop = FALSE])
d_plt<- cbind(d_plt, d_plot[, "X14", drop = FALSE])
d_plt<- cbind(d_plt, d_plot[, "X3", drop = FALSE])
d_plt<- cbind(d_plt, d_plot[, "X70", drop = FALSE])
d_plt<- cbind(d_plt, d_plot[, "X28", drop = FALSE])
d_plt<- cbind(d_plt, d_plot[, "X30", drop = FALSE])
d_plt<- cbind(d_plt, d_plot[, "X50", drop = FALSE])
d_plt<- cbind(d_plt, d_plot[, "X4", drop = FALSE])
d_plt<- cbind(d_plt, d_plot[, "X39", drop = FALSE])
d_plt<- cbind(d_plt, d_plot[, "X6", drop = FALSE])
d_plt<- cbind(d_plt, d_plot[, "X31", drop = FALSE])
d_plt<- cbind(d_plt, d_plot[, "X5", drop = FALSE])
d_plt<- cbind(d_plt, d_plot[, "X13", drop = FALSE])
d_plt<- cbind(d_plt, d_plot[, "X2", drop = FALSE])
d_plt<- cbind(d_plt, d_plot[, "X49", drop = FALSE])
d_plt<- cbind(d_plt, d_plot[, "X19", drop = FALSE])
d_plt<- cbind(d_plt, d_plot[, "X37", drop = FALSE])
d_plt<- cbind(d_plt, d_plot[, "X38", drop = FALSE])
d_plt<- cbind(d_plt, d_plot[, "X29", drop = FALSE])
d_plt<- cbind(d_plt, d_plot[, "X43", drop = FALSE])
d_plt<- cbind(d_plt, d_plot[, "X66", drop = FALSE])
d_plt<- cbind(d_plt, d_plot[, "X1", drop = FALSE])
d_plt<- cbind(d_plt, d_plot[, "X61", drop = FALSE])
d_plt<- cbind(d_plt, d_plot[, "X15", drop = FALSE])
d_plt<- cbind(d_plt, d_plot[, "X", drop = FALSE])
d_plt<- cbind(d_plt, d_plot[, "X", drop = FALSE])
d_plt<- cbind(d_plt, d_plot[, "X", drop = FALSE])
d_plt<- cbind(d_plt, d_plot[, "X", drop = FALSE])
d_plt<- cbind(d_plt, d_plot[, "X", drop = FALSE])
d_plt<- cbind(d_plt, d_plot[, "X", drop = FALSE])
d_plt<- cbind(d_plt, d_plot[, "X", drop = FALSE])
d_plt<- cbind(d_plt, d_plot[, "X", drop = FALSE])
d_plt<- cbind(d_plt, d_plot[, "X", drop = FALSE])
d_plt<- cbind(d_plt, d_plot[, "X", drop = FALSE])
d_plt<- cbind(d_plt, d_plot[, "X", drop = FALSE])








# PLOT FINAL
colnames(d_plot)[colnames(d_plot) == "mandato"] <- "mandate"

grf <- ggparcoord(data = data_reordenado_plot, columns = c(2:74), group = 1) + 
  theme_bw(base_size = 20) + 
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size = 6), # Texto do eixo X na vertical
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position = "bottom", # Legenda abaixo do gráfico
    legend.box = "horizontal",  # Configura a legenda como horizontal
    legend.text = element_text(size = 7), # Ajusta o tamanho do texto na legenda
    legend.title = element_blank(),
    #legend.title = element_text(size = 7),  # Aumenta o tamanho do texto do nome da coluna do grupo
    legend.margin = margin(t = 0)  # Remove a margem superior da legenda
  ) +
  guides(color = guide_legend(nrow = 1)) +  # Limita a legenda a uma única linha
  font

#options(repr.plot.width = 20, repr.plot.height = 10)
#plot(grf)

ggsave("meu_grafico.png", plot = grf, width = 25, height = 10, unit = "cm")


options(repr.plot.width = 4, repr.plot.height = 4)




