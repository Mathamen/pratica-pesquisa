source("./library_handler.R") # Auxilia com o load das funções
load_libraries()
carregar_arquivos()



# TODO organizar melhor as coisas, como nomes de variáveis
data <- read.csv("dados_para_pratica.csv")
data <- separar_colunas_partido(data)
data <- reduzir_para_modelo(data)

data_com_partido <- separar_colunas_partido(data)


batch_classificador(data,"mlp")




batch_classificador(data_com_partido,"mlp",TRUE)




data_fase2 <- read.csv("dados_valorados.csv")
data_fase2 <- adicionar_coluna_mandato(data_fase2)
#data_fase2 <- separar_colunas_partido(data_fase2)
batch_classificador(data_fase2,"mlp")





























# LEGADO
generate_parallel_coordinates <- function(data, lower_limit, upper_limit, color_column_index = NULL, output_file = "parallel_coordinates.png", dpi = 300, width = 12, height = 8) {
  # Validar os limites
  if (lower_limit < 1 || upper_limit > ncol(data) || lower_limit > upper_limit) {
    stop("Os limites especificados são inválidos.")
  }
  
  # Selecionar as colunas entre os limites
  selected_data <- data[, lower_limit:upper_limit]
  
  # Se o índice de cor for fornecido, adicionar a coluna de cor ao conjunto de dados
  if (!is.null(color_column_index) && color_column_index >= 1 && color_column_index <= ncol(data)) {
    selected_data$Color <- factor(data[[color_column_index]])  # Converter para fator para tratamento adequado de cores
  } else if (!is.null(color_column_index)) {
    stop("O índice especificado para cor é inválido.")
  }
  
  # Gerar o gráfico com a coluna de cor corretamente configurada
  plot <- ggparcoord(
    data = selected_data,
    columns = 1:(ncol(selected_data) - ifelse(!is.null(color_column_index), 1, 0)), # Exclui a coluna de cor do gráfico
    scale = "globalminmax", # Escalar entre o mínimo e o máximo global
    groupColumn = "Color",  # Definir a coluna de agrupamento como a coluna de cor
    title = "Parallel Coordinates Plot"
  ) +
    scale_color_manual(values = RColorBrewer::brewer.pal(length(unique(selected_data$Color)), "Set3")) +  # Paleta de cores
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      axis.text.x = element_text(angle = 90, hjust = 1),
      panel.background = element_rect(fill = "white", color = "white"),  # Fundo branco
      plot.background = element_rect(fill = "white", color = "white")   # Fundo do gráfico branco
    )
  
  # Salvar o gráfico em alta resolução
  ggsave(
    filename = output_file,
    plot = plot,
    dpi = dpi,
    width = width,
    height = height
  )
  
  message("Gráfico salvo em ", output_file)
}


generate_parallel_coordinates(data_fase2, lower_limit = 11, upper_limit = 83, color_column_index = 84, output_file = "parallel_coordinates_col84.png", dpi = 2400, width = 64, height = 40)




generate_parallel_coordinates2 <- function(data, lower_limit, upper_limit, color_column_index = NULL, output_file = "parallel_coordinates.png", dpi = 300, row_start = 1, row_end = nrow(data)) {
  # Validar os limites de colunas
  if (lower_limit < 1 || upper_limit > ncol(data) || lower_limit > upper_limit) {
    stop("Os limites especificados para as colunas são inválidos.")
  }
  
  # Validar os limites de linhas
  if (row_start < 1 || row_end > nrow(data) || row_start > row_end) {
    stop("Os limites especificados para as linhas são inválidos.")
  }
  
  # Selecionar as linhas dentro do intervalo especificado
  data <- data[row_start:row_end, ]
  
  # Selecionar as colunas entre os limites
  selected_data <- data[, lower_limit:upper_limit]
  
  # Se o índice de cor for fornecido, adicionar a coluna de cor ao conjunto de dados
  if (!is.null(color_column_index) && color_column_index >= 1 && color_column_index <= ncol(data)) {
    selected_data$Color <- factor(data[[color_column_index]])  # Converter para fator para tratamento adequado de cores
  } else if (!is.null(color_column_index)) {
    stop("O índice especificado para cor é inválido.")
  }
  
  # Gerar o gráfico com a coluna de cor corretamente configurada
  plot <- ggparcoord(
    data = selected_data,
    columns = 1:(ncol(selected_data) - ifelse(!is.null(color_column_index), 1, 0)), # Exclui a coluna de cor do gráfico
    scale = "globalminmax", # Escalar entre o mínimo e o máximo global
    groupColumn = "Color",  # Definir a coluna de agrupamento como a coluna de cor
    title = "Parallel Coordinates Plot"
  ) +
    scale_color_manual(values = RColorBrewer::brewer.pal(length(unique(selected_data$Color)), "Set3")) +  # Paleta de cores
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      axis.text.x = element_text(angle = 90, hjust = 1),
      panel.background = element_rect(fill = "white", color = "white"),  # Fundo branco
      plot.background = element_rect(fill = "white", color = "white")   # Fundo do gráfico branco
    ) +
    # Definir a espessura das linhas (quanto menor o valor, mais fina será a linha)
    geom_line(linewidth = 0.05)  # Ajuste aqui para deixar as linhas mais finas
  
  # Salvar o gráfico em alta resolução com tamanho ajustado
  ggsave(
    filename = output_file,
    plot = plot,
    dpi = dpi, 
    width = 21,  # Ajuste da largura
    height = 7, # Proporção retangular
    limitsize = FALSE
  )
  
  message("Gráfico salvo em ", output_file)
}




















generate_parallel_coordinates2(data_fase2, lower_limit = 11, upper_limit = 83, color_column_index = 84, output_file = "parallel_coordinates_partido_sem_limite.png", dpi = 1200)





generate_parallel_coordinates2(
  data = data_fase2,
  lower_limit = 11,
  upper_limit = 83,
  color_column_index = 84,
  output_file = "parallel_coordinates_limited.png",
  dpi = 1200,
  row_start = 1,
  row_end = 1000
)






calculate_batch_means_with_limits <- function(data, lower_limit, upper_limit, mandato_column, batch_size = 10) {
  # Validar os limites
  if (lower_limit < 1 || upper_limit > ncol(data) || lower_limit > upper_limit) {
    stop("Os limites especificados para as colunas são inválidos.")
  }
  
  # Validar se a coluna de mandato está presente
  if (!mandato_column %in% colnames(data)) {
    stop("A coluna de mandato especificada não está presente no dataset.")
  }
  
  # Selecionar as colunas dentro dos limites
  columns_to_average <- colnames(data)[lower_limit:upper_limit]
  
  # Inicializar lista para armazenar resultados
  results <- list()
  
  # Separar o dataset por mandato
  mandatos <- unique(data[[mandato_column]])
  
  for (mandato in mandatos) {
    # Filtrar dados do mandato atual
    mandato_data <- data[data[[mandato_column]] == mandato, ]
    
    # Processar em lotes de batch_size
    num_rows <- nrow(mandato_data)
    for (start_row in seq(1, num_rows, by = batch_size)) {
      # Determinar o intervalo de linhas
      end_row <- min(start_row + batch_size - 1, num_rows)
      
      # Selecionar os dados do lote
      batch <- mandato_data[start_row:end_row, ]
      
      # Calcular a média para as colunas especificadas
      batch_means <- colMeans(batch[, columns_to_average, drop = FALSE], na.rm = TRUE)
      
      # Adicionar a coluna de mandato ao resultado
      batch_result <- c(batch_means, mandato = mandato)
      
      # Criar um data frame para garantir que a estrutura de colunas seja mantida
      batch_df <- as.data.frame(t(batch_result))
      colnames(batch_df) <- c(columns_to_average, "mandato")
      
      # Adicionar ao resultado final
      results <- append(results, list(batch_df))
    }
  }
  
  # Combinar todos os resultados em um único data frame
  result_df <- do.call(rbind, results)
  rownames(result_df) <- NULL
  return(result_df)
}

result <- calculate_batch_means_with_limits(
  data = data_fase2,
  lower_limit = 11,    # Seleciona colunas de índice 2 a 3
  upper_limit = 83,
  mandato_column = "mandato",
  batch_size = 25
)




















generate_parallel_coordinates3 <- function(data, lower_limit, upper_limit, color_column_index = NULL, output_file = "parallel_coordinates.png", dpi = 300) {
  # Validar os limites de colunas
  if (lower_limit < 1 || upper_limit > ncol(data) || lower_limit > upper_limit) {
    stop("Os limites especificados para as colunas são inválidos.")
  }
  
  # Selecionar as colunas entre os limites
  selected_data <- data[, lower_limit:upper_limit]
  
  # Se o índice de cor for fornecido, adicionar a coluna de cor ao conjunto de dados
  if (!is.null(color_column_index) && color_column_index >= 1 && color_column_index <= ncol(data)) {
    selected_data$Color <- factor(data[[color_column_index]])  # Converter para fator para tratamento adequado de cores
  } else if (!is.null(color_column_index)) {
    stop("O índice especificado para cor é inválido.")
  }
  
  # Gerar o gráfico com a coluna de cor corretamente configurada
  plot <- ggparcoord(
    data = selected_data,
    columns = 1:(ncol(selected_data) - ifelse(!is.null(color_column_index), 1, 0)), # Exclui a coluna de cor do gráfico
    scale = "globalminmax", # Escalar entre o mínimo e o máximo global
    groupColumn = "Color",  # Definir a coluna de agrupamento como a coluna de cor
    title = "Parallel Coordinates Plot"
  ) +
    scale_color_manual(values = RColorBrewer::brewer.pal(length(unique(selected_data$Color)), "Set3")) +  # Paleta de cores
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      axis.text.x = element_text(angle = 90, hjust = 1),
      panel.background = element_rect(fill = "white", color = "white"),  # Fundo branco
      plot.background = element_rect(fill = "white", color = "white")   # Fundo do gráfico branco
    ) +
    # Definir a espessura das linhas (quanto menor o valor, mais fina será a linha)
    geom_line(linewidth = 0.05)  # Ajuste aqui para deixar as linhas mais finas
  
  # Salvar o gráfico em alta resolução com tamanho ajustado
  ggsave(
    filename = output_file,
    plot = plot,
    dpi = dpi, 
    width = 21,  # Ajuste da largura
    height = 7,  # Proporção retangular
    limitsize = FALSE
  )
  
  message("Gráfico salvo em ", output_file)
}










generate_parallel_coordinates3(
  data = result,
  lower_limit = 1,
  upper_limit = 11,
  color_column_index = 74,
  output_file = "dividido.png",
  dpi = 1200
)

batch_classificador_mandato_novo(data_fase2,"mlp",treino_teste = treino_teste)

