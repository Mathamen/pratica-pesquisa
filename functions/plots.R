



criar_scatter_date <- function(data_novo) {
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


criar_density_plot <- function(data_novo) {
  print(ggplot(data_novo, aes(x = date, fill = partido)) +
          geom_density(alpha = 0.5) + 
          labs(title = "Gráfico de Densidade por Classe", x = "Date", y = "Densidade") +
          theme_minimal())
  
  print(ggplot(data_novo, aes(x = length, fill = partido)) +
          geom_density(alpha = 0.5) + 
          labs(title = "Gráfico de Densidade por Classe", x = "Length", y = "Densidade") +
          theme_minimal())
  
  print(ggplot(data_novo, aes(x = Analytic, fill = partido)) +
          geom_density(alpha = 0.5) + 
          labs(title = "Gráfico de Densidade por Classe", x = "Analytic", y = "Densidade") +
          theme_minimal())
  
  print(ggplot(data_novo, aes(x = Tone, fill = partido)) +
          geom_density(alpha = 0.5) + 
          labs(title = "Gráfico de Densidade por Classe", x = "Tone", y = "Densidade") +
          theme_minimal())
  
  print(ggplot(data_novo, aes(x = Clout, fill = partido)) +
          geom_density(alpha = 0.5) + 
          labs(title = "Gráfico de Densidade por Classe", x = "Clout", y = "Densidade") +
          theme_minimal())
  
  print(ggplot(data_novo, aes(x = Authentic, fill = partido)) +
          geom_density(alpha = 0.5) + 
          labs(title = "Gráfico de Densidade por Classe", x = "Authentic", y = "Densidade") +
          theme_minimal())
  
  print(ggplot(data_novo, aes(x = Posemo, fill = partido)) +
          geom_density(alpha = 0.5) + 
          labs(title = "Gráfico de Densidade por Classe", x = "Posemo", y = "Densidade") +
          theme_minimal())
  
  print(ggplot(data_novo, aes(x = Negemo, fill = partido)) +
          geom_density(alpha = 0.5) + 
          labs(title = "Gráfico de Densidade por Classe", x = "Negemo", y = "Densidade") +
          theme_minimal())
  
  print(ggplot(data_novo, aes(x = Past.Focus, fill = partido)) +
          geom_density(alpha = 0.5) + 
          labs(title = "Gráfico de Densidade por Classe", x = "Past Focus", y = "Densidade") +
          theme_minimal())
  
  print(ggplot(data_novo, aes(x = Present.Focus, fill = partido)) +
          geom_density(alpha = 0.5) + 
          labs(title = "Gráfico de Densidade por Classe", x = "Present Focus", y = "Densidade") +
          theme_minimal())
  
  print(ggplot(data_novo, aes(x = Future.Focus, fill = partido)) +
          geom_density(alpha = 0.5) + 
          labs(title = "Gráfico de Densidade por Classe", x = "Future Focus", y = "Densidade") +
          theme_minimal())
  
  print(ggplot(data_novo, aes(x = Compare, fill = partido)) +
          geom_density(alpha = 0.5) + 
          labs(title = "Gráfico de Densidade por Classe", x = "Compare", y = "Densidade") +
          theme_minimal())
}