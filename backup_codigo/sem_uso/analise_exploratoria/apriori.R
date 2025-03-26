gerar_top_rules <- function(atributos_discretizados, qtd_rules,data_novo, should_inspect = TRUE){
  data_arules <- atributos_discretizados %>% select(-number_discretizado)
  data_arules <- cbind(data_arules, data_novo$partido)

  
  # Transformar o dataframe em um formato transacional
  data_transacoes <- as(data_arules, "transactions")
  
  # Executar o Apriori
  rules <- apriori(data_transacoes, parameter = list(supp = 0.01, conf = 0.8))
  
  # Filtrar as regras para incluir o partido no RHS
  target_rules <- subset(rules, rhs %pin% "partido")
  
  #100 primeiras pra não fritar CPU
  top_rules <- head(target_rules, qtd_rules)
  if (should_inspect){
    inspect(top_rules)
  }
  return (top_rules)
}