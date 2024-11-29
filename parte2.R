source("./library_handler.R")

load_libraries()
carregar_arquivos()


data <- read.csv("dados_para_pratica.csv")
data_novo <- separar_colunas_partido(data)
data_modelo <- reduzir_para_modelo(data_novo)


batch_mlp(data_modelo)