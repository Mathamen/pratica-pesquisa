load_libraries <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package, dependencies = TRUE) # Instala se não estiver presente
      library(package, character.only = TRUE) # Carrega após instalar
    }
  }
}