library(tidyverse)

dados <- read.csv("../arquivos_de_dados/bgg_dataset.csv",sep = ";",dec = ",",header = TRUE)

glimpse(dados)
View(summary(dados))

colunas_num <- names(dados)[sapply(dados, is.numeric)]

negativos <- filter(dados, if_any(all_of(colunas_num), ~ .x < 0))

View(negativos)
