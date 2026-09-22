library(tidyverse)

dados_clima <- read.csv("./tabela_clima.csv", sep=";")

glimpse(dados_clima)

dados_clima$data <- as.Date(dados_clima$data, format = "%d-%m-%Y")

ggplot(dados_clima, aes(x = data, y = Pluviosidade)) +
  geom_point()+ ggsave()
