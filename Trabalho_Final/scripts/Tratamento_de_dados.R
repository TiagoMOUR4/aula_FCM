
library(tidyverse)

dados <- read.csv("../arquivos_de_dados/bgg_dataset.csv", sep = ";", dec = ",", header = TRUE)

#glimpse(dados)
#View(summary(dados))

print(paste("Há", nrow(dados), "registros antes do processamento de dados"))

# Adotaremos as seguintes condições para clareza e coerência dos dados:
# 1) Os dados correspondem a jogos dos últimos 100 anos
# 2) A faixa de idade observada deve ser entre 1 e 18 anos de idade
# 3) Dados faltantes foram ignorados
# 4) A quantidade de jogadores está entre 1 e 100
# 5) Dados nulos são desconsiderados
# 6) Os jogos são avaliados por, no mínimo, 30 pessoas

# etapa 1 - Apenas jogos dos últimos 100 anos

dados_1 <- filter(dados, is.na(Year.Published) | Year.Published == 0 | (Year.Published > 1926 & Year.Published <= 2026))
print(paste("Há", nrow(dados_1), "registros depois da etapa 1"))

# etapa 2 - Faixa de idade entre 1 e 18 anos

dados_2 <- filter(dados_1, is.na(Min.Age) | Min.Age == 0 | Min.Age >= 1) %>%
  filter(is.na(Min.Age) | Min.Age <= 18)

print(paste("Há", nrow(dados_2), "registros depois da etapa 2"))

# etapa 3 - Desconsiderar valores ausentes, sem apagar os demais parâmetros

dados_3 <- dados_2 %>%
  mutate(across(where(is.character), ~ na_if(trimws(.x), "")))

ausentes <- colSums(is.na(dados_3))

cat(
  "Os dados ausentes, por categoria são:\n",
  paste(names(ausentes), ":", ausentes, collapse = "\n")
)

cat("\n")
print(paste("Há", nrow(dados_3), "registros depois da etapa 3"))

# etapa 4 - A quantidade de jogadores está entre 1 e 100

dados_4 <- filter(dados_3, is.na(Min.Players) | Min.Players == 0 | (Min.Players >= 1 & Min.Players <= 100)) %>%
  filter(is.na(Max.Players) | Max.Players == 0 | (Max.Players >= 1 & Max.Players <= 100))

print(paste("Há", nrow(dados_4), "registros depois da etapa 4"))

# etapa 5 - Dados nulos (iguais a zero) são desconsiderados

dados_5 <- dados_4 %>%
  mutate(across(where(is.numeric), ~ na_if(.x, 0)))

print(paste("Há", nrow(dados_5), "registros depois da etapa 5"))

# etapa 6 - Os jogos são avaliados por pelo menos 30 pessoas
# Como a autora já fez esse filtro, não há necessidade de implementar funções extras

cat("Jogos com menos de 30 avaliações:",
    sum(dados_6$Users.Rated < 30, na.rm = TRUE), "\n")
dados_6 <- dados_5


# Avaliação das comdições: 
cat("\n=== VALIDAÇÃO FINAL ===\n")

cat("Registros originais:", nrow(dados), "\n")
cat("Registros finais:", nrow(dados_6), "\n")
cat("Registros excluídos:", nrow(dados) - nrow(dados_6), "\n")
cat("Valores ausentes:", sum(is.na(dados_6)), "\n")

# Etapa 7 - Exportação dos dados tratados para CSV

write.csv2(
  dados_6,
  "../arquivos_de_dados/bgg_dataset_tratado.csv"
)

print("Dados tratados exportados com sucesso!")