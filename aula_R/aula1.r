library(tidyverse)

dados <- read.csv("Pokemon_full.csv")
dados$IMC <- dados$weight/dados$height^2

#glimpse(dados)

# selecionar colunas
select(dados, name, attack)

#filtrar dados
filter(dados, height > 10)

#criar colunas

mutate(dados, rate_atdef = attack/defense)

#comando pull
# difere do select, o retorno do select é uma coluna
#pull retorna um vetor
pull(dados, name) 


#resumir dados

summarise(dados, media_at = mean(attack), sd_at = sd(attack))

# teste - calcular a media do ataque e o desvio padrão
# apenas para pokemons com mais de 10 de altura

df <- filter(dados, height > 10)
summarise(df,media_at=mean(attack), sd_at=sd(attack))

#comando pipe -> encadeia comandos, exemplo do caso anterior

filter(dados,height>10) %>% summarise(media_at=mean(attack), sd_at=sd(attack))

# criar uma coluna que é a soma do ataque e da defesa

dados %>% 
    mutate(
        soma_ad = attack+defense,
        soma2 = height+weight
    ) %>%
    select(soma_ad, soma2)

# ---------------------------------------

dados %>%
    group_by(type) %>%
    summarise(media_at=mean(attack), sd_at=sd(attack))



dados %>%
    group_by(type) %>%
    mutate(media_at=mean(attack), sd_at=sd(attack)) %>%
    filter(attack>media_at) %>%
    select(-media_at)

x <- c(30,16,20,3)
f (x1)

#trabalha com vetores, se a função não suportar esse formato, pode usar rowwise()

#Aula2 01/09

