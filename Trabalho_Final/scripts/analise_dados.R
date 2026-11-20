
# Explorar dados
library(tidyverse)

dados <- read.csv("../arquivos_de_dados/bgg_dataset_tratado.csv", sep = ";", dec = ",", header = TRUE)
dados_brutos <- read.csv("../arquivos_de_dados/bgg_dataset.csv", sep = ";", dec = ",", header = TRUE)

#glimpse(dados)
#View(dados)

# Estilo dos gráficos
tema <- theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11, margin = margin(b = 15)),
    axis.title = element_text(size = 11),
    axis.line = element_line(linewidth = 0.3),
    axis.ticks = element_blank(),
    plot.margin = margin(15, 20, 15, 15)
  )


#######################################################
# Vendo anos com a maior quantidade de dados

freq_YP <- table(dados$Year.Published)
print(paste("A frequência máxima foi de", max(freq_YP), "em",
            names(freq_YP)[which.max(freq_YP)]))

# Gráfico do top 10
top10 <- head(sort(freq_YP, decreasing = TRUE), 10)
top10 <- as.data.frame(top10)

gr_top10_freq <- ggplot(top10, aes(x = reorder(Var1, Freq), y = Freq)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = Freq), hjust = -0.3, size = 3.7) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.13))) +
  labs(
    title = "Publicações por ano",
    subtitle = "Os 10 anos com maior número de jogos",
    x = "Ano de publicação",
    y = "Número de jogos"
  ) +
  tema

print(gr_top10_freq)

ggsave("../documentos/frequencia_jogos_top10_ano.png",
       plot = gr_top10_freq, width = 9, height = 6, dpi = 300)


#######################################################
# Classificação etária dos jogos

freq_min_age <- table(dados$Min.Age)
top10_min_age <- head(sort(freq_min_age, decreasing = TRUE), 10)
top10_min_age <- as.data.frame(top10_min_age)

# Organizar as idades numericamente
top10_min_age$Var1 <- as.numeric(as.character(top10_min_age$Var1))

gr_top10_min_age <- ggplot(top10_min_age,
                           aes(x = factor(Var1, levels = sort(Var1)), y = Freq)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = Freq), vjust = -0.5, size = 3.7) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.13))) +
  labs(
    title = "Classificação etária",
    subtitle = "As 10 idades mínimas mais frequentes",
    x = "Idade mínima (anos)",
    y = "Número de jogos"
  ) +
  tema

print(gr_top10_min_age)

ggsave("../documentos/frequencia_top10_min_age.png",
       plot = gr_top10_min_age, width = 9, height = 6, dpi = 300)


#######################################################
# As idades mais frequentes mudaram nos últimos anos?

perfil_idade_5_anos <- filter(dados, Year.Published >= 2016)

freq_min_age_5 <- table(perfil_idade_5_anos$Min.Age)
top10_min_age_5 <- head(sort(freq_min_age_5, decreasing = TRUE), 10)
top10_min_age_5 <- as.data.frame(top10_min_age_5)

top10_min_age_5$Var1 <- as.numeric(as.character(top10_min_age_5$Var1))

gr_top10_min_age_5 <- ggplot(top10_min_age_5,
                             aes(x = factor(Var1, levels = sort(Var1)), y = Freq)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = Freq), vjust = -0.5, size = 3.7) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.13))) +
  labs(
    title = "Classificação etária",
    subtitle = "Jogos publicados a partir de 2016",
    x = "Idade mínima (anos)",
    y = "Número de jogos"
  ) +
  tema

print(gr_top10_min_age_5)

ggsave("../documentos/frequencia_top10_min_age_2016.png",
       plot = gr_top10_min_age_5, width = 9, height = 6, dpi = 300)


#######################################################
# Complexidades mais frequentes

complexidade <- dados$Complexity.Average
top10_complexidade <- head(sort(table(complexidade), decreasing = TRUE), 10)

print(top10_complexidade)


#######################################################
# Distribuição da complexidade dos jogos

# Classificar os jogos por nível de complexidade
faixas_complexidade <- cut(complexidade,
                          breaks = c(1, 2, 3.5, 5),
                          labels = c("Fácil", "Médio", "Difícil"),
                          include.lowest = TRUE)

freq_complexidade <- as.data.frame(table(faixas_complexidade))

# Gráfico da distribuição de complexidade
gr_complexidade <- ggplot(freq_complexidade,
                          aes(x = faixas_complexidade, y = Freq)) +
  geom_col(width = 0.55) +
  geom_text(aes(label = Freq), vjust = -0.5, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "Complexidade dos jogos",
    x = "Nível de complexidade",
    y = "Quantidade de jogos"
  ) +
  tema +
  theme(axis.line.y = element_blank())

print(gr_complexidade)

ggsave("../documentos/distribuicao_complexidade.png",
       plot = gr_complexidade, width = 9, height = 6, dpi = 300)


#######################################################
# Complexo é sinônimo de mais demorado?
# Verificar se jogos mais complexos são mais longos

dados_complexidade <- dados %>%
  filter(!is.na(Complexity.Average), !is.na(Play.Time),
         between(Complexity.Average, 1, 5), Play.Time > 0) %>%
  mutate(Nivel = cut(Complexity.Average,
                     breaks = c(1, 2, 3.5, 5),
                     labels = c("Fácil", "Médio", "Difícil"),
                     include.lowest = TRUE))

# Estatísticas por nível de complexidade
tempo_complexidade <- dados_complexidade %>%
  group_by(Nivel) %>%
  summarise(
    Jogos = n(),
    Q1 = quantile(Play.Time, 0.25),
    Mediana = median(Play.Time),
    Q3 = quantile(Play.Time, 0.75),
    Mais_2h = round(mean(Play.Time > 120) * 100, 1),
    .groups = "drop"
  )

print(tempo_complexidade)


#######################################################
# Gráfico da duração mediana por complexidade

gr_tempo_complexidade <- ggplot(tempo_complexidade,
                                aes(x = Nivel, y = Mediana)) +
  geom_col(width = 0.55) +
  geom_text(aes(label = paste0(round(Mediana), " min")),
            vjust = -0.6, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Duração por nível de complexidade",
    x = "Complexidade",
    y = "Duração mediana (min)"
  ) +
  tema +
  theme(axis.line.y = element_blank())

print(gr_tempo_complexidade)

ggsave("../documentos/tempo_complexidade.png",
       plot = gr_tempo_complexidade, width = 9, height = 6, dpi = 300)


#######################################################
# Conferindo a informação
# Distribuição da duração por nível de complexidade

gr_distribuicao_tempo <- ggplot(tempo_complexidade,
                                aes(x = Nivel, y = Mediana)) +
  geom_linerange(aes(ymin = Q1, ymax = Q3), linewidth = 1.5) +
  geom_point(shape = 21, fill = "white", size = 4, stroke = 1.2) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0.04, 0.08))) +
  labs(
    title = "Distribuição da duração por complexidade",
    x = "Complexidade",
    y = "Duração da partida (min)"
  ) +
  tema +
  theme(axis.line.y = element_blank())

print(gr_distribuicao_tempo)

ggsave("../documentos/distribuicao_tempo_complexidade.png",
       plot = gr_distribuicao_tempo, width = 9, height = 5, dpi = 300)


#######################################################
# Os jogos se tornaram mais coletivos ou seletos?
# Comparação do número mínimo e máximo de jogadores

dados_jogadores <- dados %>%
  filter(!is.na(Year.Published), !is.na(Min.Players),
         !is.na(Max.Players), Min.Players >= 1,
         Max.Players >= Min.Players, Max.Players <= 100) %>%
  mutate(Periodo = if_else(Year.Published >= 2011 &
                            Year.Published <= 2020,
                          "2011–2020", "Demais anos"))

# Organizar os dados
freq_jogadores <- dados_jogadores %>%
  pivot_longer(cols = c(Min.Players, Max.Players),
               names_to = "Tipo", values_to = "Jogadores") %>%
  mutate(
    Tipo = recode(Tipo, "Min.Players" = "Mínimo",
                  "Max.Players" = "Máximo"),
    Jogadores = factor(if_else(Jogadores >= 10, "10+",
                              as.character(Jogadores)),
                      levels = c(as.character(1:9), "10+"))
  ) %>%
  count(Periodo, Tipo, Jogadores) %>%
  group_by(Periodo, Tipo) %>%
  mutate(Percentual = n / sum(n) * 100) %>%
  ungroup()

# Separar os períodos
jogadores_10_anos <- filter(freq_jogadores, Periodo == "2011–2020")
jogadores_anteriores <- filter(freq_jogadores, Periodo == "Demais anos")

# Mesma escala nos dois gráficos
limite <- max(freq_jogadores$Percentual) * 1.1


#######################################################
# Distribuição dos limites de participantes entre 2011 e 2020

gr_jogadores_10_anos <- ggplot(jogadores_10_anos,
                               aes(x = Jogadores, y = Percentual, fill = Tipo)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_grey() +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous(limits = c(0, limite)) +
  labs(
    title = "Distribuição do número mínimo e máximo de jogadores",
    subtitle = "Jogos publicados entre 2011 e 2020",
    x = "Número de participantes permitidos",
    y = "Proporção de jogos (%)",
    fill = NULL
  ) +
  tema +
  theme(legend.position = "top")

print(gr_jogadores_10_anos)

ggsave("../documentos/distribuicao_min_max_jogadores_2011_2020.png",
       plot = gr_jogadores_10_anos, width = 9, height = 6, dpi = 300)


#######################################################
# Distribuição dos limites de participantes nos demais anos

gr_jogadores_anteriores <- ggplot(jogadores_anteriores,
                                  aes(x = Jogadores, y = Percentual, fill = Tipo)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_grey() +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous(limits = c(0, limite)) +
  labs(
    title = "Distribuição do número mínimo e máximo de jogadores",
    subtitle = "Jogos publicados fora do período 2011–2020",
    x = "Número de participantes permitidos",
    y = "Proporção de jogos (%)",
    fill = NULL
  ) +
  tema +
  theme(legend.position = "top")

print(gr_jogadores_anteriores)

ggsave("../documentos/distribuicao_min_max_jogadores_demais_anos.png",
       plot = gr_jogadores_anteriores, width = 9, height = 6, dpi = 300)
