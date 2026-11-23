# ==========================================
# TRABALHO FINAL FCM 2026
# Matheus Wesley Rodrigues
# Hipoteses 7 e 8
# ==========================================

# 1. Carregar a base de dados
dados <- read.csv2(
  "arquivos_de_dados/bgg_dataset_tratado.csv",
  stringsAsFactors = FALSE
)

# 2. Visualizar os primeiros registros
head(dados)

# 3. Verificar quantidade de linhas e colunas
dim(dados)

# 4. Verificar nomes das variaveis
names(dados)

# 5. Verificar estrutura da base
str(dados)

# 6. Resumo estatistico
summary(dados)

# ==========================================
# 7. VERIFICACAO DOS ANOS DE PUBLICACAO
# ==========================================

summary(dados$Year.Published)

# Quantidade de anos ausentes
sum(is.na(dados$Year.Published))

# Menor e maior ano de publicacao
min(dados$Year.Published, na.rm = TRUE)

max(dados$Year.Published, na.rm = TRUE)


# ==========================================
# 8. INVESTIGACAO DE VALORES EXTREMOS
# ==========================================

# Jogos com duracao superior a 1000 minutos
subset(dados, Play.Time > 1000,
       select = c(Name, Year.Published, Play.Time))

# Jogos com mais de 20 participantes
subset(dados, Max.Players > 20,
       select = c(Name, Year.Published, Max.Players))

# ==========================================
# ANALISE DOS JOGADORES
# ==========================================

# Quantidade de jogos com mais de 20 jogadores
sum(dados$Max.Players > 20, na.rm = TRUE)

# Quantidade de jogos com exatamente 99 jogadores
sum(dados$Max.Players == 99, na.rm = TRUE)

# Quantidade de jogos com exatamente 100 jogadores
sum(dados$Max.Players == 100, na.rm = TRUE)

# Frequencia dos valores de jogadores
sort(table(dados$Max.Players), decreasing = TRUE)[1:15]

# Investigar jogos com maximo de 99 jogadores
jogos_99 <- subset(
  dados,
  Max.Players == 99,
  select = c(Name, Year.Published, Min.Players, Max.Players)
)

head(jogos_99, 20)

# ==========================================
# HIPOTESE 7 - EVOLUCAO DOS JOGOS
# ==========================================

# Criar base de trabalho
dados_7 <- dados

# Remover anos ausentes
dados_7 <- dados_7[
  !is.na(dados_7$Year.Published), ]

# Agrupar por decadas
dados_7$Decada <- floor(
  dados_7$Year.Published / 10
) * 10

# Calcular estatisticas por decada
evolucao_jogadores <- aggregate(
  cbind(Min.Players, Max.Players) ~ Decada,
  data = dados_7,
  FUN = median,
  na.rm = TRUE
)


print(evolucao_jogadores)
# Quantidade de jogos publicados por década

quantidade_decada <- as.data.frame(
  table(dados_7$Decada)
)

names(quantidade_decada) <- c("Decada", "Quantidade")

quantidade_decada$Decada <- as.numeric(
  as.character(quantidade_decada$Decada)
)

print(quantidade_decada)

# Investigar valores extremos de Max.Players

table(dados$Max.Players, useNA = "ifany")[
  names(table(dados$Max.Players, useNA = "ifany")) %in%
    c("99", "100")
]# Investigar valores extremos de Max.Players

table(dados$Max.Players, useNA = "ifany")[
  names(table(dados$Max.Players, useNA = "ifany")) %in%
    c("99", "100")
]
# Evolução da popularidade, complexidade e duração por década

# Popularidade: número de avaliações
mediana_avaliacoes <- aggregate(
  Users.Rated ~ Decada,
  data = dados_7,
  FUN = median,
  na.rm = TRUE
)

n_avaliacoes <- aggregate(
  Users.Rated ~ Decada,
  data = dados_7,
  FUN = function(x) sum(!is.na(x))
)

# Complexidade
mediana_complexidade <- aggregate(
  Complexity.Average ~ Decada,
  data = dados_7,
  FUN = median,
  na.rm = TRUE
)

n_complexidade <- aggregate(
  Complexity.Average ~ Decada,
  data = dados_7,
  FUN = function(x) sum(!is.na(x))
)

# Duração dos jogos
mediana_duracao <- aggregate(
  Play.Time ~ Decada,
  data = dados_7,
  FUN = median,
  na.rm = TRUE
)

n_duracao <- aggregate(
  Play.Time ~ Decada,
  data = dados_7,
  FUN = function(x) sum(!is.na(x))
)

# Organizar os resultados
resumo_evolucao <- Reduce(
  function(x, y) merge(x, y, by = "Decada"),
  list(
    mediana_avaliacoes,
    n_avaliacoes,
    mediana_complexidade,
    n_complexidade,
    mediana_duracao,
    n_duracao
  )
)

names(resumo_evolucao) <- c(
  "Decada",
  "Mediana_Avaliacoes",
  "N_Avaliacoes",
  "Mediana_Complexidade",
  "N_Complexidade",
  "Mediana_Duracao",
  "N_Duracao"
)

print(resumo_evolucao)

# Gráficos da Hipótese 7

par(mfrow = c(2, 2))

# 1. Evolução do número máximo de jogadores
plot(
  evolucao_jogadores$Decada,
  evolucao_jogadores$Max.Players,
  type = "b",
  xlab = "Década",
  ylab = "Mediana de jogadores",
  main = "Número máximo de jogadores"
)

# 2. Evolução da popularidade
plot(
  resumo_evolucao$Decada,
  resumo_evolucao$Mediana_Avaliacoes,
  type = "b",
  xlab = "Década",
  ylab = "Mediana de avaliações",
  main = "Popularidade dos jogos"
)

# 3. Evolução da complexidade
plot(
  resumo_evolucao$Decada,
  resumo_evolucao$Mediana_Complexidade,
  type = "b",
  xlab = "Década",
  ylab = "Mediana da complexidade",
  main = "Complexidade dos jogos"
)

# 4. Evolução da duração
plot(
  resumo_evolucao$Decada,
  resumo_evolucao$Mediana_Duracao,
  type = "b",
  xlab = "Década",
  ylab = "Mediana da duração (minutos)",
  main = "Duração dos jogos"
)

par(mfrow = c(1, 1))

# Testes de correlação de Spearman - Hipótese 7

testar_spearman <- function(variavel, nome) {

  dados_teste <- dados_7[
    complete.cases(dados_7[, c("Year.Published", variavel)]),
  ]

  resultado <- cor.test(
    dados_teste$Year.Published,
    dados_teste[[variavel]],
    method = "spearman",
    exact = FALSE
  )

  data.frame(
    Variavel = nome,
    N = nrow(dados_teste),
    Correlacao = unname(resultado$estimate),
    P_valor = resultado$p.value
  )
}

# Executar os testes
resultados_spearman <- rbind(
  testar_spearman("Min.Players", "Número mínimo de jogadores"),
  testar_spearman("Max.Players", "Número máximo de jogadores"),
  testar_spearman("Users.Rated", "Popularidade - avaliações"),
  testar_spearman("Complexity.Average", "Complexidade"),
  testar_spearman("Play.Time", "Duração")
)

print(resultados_spearman)

# Correção dos p-valores para múltiplos testes
resultados_spearman$P_ajustado <- p.adjust(
  resultados_spearman$P_valor,
  method = "BH"
)

print(resultados_spearman)


# Análise de sensibilidade: Max.Players
# Excluir temporariamente os valores 99 e 100

dados_max_sens <- subset(
  dados_7,
  !is.na(Max.Players) &
    !Max.Players %in% c(99, 100)
)

teste_max_sem_99_100 <- cor.test(
  dados_max_sens$Year.Published,
  dados_max_sens$Max.Players,
  method = "spearman",
  exact = FALSE
)

print(teste_max_sem_99_100)

cat(
  "Quantidade de jogos na análise:",
  nrow(dados_max_sens),
  "\n"
)# Resultado da correlação sem os valores 99 e 100
cat("Correlação:", teste_max_sem_99_100$estimate, "\n")
cat("P-valor:", teste_max_sem_99_100$p.value, "\n")

# Resultados com p-valores ajustados
print(resultados_spearman)
# HIPÓTESE 8
# Relação entre avaliação média e quantidade de proprietários

# Selecionar registros completos
dados_h8 <- dados[
  complete.cases(dados[, c("Rating.Average", "Owned.Users")]),
]

# Correlação de Spearman
teste_h8 <- cor.test(
  dados_h8$Rating.Average,
  dados_h8$Owned.Users,
  method = "spearman",
  exact = FALSE
)

# Apresentar resultados
print(teste_h8)

cat(
  "Quantidade de jogos analisados:",
  nrow(dados_h8),
  "\n"
)

# Gráfico de dispersão
plot(
  dados_h8$Owned.Users,
  dados_h8$Rating.Average,
  log = "x",
  pch = 16,
  cex = 0.5,
  xlab = "Quantidade de proprietários (escala logarítmica)",
  ylab = "Avaliação média",
  main = "Avaliação média e popularidade dos jogos"
)

# PACOTES PARA GRÁFICOS
library(ggplot2)
library(patchwork)

# Reunir os resultados já calculados
graficos_h7 <- merge(
  evolucao_jogadores,
  resumo_evolucao,
  by = "Decada"
)

graficos_h7 <- merge(
  graficos_h7,
  quantidade_decada,
  by = "Decada"
)

print(graficos_h7)


# ==========================================
# HIPOTESE 7 - GRAFICOS PARA APRESENTACAO
# ==========================================

# 1. NUMERO MAXIMO DE JOGADORES

g1 <- ggplot(graficos_h7,
             aes(x = Decada, y = Max.Players)) +
  geom_line(color = "#245A81", linewidth = 1.2) +
  geom_point(color = "#245A81",
             fill = "#72B7D2",
             shape = 21,
             size = 3.5,
             stroke = 1) +
  scale_x_continuous(breaks = graficos_h7$Decada) +
  labs(
    title = "Capacidade de jogadores",
    subtitle = "Mediana do numero maximo de participantes",
    x = "Decada de publicacao",
    y = "Numero de jogadores"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# 2. POPULARIDADE

g2 <- ggplot(graficos_h7,
             aes(x = Decada, y = Mediana_Avaliacoes)) +
  geom_line(color = "#168477", linewidth = 1.2) +
  geom_point(color = "#168477",
             fill = "#78C9B5",
             shape = 21,
             size = 3.5,
             stroke = 1) +
  scale_x_continuous(breaks = graficos_h7$Decada) +
  labs(
    title = "Popularidade dos jogos",
    subtitle = "Mediana do numero de avaliacoes no BGG",
    x = "Decada de publicacao",
    y = "Numero de avaliacoes"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# 3. COMPLEXIDADE

g3 <- ggplot(graficos_h7,
             aes(x = Decada, y = Mediana_Complexidade)) +
  geom_line(color = "#8055A5", linewidth = 1.2) +
  geom_point(color = "#8055A5",
             fill = "#B99BD4",
             shape = 21,
             size = 3.5,
             stroke = 1) +
  scale_x_continuous(breaks = graficos_h7$Decada) +
  labs(
    title = "Complexidade dos jogos",
    subtitle = "Mediana da complexidade percebida",
    x = "Decada de publicacao",
    y = "Complexidade"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# 4. DURACAO

g4 <- ggplot(graficos_h7,
             aes(x = Decada, y = Mediana_Duracao)) +
  geom_line(color = "#C17A25", linewidth = 1.2) +
  geom_point(color = "#C17A25",
             fill = "#E9B96E",
             shape = 21,
             size = 3.5,
             stroke = 1) +
  scale_x_continuous(breaks = graficos_h7$Decada) +
  labs(
    title = "Duracao das partidas",
    subtitle = "Mediana do tempo de jogo",
    x = "Decada de publicacao",
    y = "Duracao (minutos)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# PAINEL FINAL COM OS QUATRO GRAFICOS

painel_h7 <- (g1 | g2) / (g3 | g4) +
  plot_annotation(
    title = "EVOLUCAO DOS JOGOS DE TABULEIRO",
    subtitle = "Hipotese 7 | Analise temporal das caracteristicas dos jogos",
    caption = paste(
      "Fonte: dados do BoardGameGeek.",
      "Valores apresentados como medianas por decada.",
      "Decada de 1920: apenas 4 jogos; decada de 2020 incompleta."
    ),
    theme = theme(
      plot.title = element_text(
        size = 18,
        face = "bold",
        hjust = 0.5
      ),
      plot.subtitle = element_text(
        size = 12,
        hjust = 0.5
      ),
      plot.caption = element_text(size = 9)
    )
  )

# EXIBIR NO RGUI
print(painel_h7)

# SALVAR EM ALTA RESOLUCAO
ggsave(
  "hipotese7_painel_profissional.png",
  plot = painel_h7,
  width = 14,
  height = 9,
  dpi = 300,
  bg = "white"
)


# =====================================================
# HIPOTESE 7 - PAINEL CIENTIFICO PREMIUM
# =====================================================

library(ggplot2)
library(patchwork)

# Identidade visual
azul_marinho <- "#17324D"
azul <- "#3478A8"
verde <- "#168A83"
roxo <- "#8064A9"
laranja <- "#D28A36"

fundo <- "#F1F4F8"
cinza <- "#64748B"

# Quantidade de observacoes validas para Max.Players
base_max <- dados_7[
  complete.cases(dados_7[, c("Decada", "Max.Players")]),
]

contagem_max <- aggregate(
  Max.Players ~ Decada,
  data = base_max,
  FUN = length
)

names(contagem_max)[2] <- "N_MaxPlayers"

graficos_h7 <- merge(
  graficos_h7,
  contagem_max,
  by = "Decada",
  all.x = TRUE
)

# Funcao para padronizar os quatro graficos
criar_grafico_h7 <- function(
  dados, variavel, titulo, subtitulo,
  eixo_y, cor, casas, n_total
) {

  dados$valor_plot <- dados[[variavel]]

  dados$rotulo <- format(
    round(dados$valor_plot, casas),
    trim = TRUE,
    decimal.mark = ",",
    big.mark = "."
  )

  ggplot(
    dados,
    aes(x = Decada, y = valor_plot)
  ) +

    geom_line(
      color = cor,
      linewidth = 1.35,
      lineend = "round"
    ) +

    geom_point(
      color = cor,
      fill = "white",
      shape = 21,
      size = 3.8,
      stroke = 1.35
    ) +

    geom_label(
      aes(label = rotulo),
      size = 3,
      fontface = "bold",
      color = azul_marinho,
      fill = "white",
      label.size = 0,
      label.padding = grid::unit(0.15, "lines"),
      nudge_y = diff(range(dados$valor_plot, na.rm = TRUE)) * 0.075
    ) +

    scale_x_continuous(
      breaks = dados$Decada,
      expand = expansion(mult = c(0.04, 0.04))
    ) +

    scale_y_continuous(
      expand = expansion(mult = c(0.12, 0.28))
    ) +

    labs(
      title = titulo,
      subtitle = paste0(subtitulo, " | N = ", n_total),
      x = "Década de publicação",
      y = eixo_y
    ) +

    theme_minimal(base_size = 11) +

    theme(
      plot.title = element_text(
        size = 14,
        face = "bold",
        color = azul_marinho
      ),

      plot.subtitle = element_text(
        size = 9,
        color = cinza
      ),

      axis.title = element_text(
        size = 9,
        face = "bold",
        color = azul_marinho
      ),

      axis.text = element_text(
        size = 8,
        color = cinza
      ),

      axis.text.x = element_text(
        angle = 45,
        hjust = 1
      ),

      panel.grid.major.y = element_line(
        color = "#E5EAF0",
        linewidth = 0.5
      ),

      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),

      panel.background = element_rect(
        fill = "white",
        color = NA
      ),

      plot.background = element_rect(
        fill = "white",
        color = NA
      ),

      panel.border = element_rect(
        color = "#E4EAF0",
        fill = NA,
        linewidth = 0.7
      ),

      plot.margin = margin(14, 16, 14, 16)
    )
}


# 1. CAPACIDADE DE JOGADORES

g1 <- criar_grafico_h7(
  graficos_h7,
  "Max.Players",
  "01 | CAPACIDADE DE JOGADORES",
  "Mediana do número máximo de participantes",
  "Número de jogadores",
  azul,
  0,
  sum(graficos_h7$N_MaxPlayers, na.rm = TRUE)
)


# 2. POPULARIDADE

g2 <- criar_grafico_h7(
  graficos_h7,
  "Mediana_Avaliacoes",
  "02 | POPULARIDADE",
  "Mediana das avaliações registradas no BGG",
  "Número de avaliações",
  verde,
  1,
  sum(graficos_h7$N_Avaliacoes, na.rm = TRUE)
)


# 3. COMPLEXIDADE

g3 <- criar_grafico_h7(
  graficos_h7,
  "Mediana_Complexidade",
  "03 | COMPLEXIDADE",
  "Mediana da complexidade percebida",
  "Índice de complexidade",
  roxo,
  2,
  sum(graficos_h7$N_Complexidade, na.rm = TRUE)
)


# 4. DURACAO

g4 <- criar_grafico_h7(
  graficos_h7,
  "Mediana_Duracao",
  "04 | TEMPO DE JOGO",
  "Mediana da duração das partidas",
  "Duração (minutos)",
  laranja,
  0,
  sum(graficos_h7$N_Duracao, na.rm = TRUE)
)


# MONTAGEM DO PAINEL

painel_h7 <- (g1 | g2) / (g3 | g4) +

  plot_annotation(
    title = "A EVOLUÇÃO DOS JOGOS DE TABULEIRO",
    subtitle = paste(
      "HIPÓTESE 7 | Características dos jogos",
      "ao longo das décadas"
    ),

    caption = paste(
      "Fonte: conjunto de dados BoardGameGeek.",
      "Medianas calculadas por década.",
      "1920: apenas 4 jogos na amostra; 2020: década incompleta.",
      "Correlação não implica causalidade."
    ),

    theme = theme(
      plot.title = element_text(
        size = 21,
        face = "bold",
        color = azul_marinho,
        hjust = 0.5,
        margin = margin(b = 8)
      ),

      plot.subtitle = element_text(
        size = 12,
        color = cinza,
        hjust = 0.5,
        margin = margin(b = 18)
      ),

      plot.caption = element_text(
        size = 9,
        color = cinza,
        hjust = 0,
        margin = margin(t = 15)
      ),

      plot.background = element_rect(
        fill = fundo,
        color = NA
      )
    )
  ) &

  theme(
    plot.background = element_rect(
      fill = fundo,
      color = NA
    )
  )


# EXIBIR
print(painel_h7)


# EXPORTAR EM ALTA RESOLUCAO
ggsave(
  "HIPOTESE_7_PAINEL_PREMIUM.png",
  plot = painel_h7,
  width = 16,
  height = 11,
  dpi = 400,
  bg = fundo
)

# VERSAO PDF PARA DOCUMENTO ACADEMICO
ggsave(
  "HIPOTESE_7_PAINEL_PREMIUM.pdf",
  plot = painel_h7,
  width = 16,
  height = 11,
  bg = fundo
)

