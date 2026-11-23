# ============================================================
# JOGOS DE TABULEIRO — HIPÓTESES 1, 2 E 3
# ============================================================

# 1. PACOTES -------------------------------------------------

pacotes <- c("ggplot2", "dplyr", "scales", "patchwork")

faltantes <- pacotes[
  !vapply(
    pacotes,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(faltantes) > 0) {
  install.packages(faltantes)
}

library(ggplot2)
library(dplyr)
library(patchwork)

# 2. IMPORTAR A PLANILHA --------------------------------------
# Todas as colunas entram como texto.
# A conversão numérica é feita depois.

arquivo <- file.choose()

dados <- read.csv2(
  arquivo,
  na.strings = c("", "NA"),
  colClasses = "character",
  stringsAsFactors = FALSE,
  check.names = FALSE,
  fileEncoding = "UTF-8-BOM"
)

# Remover a coluna sem nome que contém a numeração das linhas.

nomes_validos <- !is.na(names(dados)) &
  nzchar(trimws(names(dados)))

dados <- dados[, nomes_validos, drop = FALSE]

# Padronizar espaços e pontos nos nomes.

names(dados) <- make.names(
  trimws(names(dados)),
  unique = TRUE
)

necessarias <- c(
  "Name",
  "Complexity.Average",
  "Rating.Average",
  "Play.Time",
  "Max.Players",
  "Users.Rated"
)

ausentes <- setdiff(necessarias, names(dados))

if (length(ausentes) > 0) {
  stop(
    "Colunas ausentes: ",
    paste(ausentes, collapse = ", "),
    ". Selecione bgg_dataset_tratado.csv."
  )
}

# 3. CONVERTER AS COLUNAS NUMÉRICAS ----------------------------
# Aceita valores como 8,79 ou 8.79.

colunas_numericas <- setdiff(necessarias, "Name")

for (coluna in colunas_numericas) {
  
  valores <- trimws(dados[[coluna]])
  
  valores <- gsub(
    ",",
    ".",
    valores,
    fixed = TRUE
  )
  
  convertidos <- suppressWarnings(as.numeric(valores))
  
  invalidos <- !is.na(valores) &
    nzchar(valores) &
    is.na(convertidos)
  
  if (any(invalidos)) {
    stop(
      "Há valores que não puderam ser convertidos na coluna ",
      coluna,
      ": ",
      paste(
        head(unique(valores[invalidos]), 5),
        collapse = ", "
      )
    )
  }
  
  dados[[coluna]] <- convertidos
}

jogos <- dados |>
  transmute(
    nome = Name,
    complexidade = Complexity.Average,
    nota = Rating.Average,
    duracao = Play.Time,
    jogadores = Max.Players,
    avaliacoes = Users.Rated
  )

cat("\nRegistros importados:", nrow(jogos), "\n")

# 4. DADOS VÁLIDOS PARA CADA HIPÓTESE --------------------------
# Utiliza a planilha tratada selecionada.
# Valores ausentes são removidos por análise.

h1 <- jogos |>
  filter(
    is.finite(complexidade),
    is.finite(nota),
    between(complexidade, 1, 5),
    between(nota, 1, 10)
  )

h2 <- jogos |>
  filter(
    is.finite(duracao),
    duracao > 0,
    is.finite(nota),
    between(nota, 1, 10)
  )

h3 <- jogos |>
  filter(
    is.finite(jogadores),
    jogadores >= 1,
    is.finite(avaliacoes),
    avaliacoes > 0
  )

# Conferir se há dados suficientes e variação nas duas variáveis.

verificar_analise <- function(base, x, y, hipotese) {
  
  if (
    nrow(base) < 3 ||
    length(unique(base[[x]])) < 2 ||
    length(unique(base[[y]])) < 2
  ) {
    stop(
      hipotese,
      ": dados insuficientes ou sem variação."
    )
  }
}

verificar_analise(
  h1, "complexidade", "nota", "H1"
)

verificar_analise(
  h2, "duracao", "nota", "H2"
)

verificar_analise(
  h3, "jogadores", "avaliacoes", "H3"
)

# 5. CORRELAÇÕES DE SPEARMAN ----------------------------------

teste_h1 <- cor.test(
  h1$complexidade,
  h1$nota,
  method = "spearman",
  exact = FALSE
)

teste_h2 <- cor.test(
  h2$duracao,
  h2$nota,
  method = "spearman",
  exact = FALSE
)

teste_h3 <- cor.test(
  h3$jogadores,
  h3$avaliacoes,
  method = "spearman",
  exact = FALSE
)

resultados <- data.frame(
  hipotese = c("H1", "H2", "H3"),
  comparacao = c(
    "Complexidade e nota média",
    "Duração e nota média",
    "Máximo de jogadores e número de avaliações"
  ),
  n = c(
    nrow(h1),
    nrow(h2),
    nrow(h3)
  ),
  rho = c(
    unname(teste_h1$estimate),
    unname(teste_h2$estimate),
    unname(teste_h3$estimate)
  ),
  p_valor = c(
    teste_h1$p.value,
    teste_h2$p.value,
    teste_h3$p.value
  )
)

resultados$p_ajustado_holm <- p.adjust(
  resultados$p_valor,
  method = "holm"
)

print(resultados)

# 6. ESTILO DOS GRÁFICOS --------------------------------------

fundo <- "#F7F8FC"
texto <- "#202C3C"

azul <- "#246BA8"
roxo <- "#7451A8"
laranja <- "#C86B32"

numero <- scales::label_number(
  big.mark = ".",
  decimal.mark = ","
)

subtitulo <- function(teste, n) {
  
  paste0(
    "Spearman \u03C1 = ",
    formatC(
      unname(teste$estimate),
      format = "f",
      digits = 2,
      decimal.mark = ","
    ),
    "  |  ",
    numero(n),
    " jogos"
  )
}

tema_jogos <- theme_minimal(
  base_size = 13,
  base_family = "sans"
) +
  theme(
    plot.background = element_rect(
      fill = fundo,
      color = NA
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(
      color = "#E5E9F0",
      linewidth = 0.35
    ),
    plot.title = element_text(
      face = "bold",
      size = 19,
      color = texto,
      margin = margin(b = 8)
    ),
    plot.subtitle = element_text(
      color = "#64748B",
      size = 12,
      margin = margin(b = 18)
    ),
    axis.title = element_text(
      color = texto,
      face = "bold",
      size = 12
    ),
    axis.title.x = element_text(
      margin = margin(t = 12)
    ),
    axis.title.y = element_text(
      margin = margin(r = 12)
    ),
    axis.text = element_text(
      color = "#526174",
      size = 11
    ),
    legend.position = "bottom",
    legend.title = element_text(size = 11),
    plot.caption = element_text(
      color = "#64748B",
      size = 9,
      hjust = 0,
      lineheight = 1.15,
      margin = margin(t = 16)
    ),
    plot.margin = margin(22, 25, 18, 20)
  )

fonte <- paste(
  "Fonte: BoardGameGeek / Kaggle.",
  "Base tratada fornecida pelo grupo."
)


# ============================================================
# H1 — SEIS JOGOS DESTACADOS POR CRITÉRIOS DOS DADOS
#
# Nota alta  × complexidade baixa, média e alta
# Nota baixa × complexidade baixa, média e alta
#
# Em cada combinação, selecionar o jogo com mais avaliações.
# ============================================================

# 1. PACOTES -------------------------------------------------

pacotes <- c("ggplot2", "dplyr", "ggrepel", "mgcv")

faltantes <- pacotes[
  !vapply(
    pacotes,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(faltantes) > 0) {
  install.packages(faltantes)
}

library(ggplot2)
library(dplyr)

if (!exists("h1")) {
  stop(
    "Execute primeiro a importação da planilha e a criação do objeto h1."
  )
}

colunas_necessarias <- c(
  "nome", "complexidade", "nota", "avaliacoes"
)

if (!all(colunas_necessarias %in% names(h1))) {
  stop("O objeto h1 precisa conter nome, complexidade, nota e avaliacoes.")
}

# 2. CORES ---------------------------------------------------

papel <- "#FAF7F0"
verde <- "#244B40"
dourado <- "#B78435"
cinza <- "#65746D"

# 3. LIMITES DA CLASSIFICAÇÃO ---------------------------------

cortes_complexidade <- quantile(
  h1$complexidade,
  probs = c(0.25, 0.75),
  na.rm = TRUE,
  names = FALSE
)

cortes_nota <- quantile(
  h1$nota,
  probs = c(0.25, 0.75),
  na.rm = TRUE,
  names = FALSE
)

limites <- data.frame(
  variavel = c("Complexidade", "Nota"),
  percentil_25 = c(
    cortes_complexidade[1],
    cortes_nota[1]
  ),
  percentil_75 = c(
    cortes_complexidade[2],
    cortes_nota[2]
  )
)

cat("\nLimites utilizados na classificação:\n")
print(limites)

# 4. SELECIONAR OS SEIS JOGOS ---------------------------------

destaques <- h1 |>
  filter(
    is.finite(complexidade),
    is.finite(nota),
    is.finite(avaliacoes),
    avaliacoes > 0
  ) |>
  mutate(
    faixa_complexidade = case_when(
      complexidade <= cortes_complexidade[1] ~ "Baixa",
      complexidade <= cortes_complexidade[2] ~ "Média",
      TRUE ~ "Alta"
    ),
    faixa_nota = case_when(
      nota >= cortes_nota[2] ~ "Alta",
      nota <= cortes_nota[1] ~ "Baixa",
      TRUE ~ "Intermediária"
    ),
    faixa_complexidade = factor(
      faixa_complexidade,
      levels = c("Baixa", "Média", "Alta")
    ),
    faixa_nota = factor(
      faixa_nota,
      levels = c("Alta", "Baixa", "Intermediária")
    )
  ) |>
  filter(faixa_nota %in% c("Alta", "Baixa")) |>
  group_by(faixa_nota, faixa_complexidade) |>
  arrange(
    desc(avaliacoes),
    nome,
    .by_group = TRUE
  ) |>
  slice_head(n = 1) |>
  ungroup() |>
  arrange(faixa_nota, faixa_complexidade) |>
  mutate(
    peca = row_number(),
    perfil = paste0(
      "Nota ",
      tolower(as.character(faixa_nota)),
      " / complexidade ",
      tolower(as.character(faixa_complexidade))
    )
  )

if (nrow(destaques) != 6) {
  stop(
    "Foram encontradas apenas ",
    nrow(destaques),
    " combinações com dados. Confira a tabela de candidatos."
  )
}

# Quebrar nomes longos para reduzir a largura dos rótulos.

quebrar_nome <- function(nome) {
  paste(strwrap(nome, width = 28), collapse = "\n")
}

destaques <- destaques |>
  mutate(
    rotulo = paste0(
      vapply(nome, quebrar_nome, character(1)),
      "\nComplexidade ",
      formatC(
        complexidade,
        format = "f",
        digits = 2,
        decimal.mark = ","
      ),
      " | Nota ",
      formatC(
        nota,
        format = "f",
        digits = 2,
        decimal.mark = ","
      )
    )
  )

tabela_destaques <- destaques |>
  select(
    perfil,
    nome,
    complexidade,
    nota,
    avaliacoes
  )

cat("\nSeis jogos selecionados:\n")
print(tabela_destaques, n = Inf, width = Inf)

# 5. DESENHAR OS PEÕES ----------------------------------------
# Os peões ficam acima do ponto real de cada jogo.

escala_peao <- 0.65
deslocamento <- 0.38

contorno <- data.frame(
  dx = c(
    -0.060, 0.060, 0.060, 0.042,
    0.028, 0.016, 0.016, 0.036,
    0.036, -0.036, -0.036, -0.016,
    -0.016, -0.028, -0.042, -0.060
  ),
  dy = c(
    -0.19, -0.19, -0.12, -0.08,
    0.00, 0.16, 0.22, 0.22,
    0.29, 0.29, 0.22, 0.22,
    0.16, 0.00, -0.08, -0.12
  )
)

angulo <- seq(
  from = 0,
  to = 2 * pi,
  length.out = 60
)

cabeca <- data.frame(
  dx = 0.034 * cos(angulo),
  dy = 0.075 * sin(angulo) + 0.36
)

peoes <- merge(
  destaques,
  contorno,
  by = NULL
) |>
  mutate(
    x = complexidade + dx * escala_peao,
    y = nota + deslocamento + dy * escala_peao
  )

cabecas <- merge(
  destaques,
  cabeca,
  by = NULL
) |>
  mutate(
    x = complexidade + dx * escala_peao,
    y = nota + deslocamento + dy * escala_peao
  )

# Posição inicial dos rótulos.
# Notas altas recebem rótulos acima.
# Notas baixas recebem rótulos abaixo.

destaques <- destaques |>
  mutate(
    x_rotulo = complexidade,
    y_rotulo = if_else(
      faixa_nota == "Alta",
      nota + 1.05,
      nota - 1.10
    )
  )

# 6. CORRELAÇÃO DE SPEARMAN -----------------------------------

teste_h1_tabuleiro <- cor.test(
  h1$complexidade,
  h1$nota,
  method = "spearman",
  exact = FALSE
)

rho_texto <- formatC(
  unname(teste_h1_tabuleiro$estimate),
  format = "f",
  digits = 2,
  decimal.mark = ","
)

n_texto <- format(
  sum(
    is.finite(h1$complexidade) &
      is.finite(h1$nota)
  ),
  big.mark = ".",
  decimal.mark = ",",
  scientific = FALSE,
  trim = TRUE
)

# 7. CONSTRUIR O GRÁFICO --------------------------------------

grafico_h1_tabuleiro <- ggplot(
  h1,
  aes(x = complexidade, y = nota)
) +
  
  # Jogos individuais.
  geom_point(
    color = verde,
    alpha = 0.09,
    size = 0.8
  ) +
  
  # Tendência visual.
  geom_smooth(
    method = "gam",
    formula = y ~ s(x, bs = "cs"),
    se = FALSE,
    color = verde,
    linewidth = 1.2
  ) +
  
  # Conexão entre o valor real e o peão.
  geom_segment(
    data = destaques,
    aes(
      x = complexidade,
      xend = complexidade,
      y = nota,
      yend = nota + deslocamento -
        0.19 * escala_peao
    ),
    inherit.aes = FALSE,
    color = dourado,
    linewidth = 0.5
  ) +
  
  # Corpo dos peões.
  geom_polygon(
    data = peoes,
    aes(
      x = x,
      y = y,
      group = peca
    ),
    inherit.aes = FALSE,
    fill = dourado,
    color = papel,
    linewidth = 0.3
  ) +
  
  # Cabeça dos peões.
  geom_polygon(
    data = cabecas,
    aes(
      x = x,
      y = y,
      group = peca
    ),
    inherit.aes = FALSE,
    fill = dourado,
    color = papel,
    linewidth = 0.3
  ) +
  
  # Coordenadas exatas dos jogos destacados.
  geom_point(
    data = destaques,
    aes(
      x = complexidade,
      y = nota
    ),
    inherit.aes = FALSE,
    shape = 21,
    fill = dourado,
    color = papel,
    size = 3.1,
    stroke = 0.6
  ) +
  
  # Ligações dos rótulos aos valores reais.
  geom_segment(
    data = destaques,
    aes(
      x = complexidade,
      y = nota,
      xend = x_rotulo,
      yend = y_rotulo
    ),
    inherit.aes = FALSE,
    color = dourado,
    linewidth = 0.35,
    alpha = 0.65
  ) +
  
  # Rótulos reposicionados automaticamente.
  ggrepel::geom_label_repel(
    data = destaques,
    aes(
      x = x_rotulo,
      y = y_rotulo,
      label = rotulo
    ),
    inherit.aes = FALSE,
    direction = "x",
    color = verde,
    fill = papel,
    size = 3.3,
    lineheight = 1.15,
    box.padding = 0.6,
    point.padding = 0.2,
    segment.color = dourado,
    min.segment.length = 0,
    max.overlaps = Inf,
    max.iter = 20000,
    max.time = 3,
    seed = 123
  ) +
  
  scale_x_continuous(
    breaks = 1:5,
    expand = expansion(mult = 0.08)
  ) +
  
  scale_y_continuous(
    breaks = 1:10,
    expand = expansion(mult = 0.06)
  ) +
  
  labs(
    title = "Quanto mais difícil, melhor a jogada?",
    subtitle = paste0(
      "Complexidade e avaliação dos jogos",
      "   |   Spearman \u03C1 = ",
      rho_texto,
      "   |   ",
      n_texto,
      " jogos"
    ),
    x = "Complexidade média (1–5)",
    y = "Nota média (1–10)",
    caption = paste0(
      "Destaques: jogo com mais avaliações em cada uma das seis combinações.",
      "\nComplexidade baixa, média e alta definida pelos quartis; ",
      "notas baixas e altas pelos quartis inferior e superior.",
      "\nPontos dourados marcam valores exatos. ",
      "A curva resume a tendência.",
      "\nFonte: BoardGameGeek / Kaggle — base tratada do grupo."
    )
  ) +
  
  theme_minimal(
    base_size = 13,
    base_family = "sans"
  ) +
  
  theme(
    plot.background = element_rect(
      fill = papel,
      color = NA
    ),
    panel.background = element_rect(
      fill = papel,
      color = NA
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(
      color = "#E6E3D9",
      linewidth = 0.35
    ),
    plot.title = element_text(
      color = verde,
      face = "bold",
      size = 21,
      margin = margin(b = 10)
    ),
    plot.subtitle = element_text(
      color = cinza,
      size = 11,
      margin = margin(b = 20)
    ),
    axis.title = element_text(
      color = verde,
      face = "bold",
      size = 12
    ),
    axis.title.x = element_text(
      margin = margin(t = 14)
    ),
    axis.title.y = element_text(
      margin = margin(r = 14)
    ),
    axis.text = element_text(
      color = cinza,
      size = 11
    ),
    plot.caption = element_text(
      color = cinza,
      hjust = 0,
      size = 9,
      lineheight = 1.2,
      margin = margin(t = 18)
    ),
    plot.margin = margin(25, 35, 20, 25),
    legend.position = "none"
  )

# 8. SALVAR --------------------------------------------------

pasta_saida <- path.expand(
  "~/Desktop/Jogos_Tabuleiro_Graficos"
)

dir.create(
  pasta_saida,
  recursive = TRUE,
  showWarnings = FALSE
)

arquivo_saida <- file.path(
  pasta_saida,
  "H1_seis_jogos_tabuleiro.png"
)

# Exportação direta para evitar problemas com a aba Plots.

ggsave(
  filename = arquivo_saida,
  plot = grafico_h1_tabuleiro,
  device = "png",
  width = 14,
  height = 9,
  units = "in",
  dpi = 400,
  bg = papel
)

write.csv2(
  tabela_destaques,
  file.path(
    pasta_saida,
    "H1_seis_jogos_destacados.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv2(
  limites,
  file.path(
    pasta_saida,
    "H1_limites_classificacao.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# Abrir a imagem no Mac.

system2("open", shQuote(arquivo_saida))

cat(
  "\nImagem salva em:\n",
  arquivo_saida,
  "\n"
)


# ============================================================
# H2 — DURAÇÃO DAS PARTIDAS × NOTA MÉDIA
# Execute após o script que criou o objeto h2.
# ============================================================

# 1. PACOTES -------------------------------------------------

pacotes <- c("ggplot2", "dplyr")

faltantes <- pacotes[
  !vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)
]

if (length(faltantes) > 0) {
  install.packages(faltantes)
}

library(ggplot2)
library(dplyr)

if (!exists("h2")) {
  stop("Execute primeiro o script que criou o objeto h2.")
}

if (!all(c("duracao", "nota") %in% names(h2))) {
  stop("O objeto h2 precisa ter as colunas duracao e nota.")
}

# 2. CORES E FORMATAÇÃO --------------------------------------

papel   <- "#FAF7F0"
verde   <- "#244B40"
dourado <- "#B78435"
cinza   <- "#65746D"

formatar_inteiro <- function(x) {
  format(
    x,
    big.mark = ".",
    decimal.mark = ",",
    scientific = FALSE,
    trim = TRUE
  )
}

formatar_decimal <- function(x) {
  formatC(
    x,
    digits = 2,
    format = "f",
    decimal.mark = ","
  )
}

# 3. PREPARAR OS DADOS ---------------------------------------

base_h2 <- h2 |>
  filter(
    is.finite(duracao),
    duracao > 0,
    is.finite(nota),
    between(nota, 1, 10)
  )

if (
  nrow(base_h2) < 3 ||
  n_distinct(base_h2$duracao) < 2 ||
  n_distinct(base_h2$nota) < 2
) {
  stop("Não há variação suficiente para calcular a correlação.")
}

# Faixas explícitas de duração.
# Nenhum jogo válido é excluído por ter duração muito alta.

ordem_faixas <- c(
  "Até 15 min",
  ">15–30 min",
  ">30–60 min",
  ">1–2 h",
  ">2–4 h",
  ">4–8 h",
  ">8 h"
)

base_h2 <- base_h2 |>
  mutate(
    faixa = cut(
      duracao,
      breaks = c(0, 15, 30, 60, 120, 240, 480, Inf),
      labels = ordem_faixas,
      right = TRUE,
      include.lowest = TRUE
    )
  )

# 4. CORRELAÇÃO ----------------------------------------------
# Usa os valores originais, sem substituir pelas faixas.

teste_h2 <- cor.test(
  base_h2$duracao,
  base_h2$nota,
  method = "spearman",
  exact = FALSE
)

rho_texto <- formatar_decimal(
  unname(teste_h2$estimate)
)

# 5. RESUMO POR FAIXA ----------------------------------------

resumo_h2 <- base_h2 |>
  group_by(faixa, .drop = TRUE) |>
  summarise(
    quantidade = n(),
    mediana = median(nota),
    quartil_25 = unname(quantile(nota, 0.25)),
    quartil_75 = unname(quantile(nota, 0.75)),
    .groups = "drop"
  )

rotulos_faixas <- setNames(
  paste0(
    as.character(resumo_h2$faixa),
    "\nn = ",
    formatar_inteiro(resumo_h2$quantidade)
  ),
  as.character(resumo_h2$faixa)
)

# 6. GRÁFICO -------------------------------------------------

grafico_h2_boxplot <- ggplot(
  base_h2,
  aes(x = faixa, y = nota)
) +
  
  geom_boxplot(
    width = 0.58,
    fill = "#B9CFC2",
    color = verde,
    linewidth = 0.7,
    outlier.color = verde,
    outlier.alpha = 0.16,
    outlier.size = 0.8
  ) +
  
  # Ponto dourado marca a mediana.
  geom_point(
    data = resumo_h2,
    aes(x = faixa, y = mediana),
    inherit.aes = FALSE,
    shape = 21,
    fill = dourado,
    color = papel,
    size = 3.5,
    stroke = 0.7
  ) +
  
  scale_x_discrete(
    labels = rotulos_faixas,
    drop = TRUE
  ) +
  
  scale_y_continuous(
    breaks = 1:10,
    limits = c(1, 10),
    expand = expansion(mult = c(0.02, 0.04))
  ) +
  
  labs(
    title = "Vale o tempo à mesa?",
    subtitle = paste0(
      "Distribuição das notas por duração da partida\n",
      "Spearman \u03C1 = ",
      rho_texto,
      "   |   ",
      formatar_inteiro(nrow(base_h2)),
      " jogos"
    ),
    x = "Duração da partida",
    y = "Nota média do jogo (1–10)",
    caption = paste(
      "Caixas: 50% centrais das notas. Linha interna e ponto dourado: mediana.",
      "Hastes: até 1,5 intervalo interquartil; pontos externos indicam valores além das hastes.",
      "n = quantidade de jogos. Todos os jogos válidos estão incluídos; durações acima de 8 h estão agrupadas.",
      "Spearman usa as durações individuais originais. Associação não implica causalidade.",
      "Fonte: BoardGameGeek / Kaggle — base tratada do grupo.",
      sep = "\n"
    )
  ) +
  
  theme_minimal(
    base_size = 13,
    base_family = "sans"
  ) +
  
  theme(
    plot.background = element_rect(
      fill = papel,
      color = NA
    ),
    panel.background = element_rect(
      fill = papel,
      color = NA
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      color = "#E3E2D9",
      linewidth = 0.4
    ),
    plot.title = element_text(
      color = verde,
      face = "bold",
      size = 24,
      margin = margin(b = 10)
    ),
    plot.subtitle = element_text(
      color = cinza,
      size = 12,
      lineheight = 1.3,
      margin = margin(b = 22)
    ),
    axis.title = element_text(
      color = verde,
      face = "bold",
      size = 12
    ),
    axis.title.x = element_text(
      margin = margin(t = 18)
    ),
    axis.title.y = element_text(
      margin = margin(r = 14)
    ),
    axis.text.x = element_text(
      color = verde,
      size = 11,
      lineheight = 1.3,
      margin = margin(t = 10)
    ),
    axis.text.y = element_text(
      color = cinza,
      size = 11
    ),
    plot.caption = element_text(
      color = cinza,
      size = 9,
      hjust = 0,
      lineheight = 1.2,
      margin = margin(t = 22)
    ),
    plot.margin = margin(25, 30, 20, 25)
  )

# 7. SALVAR --------------------------------------------------

pasta_saida <- path.expand(
  "~/Desktop/Jogos_Tabuleiro_Graficos"
)

dir.create(
  pasta_saida,
  recursive = TRUE,
  showWarnings = FALSE
)

arquivo_saida <- file.path(
  pasta_saida,
  "H2_boxplot_duracao_nota.png"
)


ggsave(
  filename = arquivo_saida,
  plot = grafico_h2_boxplot,
  device = "png",
  width = 13,
  height = 8,
  units = "in",
  dpi = 400,
  bg = papel
)

write.csv2(
  resumo_h2,
  file = file.path(
    pasta_saida,
    "H2_resumo_por_duracao.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# 8. MOSTRAR RESULTADOS E ABRIR A IMAGEM -----------------------

print(resumo_h2)

cat(
  "\nCorrelação de Spearman:", rho_texto,
  "\nImagem salva em:", arquivo_saida,
  "\n"
)

if (Sys.info()[["sysname"]] == "Darwin") {
  system2("open", shQuote(arquivo_saida))
} else if (.Platform$OS.type == "windows") {
  shell.exec(arquivo_saida)
}


# ============================================================
# H3 — CAPACIDADE DE JOGADORES E POPULARIDADE
# BOXPLOTS POR CAPACIDADE MÁXIMA
# ============================================================

library(ggplot2)
library(dplyr)

if (!exists("h3")) {
  stop("Execute primeiro o script que criou o objeto h3.")
}

# 1. CORES ---------------------------------------------------

papel <- "#FAF7F0"
verde <- "#244B40"
verde_claro <- "#B9CFC2"
dourado <- "#B78435"
cinza <- "#65746D"

# 2. DADOS VÁLIDOS --------------------------------------------

base_h3 <- h3 |>
  filter(
    is.finite(jogadores),
    jogadores >= 1,
    is.finite(avaliacoes),
    avaliacoes > 0
  )

# 3. CORRELAÇÃO COM OS VALORES ORIGINAIS -----------------------

teste_h3 <- cor.test(
  base_h3$jogadores,
  base_h3$avaliacoes,
  method = "spearman",
  exact = FALSE
)

rho_texto <- formatC(
  unname(teste_h3$estimate),
  format = "f",
  digits = 2,
  decimal.mark = ","
)

formatar_numero <- function(x) {
  format(
    x,
    big.mark = ".",
    decimal.mark = ",",
    scientific = FALSE,
    trim = TRUE
  )
}

# 4. GRUPOS PARA A VISUALIZAÇÃO -------------------------------

ordem_grupos <- c(
  "1", "2", "3", "4", "5",
  "6", "7", "8", "9–12", "13+"
)

base_h3 <- base_h3 |>
  mutate(
    grupo = case_when(
      jogadores <= 8 ~ as.character(jogadores),
      jogadores <= 12 ~ "9–12",
      TRUE ~ "13+"
    ),
    grupo = factor(
      grupo,
      levels = ordem_grupos
    ),
    
    # Transformação explícita para construir as caixas.
    log_avaliacoes = log10(avaliacoes)
  )

# 5. MEDIANAS E TAMANHOS DOS GRUPOS ----------------------------

resumo_h3 <- base_h3 |>
  group_by(grupo) |>
  summarise(
    n = n(),
    mediana = median(avaliacoes),
    .groups = "drop"
  ) |>
  mutate(
    log_mediana = log10(mediana)
  )

# Dois níveis de texto no eixo:
# capacidade máxima e número de jogos no grupo.

rotulos_grupos <- setNames(
  paste0(
    as.character(resumo_h3$grupo),
    "\nn = ",
    formatar_numero(resumo_h3$n)
  ),
  as.character(resumo_h3$grupo)
)

# Marcas do eixo vertical nas unidades originais.

marcas_y <- c(
  30, 100, 300, 1000,
  3000, 10000, 30000, 100000
)

marcas_y <- marcas_y[
  marcas_y >= min(base_h3$avaliacoes) &
    marcas_y <= max(base_h3$avaliacoes)
]

# 6. GRÁFICO -------------------------------------------------

grafico_h3_boxplot <- ggplot(
  base_h3,
  aes(x = grupo, y = log_avaliacoes)
) +
  
  geom_boxplot(
    width = 0.58,
    fill = verde_claro,
    color = verde,
    linewidth = 0.65,
    outlier.color = verde,
    outlier.alpha = 0.12,
    outlier.size = 0.65
  ) +
  
  # Mediana de cada grupo.
  geom_point(
    data = resumo_h3,
    aes(
      x = grupo,
      y = log_mediana
    ),
    inherit.aes = FALSE,
    shape = 21,
    fill = dourado,
    color = papel,
    size = 3.2,
    stroke = 0.6
  ) +
  
  scale_x_discrete(
    labels = rotulos_grupos
  ) +
  
  scale_y_continuous(
    breaks = log10(marcas_y),
    labels = formatar_numero(marcas_y),
    expand = expansion(mult = c(0.04, 0.08))
  ) +
  
  labs(
    title = "Mais jogadores, mais popularidade?",
    subtitle = paste0(
      "Distribuição das avaliações por capacidade máxima",
      "\nSpearman \u03C1 = ",
      rho_texto,
      "   |   ",
      formatar_numero(nrow(base_h3)),
      " jogos"
    ),
    x = "Capacidade máxima de jogadores",
    y = "Número de avaliações (escala logarítmica)",
    caption = paste0(
      "Caixas: 50% centrais. Linha interna e ponto dourado: mediana.",
      "\nHastes: até 1,5 intervalo interquartil na escala logarítmica; ",
      "pontos externos mostram valores além das hastes.",
      "\nAs duas últimas categorias agrupam capacidades. ",
      "Spearman usa os valores individuais originais.",
      "\nFonte: BoardGameGeek / Kaggle — base tratada do grupo."
    )
  ) +
  
  theme_minimal(
    base_size = 13,
    base_family = "sans"
  ) +
  
  theme(
    plot.background = element_rect(
      fill = papel,
      color = NA
    ),
    panel.background = element_rect(
      fill = papel,
      color = NA
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      color = "#E3E2D9",
      linewidth = 0.4
    ),
    plot.title = element_text(
      color = verde,
      face = "bold",
      size = 22,
      margin = margin(b = 10)
    ),
    plot.subtitle = element_text(
      color = cinza,
      size = 12,
      lineheight = 1.3,
      margin = margin(b = 22)
    ),
    axis.title = element_text(
      color = verde,
      face = "bold",
      size = 12
    ),
    axis.title.x = element_text(
      margin = margin(t = 16)
    ),
    axis.title.y = element_text(
      margin = margin(r = 14)
    ),
    axis.text.x = element_text(
      color = verde,
      size = 10,
      lineheight = 1.3,
      margin = margin(t = 8)
    ),
    axis.text.y = element_text(
      color = cinza,
      size = 11
    ),
    plot.caption = element_text(
      color = cinza,
      size = 9,
      hjust = 0,
      lineheight = 1.2,
      margin = margin(t = 20)
    ),
    plot.margin = margin(25, 30, 20, 25)
  )

# 7. SALVAR --------------------------------------------------

pasta_saida <- path.expand(
  "~/Desktop/Jogos_Tabuleiro_Graficos"
)

dir.create(
  pasta_saida,
  recursive = TRUE,
  showWarnings = FALSE
)

arquivo_saida <- file.path(
  pasta_saida,
  "H3_boxplot_popularidade.png"
)

ggsave(
  filename = arquivo_saida,
  plot = grafico_h3_boxplot,
  device = "png",
  width = 13,
  height = 8,
  units = "in",
  dpi = 400,
  bg = papel
)

# Salvar as medianas e o número de jogos por grupo.

write.csv2(
  resumo_h3 |>
    select(grupo, n, mediana),
  file.path(
    pasta_saida,
    "H3_resumo_por_capacidade.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

print(resumo_h3)

# Abrir a figura no Mac.

system2("open", shQuote(arquivo_saida))