# O que faz um jogo de tabuleiro ser bem avaliado?
# Hipóteses 4, 5 e 6 | Lorena Duarte Campos
# Dados tratados do BoardGameGeek: https://doi.org/10.21227/9g61-bs59


library(ggplot2)

# 1. Importar os dados -------------------------------------------------------

arquivo <- file.choose()
dados <- read.csv2(arquivo, na.strings = c("", "NA"),
                   fileEncoding = "UTF-8-BOM")

# Selecionar colunas usadas e dar nomes mais curtos para a análise.
colunas <- c("Year.Published", "Complexity.Average", "Play.Time",
             "Users.Rated", "Max.Players", "Domains")
dados <- dados[, colunas]
names(dados) <- c("ano", "complexidade", "duracao", "avaliacoes",
                  "max_jogadores", "dominios")

# Cada análise usa todos os pares válidos para suas duas variáveis.** desconsidera os dados que uma das variaveis faltando
# Ano de coleta = 2021, pois foi o ano que a autora publicou o banco de dados
ano_coleta <- 2021
complexidade_valida <- with(dados, complexidade >= 1 & complexidade <= 5)
ano_valido <- with(dados, is.finite(ano) & ano > 0 & ano <= ano_coleta)
h4 <- subset(dados, complexidade_valida & ano_valido)
h5 <- subset(dados, complexidade_valida & is.finite(duracao) & duracao > 0)
h6 <- subset(dados, complexidade_valida & is.finite(avaliacoes) & avaliacoes > 0)
h6_ano <- subset(dados, complexidade_valida & ano_valido &
                   is.finite(avaliacoes) & avaliacoes > 0)

# 2. Correlação em cada hipótese ---------------------------------
# Spearman compara a ordem dos jogos nas duas variáveis. Rho positivo
# indica que valores maiores de uma tendem a acompanhar valores maiores
# da outra. O teste é bilateral e usa p aproximado devido aos empates.
spearman <- function(x, y, hipotese, analise) {
  teste <- cor.test(x, y, method = "spearman", exact = FALSE)
  data.frame(hipotese, analise, n = length(x), rho = unname(teste$estimate),
             p_valor = teste$p.value)
}

# H4 — Ano de publicação e complexidade
res4 <- with(h4, spearman(ano, complexidade, "H4", "Principal"))

# H5 — Complexidade e duração
res5 <- with(h5, spearman(complexidade, duracao, "H5", "Principal"))

# H6 — Complexidade e número de avaliações
res6 <- with(h6, spearman(complexidade, avaliacoes, "H6", "Principal"))

# 3. Análises complementares ---------------------------------------

# H4 — Repete a análise dentro de cada domínio.

dominios_escolhidos <- c("Wargames", "Strategy Games", "Family Games")
h4_dominios <- lapply(dominios_escolhidos, function(dominio) {
  grupo <- subset(h4, grepl(dominio, dominios, fixed = TRUE))
  with(grupo, spearman(ano, complexidade, "H4", dominio))
})
res4_dominios <- do.call(rbind, h4_dominios)

# H5 — Repete a análise até o percentil 99 da duração.
p99 <- unname(quantile(h5$duracao, 0.99))
h5_p99 <- subset(h5, duracao <= p99)
res5_p99 <- with(h5_p99, spearman(complexidade, duracao, "H5",
                                  sprintf("Duração até o P99 (%g min)", p99)))

# H5 — Repete por faixa de número máximo de jogadores.
h5_jogadores <- subset(h5, max_jogadores > 0)
h5_jogadores$faixa <- cut(
  h5_jogadores$max_jogadores, breaks = c(0, 2, 4, Inf),
  labels = c("1–2", "3–4", "5 ou mais")
)
grupos_h5 <- split(h5_jogadores, h5_jogadores$faixa, drop = TRUE)
res5_jogadores <- do.call(rbind, lapply(names(grupos_h5), function(faixa) {
  with(grupos_h5[[faixa]], spearman(complexidade, duracao, "H5", faixa))
}))

# H6 — Compara a correlação antes e depois do ajuste por ano nas mesmas
# linhas. Retira de cada posto a média do seu ano e correlaciona o que
# sobra. O coeficiente ajustado é apresentado de forma descritiva.
res6_ano <- with(h6_ano, spearman(complexidade, avaliacoes, "H6",
                                  "Bruta na amostra com ano válido"))
posto_complexidade <- rank(h6_ano$complexidade)
posto_avaliacoes <- rank(h6_ano$avaliacoes)
posto_complexidade_ajustado <- posto_complexidade -
  ave(posto_complexidade, h6_ano$ano)
posto_avaliacoes_ajustado <- posto_avaliacoes -
  ave(posto_avaliacoes, h6_ano$ano)
rho_ajustado <- cor(posto_complexidade_ajustado, posto_avaliacoes_ajustado)
res6_ajuste <- data.frame(hipotese = "H6",
                          analise = "Correlação dos postos ajustada por ano",
                          n = nrow(h6_ano), rho = rho_ajustado, p_valor = NA_real_)

# H6 — Repete por faixa de duração em minutos.
h6_duracao <- subset(h6, duracao > 0)
h6_duracao$faixa <- cut(
  h6_duracao$duracao, breaks = c(0, 30, 60, 120, Inf),
  labels = c("Até 30 min", ">30 a 60 min", ">60 a 120 min", ">120 min")
)
grupos_h6 <- split(h6_duracao, h6_duracao$faixa, drop = TRUE)
res6_duracao <- do.call(rbind, lapply(names(grupos_h6), function(faixa) {
  with(grupos_h6[[faixa]], spearman(complexidade, avaliacoes, "H6", faixa))
}))

resultados <- rbind(
  res4, res4_dominios,
  res5, res5_p99, res5_jogadores,
  res6, res6_ano, res6_ajuste, res6_duracao
)

# Formatação
n_br <- function(x) format(x, big.mark = ".", scientific = FALSE, trim = TRUE)
rho_br <- function(x) formatC(x, digits = 3, format = "f", decimal.mark = ",")
p_texto <- function(x) {
  ifelse(is.na(x), "", ifelse(x < 0.001, "<0,001", formatC(
    x, digits = 3, format = "f", decimal.mark = ","
  )))
}
resultados$p_valor <- p_texto(resultados$p_valor)
print(resultados, row.names = FALSE)
write.csv2(resultados, "resultados_h4_h5_h6.csv", row.names = FALSE,
           fileEncoding = "UTF-8")

# 4. Resumo das distribuições para visualizar ---------------------------

resumir <- function(grupos, valores) {
  grupos <- split(valores, grupos, drop = TRUE)
  data.frame(
    grupo = factor(names(grupos), levels = names(grupos)),
    n = lengths(grupos),
    q1 = vapply(grupos, function(x) quantile(x, 0.25), numeric(1)),
    mediana = vapply(grupos, median, numeric(1)),
    q3 = vapply(grupos, function(x) quantile(x, 0.75), numeric(1)),
    row.names = NULL
  )
}

periodos <- c("Antes de 1970", "1970–79", "1980–89", "1990–99",
              "2000–09", "2010–19", "2020–21")
periodo <- cut(h4$ano, c(-Inf, 1970, 1980, 1990, 2000, 2010, 2020, 2022),
               labels = periodos, right = FALSE)
dist4 <- resumir(periodo, h4$complexidade)
print(dist4, row.names = FALSE)

# Meio ponto por faixa. Para faciliar a visualizacao
faixas <- c("1–<1,5", "1,5–<2", "2–<2,5", "2,5–<3",
            "3–<3,5", "3,5–<4", "4–<4,5", "4,5–5")
faixa_complexidade <- function(x) {
  cut(x, c(seq(1, 4.5, by = 0.5), Inf), labels = faixas, right = FALSE)
}
dist5 <- resumir(faixa_complexidade(h5$complexidade), h5$duracao)
dist6 <- resumir(faixa_complexidade(h6$complexidade), h6$avaliacoes)
print(dist5, row.names = FALSE)
print(dist6, row.names = FALSE)

# 5. Gráficos  --------------------------------------------

fundo <- "#FBFAF6"
texto <- "#172C38"
legenda <- "Pontos: mediana   |   Intervalos: 50% centrais   |   Fonte: BoardGameGeek"
tema <- theme_minimal(base_size = 15) +
  theme(
    plot.background = element_rect(fill = fundo, colour = NA),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(face = "bold", size = 23, colour = texto),
    plot.subtitle = element_text(size = 13, colour = "#435966",
                                 margin = margin(t = 5, b = 18)),
    plot.caption = element_text(hjust = 0, colour = "#435966", size = 10,
                                margin = margin(t = 16)),
    axis.title = element_text(face = "bold", colour = texto),
    axis.title.x = element_text(margin = margin(t = 9)),
    axis.text = element_text(colour = "#36505D", size = 11),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(colour = "#DFE5E2"),
    plot.margin = margin(24, 32, 22, 28)
  )

rotulos <- function(resumo) {
  setNames(paste0(resumo$grupo, "\nn = ", n_br(resumo$n)), resumo$grupo)
}

identificacao <- function(r) {
  paste0(r$hipotese, "  •  Spearman ρ = ", rho_br(r$rho))
}

g4 <- ggplot(dist4, aes(grupo, mediana)) +
  geom_linerange(aes(ymin = q1, ymax = q3), colour = "#86BDB2", linewidth = 3) +
  geom_point(shape = 21, fill = "#126C64", colour = fundo, size = 5, stroke = 1) +
  scale_x_discrete(labels = rotulos(dist4)) +
  scale_y_continuous(breaks = 1:5, limits = c(1, 5)) +
  labs(title = "Jogos mais antigos são menos complexos?",
       subtitle = identificacao(res4), x = "Período de publicação",
       y = "Complexidade média no BGG (1–5)", caption = legenda) +
  tema

grafico_faixas <- function(resumo, cor, cor_faixa, titulo, resultado,
                           eixo_y, marcas) {
  ggplot(resumo, aes(grupo, mediana, group = 1)) +
    geom_ribbon(aes(ymin = q1, ymax = q3), fill = cor_faixa) +
    geom_line(colour = cor, linewidth = 1.15) +
    geom_point(shape = 21, fill = cor, colour = fundo, size = 4.3, stroke = 1) +
    scale_x_discrete(labels = rotulos(resumo)) +
    scale_y_log10(breaks = marcas, labels = n_br) +
    labs(title = titulo, subtitle = identificacao(resultado),
         x = "Faixa de complexidade média no BGG",
         y = eixo_y, caption = legenda) +
    tema
}

g5 <- grafico_faixas(dist5, "#A4452C", "#F0D9CB",
                     "Jogos mais complexos duram mais?",
                     res5, "Duração (min, escala log)", c(15, 30, 60, 120, 240, 480, 960))
g6 <- grafico_faixas(dist6, "#394D8C", "#DCE2F3",
                     "Jogos mais complexos recebem mais avaliações?",
                     res6, "Número de avaliações (escala log)", c(30, 60, 120, 240, 480, 960, 1920))

# O quarto gráfico usa as mesmas estimativas já calculadas para a tabela.
rotulos_h5 <- setNames(
  paste0(res5_jogadores$analise, "\nn = ", n_br(res5_jogadores$n)),
  res5_jogadores$analise
)
res5_jogadores$valor <- formatC(
  res5_jogadores$rho, digits = 2, format = "f", decimal.mark = ","
)

grafico_h5_extra <- ggplot(res5_jogadores, aes(x = rho, y = analise)) +
  geom_segment(aes(x = 0, xend = rho, yend = analise),
               colour = "#F0D9CB", linewidth = 1.1) +
  geom_point(shape = 21, size = 5.4, fill = "#A4452C",
             colour = fundo, stroke = 1) +
  geom_text(aes(label = valor), nudge_x = 0.032, hjust = 0,
            colour = "#A4452C", fontface = "bold", size = 4.4) +
  scale_x_continuous(
    limits = c(0, 1), breaks = seq(0, 1, by = 0.25),
    labels = c("0", "0,25", "0,50", "0,75", "1"),
    expand = expansion(mult = 0)
  ) +
  scale_y_discrete(limits = rev(names(grupos_h5)), labels = rotulos_h5) +
  labs(
    title = "Jogos mais complexos tendem a durar mais",
    subtitle = "H5 complementar  •  Comparação por número máximo de jogadores",
    x = "Correlação de Spearman (ρ)",
    y = NULL,
    caption = "Cada ponto representa uma faixa. Fonte · BoardGameGeek"
  ) +
  tema +
  theme(
    axis.text.y = element_text(lineheight = 0.95, margin = margin(r = 10)),
    panel.grid.major.x = element_line(colour = "#DFE5E2", linewidth = 0.45),
    panel.grid.major.y = element_blank()
  )

# 6. Salvar gráficos em PNG ----------------------------------------

dir.create("graficos", showWarnings = FALSE)
graficos <- list(
  h4_ano_complexidade = g4,
  h5_complexidade_duracao = g5,
  h5_complementar_por_jogadores = grafico_h5_extra,
  h6_complexidade_popularidade = g6
)

for (nome in names(graficos)) {
  ggsave(
    file.path("graficos", paste0(nome, ".png")), plot = graficos[[nome]],
    width = 11, height = 6.2, dpi = 300, bg = fundo
  )
}
message("Tabela em resultados_h4_h5_h6.csv e figuras na pasta graficos/.")

getwd()
