library(tidyverse)
library(chromote)
library(rvest)

# Cria uma sessão do Chrome
b <- ChromoteSession$new()

anos <- c(2015, 2014, 2013, 2012, 2011, 2010)

# Cria as URLs
urls_paginas <- paste0(
  "https://www.scrapethissite.com/pages/ajax-javascript/#",
  anos
)

dados <- map(urls_paginas, function(url) {

  print("copiando dados de:")
  print(url)

  # Força o carregamento completo da página
  b$Page$navigate("about:blank")
  b$Page$navigate(url)

  Sys.sleep(3)

  html <- b$Runtime$evaluate(
    "document.documentElement.outerHTML"
  )$result$value

  page <- read_html(html)

  filmes <- page %>%
    html_elements(".film")

  print(length(filmes))

  tibble(
    title = filmes %>%
      html_element(".film-title") %>%
      html_text2(),

    nominacoes = filmes %>%
      html_element(".film-nominations") %>%
      html_text2(),

    awards = filmes %>%
      html_element(".film-awards") %>%
      html_text2(),

    best_picture = !is.na(
      filmes %>%
        html_element(".glyphicon-flag") %>%
        html_attr("class")
    )
  )
})

dados <- list_rbind(dados)

View(dados)