library(tidyverse)
library(chromote)
library(rvest)

# Cria uma sessão do Chrome
b <- ChromoteSession$new()

# Navega para a página

b$Page$navigate("https://www.scrapethissite.com/pages/simple/")
Sys.sleep(5)

html <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value

#cat(substr(html, 1, 500), "...\n")

page <- read_html(html)
html_elements(page, ".country") %>% html_element("country-name") %>% html_text2()

page %>% html_elements(".country")


# o map aplica uma função em um vetor

map(1:5, ~ .x/2)

#? Forma moderna equivalente no purrr:
paises_df <- page %>%
  html_elements(".country") %>%
  map(~ tibble(
    pais      = .x %>% html_element(".country-name") %>% html_text2(),
    capital   = .x %>% html_element(".country-capital") %>% html_text2(),
    populacao = .x %>% html_element(".country-population") %>% html_text2(),
    area      = .x %>% html_element(".country-area") %>% html_text2()
  )) %>%
  list_rbind()
#fecha sessão
  b$close

b <- ChromoteSession$new()

webpage <- "https://www.scrapethissite.com/pages/forms/"
b$Page$navigate(webpage)
Sys.sleep(5)

html <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value

page <- read_html(html)

hrefs <- html_elements(page, ".pagination") %>%
html_elements("a") %>%
 html_attr("href") %>%
 unique()
