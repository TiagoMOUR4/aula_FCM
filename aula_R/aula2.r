#aula2 01/09/2026

library(tidyverse)
dados <- read.csv("Pokemon_full.csv")

#Uso do Regex, a special sequence of characters that defines a search pattern for finding or managing text.

#grepl verifica se existe uma string dentro de outra, retornando true ou false

grepl('saur', 'Venosaur')
grepl('saur', 'Charmander')

x <- c("Venosaur", "BulbaSaur")
grepl("[Ss]aur", x) # [Ss] significa que pode ser tanto S ou s

dados %>%
filter(grepl("saur", name))

#* REGular EXpressions
#* REGEX


grepl("saur", "Venosaur")
grepl("saur", "Charmander")
grepl("saur", c("Venosaur", "Charmander")) %>% any()

grep("saur", c("Venosaur", "Charmander", "Venosaur"))

grepl("saur", "Charmander") # Regex

x <- c("Venosaur", "BulbaSaur")
grepl("[Ss]aur", x) # Regex
grep("[Ss]aur", x) # Regex

x <- c(
  "Amonia",
  "Ferro",
  "Dióxido de enxofre",
  "Dioxido de Enxofre",
  "Manganês",
  "Dióxido  de  Enxofre",
  "dioxido de  Enxofre",
  "dioxidode  Enxofre"
)

# + um ou mais
# * zero ou mais
grepl("[Dd]i[óo]xido *de\\s+[eE]nxofre", x)

gsub("[Dd]i[óo]xido *de\\s+[eE]nxofre", "Dióxido de Enxofre", x)

n <- c("097.765.986-90", "123.765.98-37")
grepl("\\d{3}\\.\\d{3}\\.\\d{3}-\\d{2}", n)

grepl(".", c("a", "b", "c", "0", " "))

dados %>% 
filter(grepl("saur", name))

#função JOIN

df_summary <- dados %>%
    group_by(type) %>%
    summarise(media_type=mean(attack)) %>%
    filter(type != 'grass')

left_join(dados, df_summary) %>% head()

#left_join -> mantem os dados que estiverem à esquerda (literalmente, no caso seria o 'dados')
#right_join -> // à direita 
#inner_join -> mantém o que for comum aos dois
#full_join -> mantém tudo

df1 <- dados %>% slice(1:10)
df2 <- dados %>% slice(11:20)

bind_rows(df1,df2)

# Usando GGPLOT
dados <- read.csv("Pokemon_full.csv")
ggplot(dados, aes(x=attack, y=defense))+ 
geom_point()+
theme_bw()

ggsave("./outputs/grafico_aulaR.png", width = 4, height = 3)
