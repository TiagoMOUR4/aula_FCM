library(tidyverse)
dados <- read.csv("../aula_R/Pokemon_full.csv")

df_summary <- dados %>%
    group_by(type) %>%
    summarise(
        media_attack = mean(attack),
        sd_attack = sd(attack),
        media_defense = mean(defense),
        sd_defense = sd(defense)
    )

ggplot(df_summary, aes(x = type)) +
    geom_point(aes(y = media_attack, color = "Ataque"),
               position = position_nudge(x = -0.15), size = 2) +
    geom_errorbar(aes(ymin = media_attack-sd_attack,
                      ymax = media_attack+sd_attack,
                      color = "Ataque"),
                  position = position_nudge(x = -0.15), width = 0) +

    geom_point(aes(y = media_defense, color = "Defesa"),
               position = position_nudge(x = 0.15), size = 2) +
    geom_errorbar(aes(ymin = media_defense-sd_defense,
                      ymax = media_defense+sd_defense,
                      color = "Defesa"),
                  position = position_nudge(x = 0.15), width = 0) +

    labs(x = "Tipo",
         y = "Média e desvio padrão",
         color = "Atributo") +
    theme_bw() +
    theme(
        axis.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "top"
    )

ggsave("./outputs/grafico_aulaR.png", width = 5, height = 4)