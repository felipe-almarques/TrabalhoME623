################################################################################
#############                   Trabalho Final                    #############
################################################################################

## Pacotes
library(tidyverse)
library(ggstats)

## Dados
dados <- read_csv("artifacts/Experimento - Oficial.csv")
colnames(dados) <- c("id", "milho", "oleo", "marca", "casa")
dados$oleo[!(dados$oleo == "Margarina" | dados$oleo == "Azeite")] <- "Óleo"
dados <- dados[!(dados$marca == "Kodilar Premium"),]
dados$marca[dados$marca == "Carrefur Classic"] <- "Carrefour"
dados$oleo <- factor(dados$oleo)
dados$marca <- factor(dados$marca)
dados$casa <- factor(dados$casa)

glimpse(dados)

## Analise Descritiva
# Milho x Óleo 

dados %>% 
  group_by(oleo) %>% 
  reframe(soma = sum(milho)) %>% 
  mutate(prop = soma / sum(soma)) %>% 
  ggplot(aes(x = oleo, y = prop)) +
  geom_bar(stat = "identity", fill = "lightblue3") +
  scale_y_continuous(labels = scales::percent, limits = c(0, .55)) +
  labs(x = "Tipo de Óleo", y = "Quantidade de milho não estourado") +
  geom_text(
    aes(
      label = scales::percent(prop, accuracy = .1),
      y = prop
    ),
    vjust = -.5,
    stat = "identity"
  ) +
  theme_bw()

dados %>% 
  ggplot() +
  geom_boxplot(aes(x = oleo, y = milho, fill = oleo)) +
  labs(x = "Tipo de Óleo", y = "Quantidade de milho não estourado",
       fill = "Tipo de Óleo") +
  theme_bw() +
  theme(
    legend.position = "none",
    legend.title = element_text(face = "bold"),
    legend.background = element_rect(
      fill = "lightgray",
      linewidth = 1,
      color = "gray"
    )
  )
  
# Milho x Pipoca










