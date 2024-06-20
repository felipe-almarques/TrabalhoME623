################################################################################
#############                   Trabalho Final                    #############
################################################################################

## Pacotes
library(tidyverse)
library(ggstats)
library(patchwork)

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
# Proporção de pipoca estourada

p1 <- dados %>% 
  group_by(oleo) %>% 
  reframe(soma = sum(milho)) %>% 
  mutate(prop = soma / sum(soma)) %>% 
  ggplot(aes(x = oleo, y = prop)) +
  geom_bar(stat = "identity", fill = "lightblue3") +
  scale_y_continuous(labels = scales::percent, limits = c(0, .55)) +
  labs(x = "Tipo de Óleo", y = "") +
  geom_text(
    aes(
      label = scales::percent(prop, accuracy = .1),
      y = prop
    ),
    vjust = -.5,
    stat = "identity"
  ) +
  theme_bw()

p2 <- dados %>% 
  group_by(marca) %>% 
  reframe(soma = sum(milho)) %>% 
  mutate(prop = soma / sum(soma)) %>% 
  ggplot(aes(x = marca, y = prop)) +
  geom_bar(stat = "identity", fill = "lightblue3") +
  scale_y_continuous(labels = scales::percent, limits = c(0, .55)) +
  labs(x = "Marca da Pipoca", y = "") +
  geom_text(
    aes(
      label = scales::percent(prop, accuracy = .1),
      y = prop
    ),
    vjust = -.5,
    stat = "identity"
  ) +
  theme_bw()
  
graf <- p1 + p2
gt <- patchworkGrob(graf)

pdf(file = "artifacts/graf_prop.pdf", height = 6, width = 8)
gridExtra::grid.arrange(gt, left = "Quantidade de milho não estourado")
dev.off()

# Box-plots

b1 <- dados %>% 
  ggplot() +
  geom_boxplot(aes(x = marca, y = milho, fill = marca)) +
  labs(x = "Marca da Pipoca", y = "",
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


b2 <- dados %>% 
  ggplot() +
  geom_boxplot(aes(x = oleo, y = milho, fill = oleo)) +
  labs(x = "Tipo de Óleo", y = "",
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

b3 <- dados %>% 
  ggplot() +
  geom_boxplot(aes(x = casa, y = milho, fill = casa)) +
  labs(x = "Casa", y = "",
       fill = "Casa") +
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


graf <- b1 + b2 + b3
gt <- patchworkGrob(graf)

pdf(file = "artifacts/graf_boxplots.pdf", width = 9.5, height = 6)
gridExtra::grid.arrange(gt, left = "Quantidade de milho não estourado")
dev.off()

pdf(file = "artifacts/graf_boxplots_pipocas.pdf", width = 8, height = 6)
dados %>% 
  ggplot() +
  geom_boxplot(aes(x = oleo, y = milho, fill = oleo)) +
  theme_bw() +
  facet_wrap(~marca) + 
  labs(y = "Quantidade de milho não estourada",
       x = "Tipo de Óleo utilizado") + 
  theme(legend.position = "none")
dev.off()

dados %>% 
  ggplot() +
  geom_boxplot(aes(x = marca, y = milho, fill = marca)) +
  theme_bw() +
  facet_wrap(~oleo) + 
  labs(y = "Quantidade de milho não estourada",
       x = "Marca da Pipoca") +
  theme(legend.position = "none")

# Gráfico dos perfis médios (Interação)

pdf(file = "artifacts/graf_perfis.pdf", width = 8, height = 6)
dados %>% 
  group_by(oleo, marca) %>% 
  reframe(media = mean(milho)) %>% 
  mutate(id = row_number()) %>% 
  ggplot(aes(x = oleo, y = media, color = marca, group = marca)) +
  geom_point(size = 2) +
  geom_line(aes(linetype = marca), size = 1) +
  labs(y = "Quantidade de milho não estourada",
       x = "Tipo de Óleo utilizado") + 
  theme_bw()
dev.off()

#### ANOVA

modelo <- lm(milho ~ oleo*marca + casa, data = dados)
anova(modelo)

modelo2 <- lm(milho ~ oleo + marca + casa, data = dados)
anova(modelo2)

# O modelo aponta que o óleo é importante, mas a marca não faz muita diferença

anova(modelo2, modelo)


#### Diagnóstico
# qqplot - Resíduos padronizados
pdf(file = "artifacts/graf_qqplot.pdf", height = 6, width = 8)
data.frame(erros = modelo2$residuals / sqrt(50.518), 
           preditos = modelo2$fitted.values) %>% 
  ggplot(aes(sample = erros)) + 
  stat_qq(color = "blue") + 
  stat_qq_line() + 
  labs(x = "Resíduos Padronizados",
       y = "Quantis Teóricos") +
  theme_bw()
dev.off()

# resíduos vs Ordem de coleta
pdf(file = "artifacts/graf_ordem.pdf", height = 6, width = 8)
dados %>% 
  mutate(residuos = modelo2$residuals) %>% 
  ggplot(aes(x = id, y = residuos)) + 
  geom_point() + 
  geom_line() + 
  labs(x = "Ordem de coleta dos dados",
       y = "Resíduos") + 
  theme_bw()
dev.off()

data.frame(res = modelo2$residuals, ajust = modelo2$fitted.values) %>% 
  ggplot(aes(x = ajust, y = res)) + 
  geom_point() # Indícios de variância não constante

# modelo log milho
modelo3 <- lm(log(milho) ~ oleo + marca + casa, data = dados)
anova(modelo3)

pdf(file = "artifacts/graf_resXajust.pdf", width = 8, height = 6)
data.frame(res = modelo3$residuals, ajust = modelo3$fitted.values) %>% 
  ggplot(aes(x = ajust, y = res)) + 
  geom_point() +
  labs(x = "Valores ajustados",
       y = "Resíduos") + 
  theme_bw()
dev.off()
