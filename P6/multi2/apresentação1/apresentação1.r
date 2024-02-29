## Componentes principais
library(tidyverse)

data <- read_csv("includes/energy.csv")
X <- data |>
    select(-property_type, -stars) |>
    as.matrix() |>
    scale()

## Componentes principais
#| echo: false
options(digits = 3)
theme_moon <- theme(
    plot.background = element_rect("#002b36"),
    panel.background = element_blank(),
    panel.grid = element_line("#93a1a120"),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line("#93a1a120"),
    strip.background = element_rect("#104b56"),
    text = element_text(colour = "#93a1a1"),
    axis.text = element_text(colour = "#93a1a1"),
    strip.text = element_text(colour = "#93a1a1"),
    legend.background = element_blank(),
    legend.key = element_blank(),
)

#| collapse: true
pca <- prcomp(X, scale. = TRUE)
summary(pca)

## Gráfico biplot
#| fig-height: 6
#| fig-width: 6.64
#| fig-align: center
#| output-location: slide
factoextra::fviz_pca_var(
    pca,
    repel = TRUE,
    col.var = "cos2",
    col.circle = "#93a1a140"
) +
    scale_color_viridis_c(option = "E", begin = 0.15) +
    labs(title = "") +
    theme_moon

## Análise Fatorial

## 1 fator
#| collapse: true
fat1 <- factanal(X, factors = 1)
fat1

## 2 fatores
#| collapse: true
fat2 <- factanal(X, factors = 2, scores = "regression")
fat2

## Comparação
#| fig-height: 6
#| out-width: 100%
#| output-location: slide
library(ggfortify)
data8 <- data |>
    mutate(property_type = fct_lump_n(property_type, 8))

autoplot(
    pca, data8,
    colour = "property_type",
    loadings = TRUE,
    loadings.label = TRUE,
    loadings.label.repel = TRUE,
    loadings.colour = "#93a1a180",
    loadings.label.colour = "#93a1a1",
) + theme_moon + theme(legend.position = "top")

## Comparação
#| fig-height: 6
#| out-width: 100%
#| output-location: slide
autoplot(
    fat2, data8,
    colour = "property_type",
    loadings = TRUE,
    loadings.label = TRUE,
    loadings.label.repel = TRUE,
    loadings.colour = "#93a1a180",
    loadings.label.colour = "#93a1a1",
) + theme_moon + theme(legend.position = "top")

##
