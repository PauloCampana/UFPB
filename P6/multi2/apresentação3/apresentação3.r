## Resumo dos dados
library(tidyverse)

data <- read_csv("energy.csv")

X <- data |>
    select(area, ghg_total, electricity, natural_gas) |>
    mutate(across(everything(), scale)) |>
    as.matrix()

Y <- data |>
    select(ghg_intensity, eui_site, eui_source, eui_site_wn, eui_source_wn) |>
    mutate(across(everything(), scale)) |>
    as.matrix()

options(digits = 3)
set.seed(0)

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

## Correlação
data |>
    relocate(ghg_intensity, .before = eui_site) |>
    select(-property_type, -stars, -score) |>
    cor() |>
    corrplot::corrplot(
        method = "ellipse",
        col = viridis::cividis(200),
        tl.col = "#93a1a1",
        tl.pos = "l",
        cl.ratio = 0.25,
    )

## Análise de correlação canônica
cca <- yacca::cca(X, Y)
summary(cca)

## Gráficos
par(
    bg = "#002b36", fg = "#93a1a1",
    col = "#93a1a1", col.axis = "#93a1a1",
    col.main = "#93a1a1", col.lab = "#93a1a1",
    pch = 19
)
yacca::plot.cca(cca)

## Análise de discriminante
data <- read_csv("energy.csv") |>
    relocate(ghg_intensity, .before = eui_site) |>
    filter(property_type %in% c(
        "Hotel", "K-12 School", "Supermarket/Grocery Store"
    )) |>
    mutate(property_type = factor(property_type,
        labels = c("Hotel", "Escola", "Supermercado"),
    ))

## Caso linear
library(MASS)
set.seed(0)

idx <- sample(nrow(data), 500)
train <- data[-idx, ]
test <- data[idx, ]

lda <- lda(property_type ~ . - stars - score, train)

## Acurácia
predict <- predict(lda, test)$class
cm <- caret::confusionMatrix(test$property_type, predict)
cm$table
cm$byClass[,1:2]

## Acurácia
ggord::ggord(
    lda, train$property_type,
    cols = c("#ffcc32", "#7cb342", "#f44336"),
    hull = TRUE, ellipse = FALSE, alpha_el = 0.25,
    arrow = NULL, txt = NULL,
    size = 1.5,
    xlims = c(-5.5, 15.5), ylims = c(-5.5, 15.5),
    coord_fix = FALSE
) + scale_shape_manual(values = c(24, 21, 22)) +
    theme_moon

## Partição dos grupos
par(
    bg = "#002b36", fg = "#93a1a1",
    col = "#93a1a1", col.axis = "#93a1a1",
    col.main = "#93a1a1", col.lab = "#93a1a1"
)
klaR::partimat(
    property_type ~ ghg_total + eui_source_wn,
    data = data, method = "lda",
    image.colors = c("#ffcc3240", "#7cb34240", "#f4433640")
)

## Caso quadrático
qda <- qda(property_type ~ . - stars - score, train)

predict <- predict(qda, test)$class
cm <- caret::confusionMatrix(test$property_type, predict)
cm$table
cm$byClass[,1:2]

## Partição dos grupos
par(
    bg = "#002b36", fg = "#93a1a1",
    col = "#93a1a1", col.axis = "#93a1a1",
    col.main = "#93a1a1", col.lab = "#93a1a1"
)
klaR::partimat(
    property_type ~ ghg_total + eui_source_wn,
    data = data, method = "qda",
    image.colors = c("#ffcc3240", "#7cb34240", "#f4433640")
)
