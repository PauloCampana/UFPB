library(tidyverse)
library(cluster)
library(factoextra)
library(mclust)

data <- read_csv("energy.csv") |>
    filter(property_type %in% c(
        "Hotel", "K-12 School", "Supermarket/Grocery Store"
    )) |>
    mutate(property_type = factor(property_type,
        labels = c("Hotel", "Escola", "Supermercado"),
    ))

X <- data |>
    mutate(across(area:eui_source_wn, scale)) |>
    select(-property_type, -stars, -score) |>
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

## Classificação Hierárquica
dist_euc <- dist(X, method = "euclidian")
clust <- hclust(dist_euc, method = "ward.D")
par(bg = "#002b36", fg = "#93a1a1", mar = rep(0, 4))
plot(
    clust, hang = 0,
    labels = c("🟨", "🟩", "🟥")[data$property_type],
    axes = FALSE, ann = FALSE
)
rect.hclust(clust, k = 3, border = c("#7cb342", "#f44336", "#ffcc32"))

## Comparação de métodos
sapply(
    c("average", "single", "complete", "ward"),
    function(m) agnes(X, method = m)$ac
)

## Eficiência da classificação
table(data.frame(
    tipo = data$property_type,
    classe = cutree(clust, k = 3)
))

fviz_cluster(
    list(
        data = X,
        cluster = c("Escola", "Supermercado", "Hotel")[cutree(clust, k = 3)]
    ),
    show.clust.cent = FALSE, geom = "point",
    colour = c("#ffcc32", "#7cb342", "#f44336")[data$property_type],
) + scale_color_manual(
    values = c("#7cb342", "#ffcc32", "#f44336"),
    aesthetics = c("colour", "fill")
) + theme_moon

## K-means
km <- data |>
    mutate(across(area:eui_source_wn, scale)) |>
    summarise(across(area:eui_source_wn, mean), .by = property_type) |>
    select(-property_type) |>
    kmeans(X, centers = _)

## Within cluster sum of squares by cluster:
## [1] 2725 1724 2334
##  (between_SS / total_SS =  60.8 %)

## Eficiência da classificação
table(data.frame(
    tipo = data$property_type,
    classe = km$cluster
))

fviz_cluster(
    list(
        data = X,
        cluster = c("Escola", "Hotel", "Supermercado")[km$cluster]
    ),
    show.clust.cent = FALSE, geom = "point",
    colour = c("#ffcc32", "#7cb342", "#f44336")[data$property_type],
) + scale_color_manual(
    values = c("#7cb342", "#ffcc32", "#f44336"),
    aesthetics = c("colour", "fill")
) + theme_moon

## Mistura Gaussiana
bic <- mclustBIC(X)
par(
    bg = "#002b36", fg = "#93a1a1",
    col.axis = "#93a1a1", col.lab = "#93a1a1"
)
plot(bic)

## Eficiência da classificação
vvv <- Mclust(X, G = 3, modelNames = "VVV")
table(data.frame(
    tipo = data$property_type,
    classe = vvv$classification
))

evi <- Mclust(X, G = 3, modelNames = "EVI")
table(data.frame(
    tipo = data$property_type,
    classe = evi$classification
))

## Modelo VVV
fviz_cluster(
    list(
        data = X,
        cluster = c("Escola", "Hotel", "Supermercado")[vvv$classification]
    ),
    show.clust.cent = FALSE, geom = "point",
    colour = c("#ffcc32", "#7cb342", "#f44336")[data$property_type],
) + scale_color_manual(
    values = c("#7cb342", "#ffcc32", "#f44336"),
    aesthetics = c("colour", "fill")
) + theme_moon

## Modelo EVI
fviz_cluster(
    list(
        data = X,
        cluster = c("Escola", "Hotel", "Supermercado")[evi$classification]
    ),
    show.clust.cent = FALSE, geom = "point",
    colour = c("#ffcc32", "#7cb342", "#f44336")[data$property_type],
) + scale_color_manual(
    values = c("#7cb342", "#ffcc32", "#f44336"),
    aesthetics = c("colour", "fill")
) + theme_moon
