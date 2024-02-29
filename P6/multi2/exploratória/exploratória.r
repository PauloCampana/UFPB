## Carregando os dados
library(tidyverse)

raw <- read_csv("https://data.cityofchicago.org/api/views/xq83-jr8c/rows.csv")

## Seleção de variáveis
data <- raw |>
    select(
        property_type = `Primary Property Type`,
        stars         = `Chicago Energy Rating`,
        score         = `ENERGY STAR Score`,
        area          = `Gross Floor Area - Buildings (sq ft)`,
        ghg_total     = `Total GHG Emissions (Metric Tons CO2e)`,
        ghg_intensity = `GHG Intensity (kg CO2e/sq ft)`,
        electricity   = `Electricity Use (kBtu)`,
        natural_gas   = `Natural Gas Use (kBtu)`,
        eui_site      = `Site EUI (kBtu/sq ft)`,
        eui_source    = `Source EUI (kBtu/sq ft)`,
        eui_site_wn   = `Weather Normalized Site EUI (kBtu/sq ft)`,
        eui_source_wn = `Weather Normalized Source EUI (kBtu/sq ft)`,
    )

## Limpeza
data <- data |>
    drop_na(everything()) |>
    mutate(
        property_type = fct_reorder(
            property_type,
            eui_source_wn,
            .fun = mean,
            .desc = TRUE,
        ),
        stars = as.factor(stars),
    )

write_csv(data, "energy.csv")

X <- data |>
    select(-property_type, -stars) |>
    as.matrix() |>
    scale()

## Correlação
#| output: false
#| fig-height: 6
#| out-width: 100%
X |>
    cor() |>
    corrplot::corrplot(
        method = "ellipse",
        col = viridis::cividis(200),
        tl.col = "black",
        tl.pos = "l",
        cl.ratio = 0.25,
    )

#| echo: false
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
)

## Normalidade
#| fig-height: 6
#| out-width: 100%
#| output-location: slide
data |>
    pivot_longer(-c(property_type, stars)) |>
    ggplot(aes(x = value)) +
    facet_wrap(vars(name), nrow = 5, scales = "free", strip.position = "right") +
    geom_density(color = "#93a1a1") +
    labs(x = "Valor", y = "Densidade") +
    theme(axis.text.y = element_blank()) +
    theme_moon

## Relação com o tipo de propriedade
#| fig-height: 6
#| out-width: 100%
#| output-location: slide
data |>
    summarise(
        eui_source_wn = mean(eui_source_wn),
        .by = property_type,
    ) |>
    ggplot(aes(x = property_type, y = eui_source_wn, fill = property_type)) +
    geom_col() +
    scale_fill_viridis_d(option = "E", direction = -1, begin = 0.1) +
    labs(x = "", y = "Uso normalizado de energia") +
    theme(
        axis.text.x = element_text(angle = 60, hjust = 1, size = 12),
        legend.position = "none"
    ) +
    theme_moon

## Relação entre a pontuação e as variáveis contínuas
#| fig-height: 6
#| out-width: 100%
#| output-location: slide
data |>
    pivot_longer(-c(property_type, stars, score)) |>
    summarise(
        value = mean(value),
        .by = c(score, name),
    ) |>
    ggplot(aes(x = score, y = value)) +
    facet_wrap(vars(name), scales = "free") +
    geom_line(color = "#93a1a1") +
    labs(x = "Pontuação", y = "Valor") +
    theme_moon

##
