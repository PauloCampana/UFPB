## Dados

library(tidyverse)

data <- read_csv("includes/icms_amapá.csv") |>
    mutate(data = ym(as.character(data))) |>
    filter(year(data) >= 2000)

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

## Gráfico da série

data |>
    ggplot(aes(x = data, y = icms)) +
    geom_line(color = "#93a1a1") +
    scale_y_continuous(labels = \(x) format(x,
        scientific = FALSE, big.mark = ",", zero.print = FALSE
    )) +
    labs(x = "Ano", y = "ICMS (1000 R$)") +
    theme_moon

## Deflacionando a série

data$icms_def <- deflateBR::deflate(data$icms, data$data, "01/2024")

data |>
    pivot_longer(-data) |>
    ggplot(aes(x = data, y = value, color = name)) +
    geom_line() +
    scale_color_manual(values = c("#93a1a1", "#e3f151")) +
    scale_y_continuous(labels = \(x) format(x,
        scientific = FALSE, big.mark = ",", zero.print = FALSE
    )) +
    labs(x = "Ano", y = "ICMS (1000 R$)") +
    theme_moon

## Interpretação
## Tendência e Sazonalidade

tseries::kpss.test(data$icms)
tseries::adf.test(data$icms)
tseries::pp.test(data$icms)

tseries::kpss.test(data$icms)
tseries::adf.test(data$icms)
tseries::pp.test(data$icms)

## Tendência e Sazonalidade

data2 <- data |>
    filter(year(data) < 2023)
series <- ts(data2$icms, freq = 12, start = c(2000, 1))
series_def <- ts(data2$icms_def, freq = 12, start = c(2000, 1))

par(
    bg = "#002b36", fg = "#93a1a1", cex.axis = 1.25,
    col.axis = "#93a1a1", col.lab = "#93a1a1", col.main = "#93a1a1"
)
plot(stl(series, s.window = "periodic"))

## Tendência e Sazonalidade

par(
    bg = "#002b36", fg = "#93a1a1", cex.axis = 1.25,
    col.axis = "#93a1a1", col.lab = "#93a1a1", col.main = "#93a1a1"
)
plot(stl(series_def, s.window = "periodic"))

##
