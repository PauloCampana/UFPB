## Fonte
library(tidyverse)
library(tseries)
library(seastests)

raw <- read_csv(
    "https://data.cityofchicago.org/api/views/k7hf-8y75/rows.csv"
)

## Estação climática
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
)

leaflet::leaflet() |>
    leaflet::addTiles() |>
    leaflet::addMarkers(lng = -87.622817, lat = 41.901997) |>
    leaflet::fitBounds(-90, -45, 90, 45)

## Preparação
data <- raw |>
    filter(`Station Name` == "Oak Street Weather Station") |>
    select(
        date = `Measurement Timestamp`,
        air_temperature = `Air Temperature`,
        humidity = `Humidity`,
        rain_interval = `Interval Rain`,
        pressure = `Barometric Pressure`,
        solar = `Solar Radiation`,
        wet_bulb_temperature = `Wet Bulb Temperature`,
        rain_intensity = `Rain Intensity`,
        rain_total = `Total Rain`,
        rain_type = `Precipitation Type`,
        wind_direction = `Wind Direction`,
        wind_speed = `Wind Speed`,
    ) |>
    mutate(date = mdy_hms(date))
write_csv(data, "chicago.csv")

## Preparação
data23 <- data |>
    filter(year(date) == 2023, month(date) == 10) |>
    arrange(date) |>
    mutate(date = make_datetime(
        year = 2023, month = 10, day = day(date),
        hour = 2 * (hour(date) %/% 2)
    )) |>
    summarise(
        air_temperature = mean(air_temperature),
        .by = date,
    )

## Criando a série
#| output-location: slide
#| out-width: 100%
#| fig-height: 6
#| fig-align: center
series <- data23 |>
    pull(air_temperature) |>
    ts(frequency = 12, start = c(1, 0))

data23 |>
    ggplot(aes(x = date, y = air_temperature)) +
    geom_line(color = "#93a1a1") +
    labs(x = "Dia", y = "Temperatura") +
    theme_moon

## Gráficos de Autocorrelação da série
par(
    bg = "#002b36",
    fg = "#93a1a1",
    col.axis = "#93a1a1",
    col.lab = "#93a1a1",
    col.main = "#93a1a1",
    omi = rep(0, 4),
    mar = rep(3, 4)
)
acf(series, main = "Função de Autocorrelação")
pacf(series, main = "Função de Autocorrelação parcial")

## Decomposição da série
#| fig-height: 6
#| out-width: 100%
#| output-location: slide
dec_add <- decompose(series, type = "additive")
par(
    bg = "#002b36",
    fg = "#93a1a1",
    col.axis = "#93a1a1",
    col.lab = "#93a1a1",
    col.main = "#93a1a1"
)
plot(dec_add)

## Decomposição multiplicativa
#| fig-height: 6
#| out-width: 100%
#| output-location: slide
dec_mul <- decompose(series, type = "multiplicative")
par(
    bg = "#002b36",
    fg = "#93a1a1",
    col.axis = "#93a1a1",
    col.lab = "#93a1a1",
    col.main = "#93a1a1"
)
plot(dec_mul)

##
