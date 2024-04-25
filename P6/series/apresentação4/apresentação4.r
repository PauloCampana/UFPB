
## Fonte
library(tidyverse)
library(tseries)
library(seastests)
library(forecast)

## Estação climática
options(digits = 7)

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
    leaflet::setView(lng = -87.622817, lat = 41.901997, zoom = 9)

## Preparação
data <- read_csv("chicago.csv") |>
    filter(year(date) == 2023, month(date) == 10)

## Preparação
all_time <- make_datetime(
    year = 2023, month = 10,
    day = rep(1:31, each = 24),
    hour = rep(0:23, times = 24)
)

data <- data |>
    right_join(tibble(all_time), by = join_by(date == all_time)) |>
    arrange(date) |>
    mutate(across(
        where(is.numeric),
        \(x) as.numeric(zoo::na.approx(zoo::zoo(x, date)))
    ))

## Série
series <- data |>
    pull(air_temperature) |>
    ts(frequency = 24, start = c(1, 0))

data |>
    ggplot(aes(x = date, y = air_temperature)) +
    geom_line(color = "#93a1a1") +
    labs(x = "Dia", y = "Temperatura") +
    theme_moon

## Decomposição clássica
dec <- decompose(series, type = "additive")
par(
    bg = "#002b36", fg = "#93a1a1",
    col.axis = "#93a1a1", col.lab = "#93a1a1", col.main = "#93a1a1"
)
plot(dec)

kpss.test(series)
adf.test(series)
pp.test(series)

seastests::fried(series)
seastests::kw(series)
seastests::welch(series)

## Modelos SARIMA
sarima_aicc <- auto.arima(series)
sarima_aicc
lmtest::coeftest(sarima_aicc)

## Modelos SARIMA
sarima <- Arima(
    series,
    order = c(2, 1, 0), seasonal = c(1, 0, 1)
)
sarima
lmtest::coeftest(sarima)

## Modelo SARIMAX
sarimax <- Arima(
    series, xreg = data$humidity,
    order = c(2, 1, 0), seasonal = c(1, 0, 1)
)
sarimax
lmtest::coeftest(sarimax)

Box.test(residuals(sarima), type = "Box-Pierce")
Box.test(residuals(sarima), type = "Ljung-Box")
Box.test(residuals(sarimax), type = "Box-Pierce")
Box.test(residuals(sarimax), type = "Ljung-Box")

nortest::lillie.test(residuals(sarima))
shapiro.test(residuals(sarima))
nortest::lillie.test(residuals(sarimax))
shapiro.test(residuals(sarimax))


## Suposições dos modelos SARIMA e SARIMAX
data.frame(
    SARIMA = scale(residuals(sarima)),
    SARIMAX = scale(residuals(sarimax))
) |>
    pivot_longer(everything()) |>
    ggplot(aes(x = value)) +
    facet_grid(cols = vars(name)) +
    geom_histogram(aes(y = after_stat(density)), fill = "#00000000", color = "#93a1a1") +
    geom_function(fun = dnorm, color = "#ff4040", alpha = 0.75) +
    labs(x = "Resíduos", y = "Densidade") +
    theme_moon +
    theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
    )

## Previsões
verdadeiro <- read_csv("chicago.csv") |>
    filter(year(date) == 2023, month(date) == 11, day(date) == 1)

## Previsão com modelos SARIMA e SARIMAX
pred_sarima <- lapply(
    c(3, 6, 12, 24),
    \(h) forecast(sarima, h)$mean
)
pred_sarimax <- lapply(
    c(3, 6, 12, 24),
    \(h) forecast(sarimax, h, xreg = verdadeiro$humidity[1:h])$mean
)

## Previsão com método Holt-Winters
hw <- hw(
    series, h = 24,
    seasonal = "additive",
    damped = TRUE,
)
pred_hw <- lapply(
    c(3, 6, 12, 24),
    \(h) forecast(hw, h = h)$mean
)

## Previsão com modelo ETS
ets <- ets(series)
ets
pred_ets <- lapply(
    c(3, 6, 12, 24),
    \(h) forecast(ets, h = h)$mean
)

## Previsão com métodos simples
snaive <- snaive(series, h = 24)
pred_snaive <- lapply(
    c(3, 6, 12, 24),
    \(h) forecast(snaive, h = h)$mean
)

sma <- smooth::sma(series, h = 24)
pred_sma <- lapply(
    c(3, 6, 12, 24),
    \(h) forecast(sma, h = h)$mean
)

## Comparação por RMSE
library(yardstick)
truth <- list(
    verdadeiro$air_temperature[1:3],
    verdadeiro$air_temperature[1:6],
    verdadeiro$air_temperature[1:12],
    verdadeiro$air_temperature[1:24]
)

data.frame(
    sarima  = sapply(1:4, \(i) rmse_vec(truth[[i]], pred_sarima[[i]])),
    sarimax = sapply(1:4, \(i) rmse_vec(truth[[i]], pred_sarimax[[i]])),
    hw      = sapply(1:4, \(i) rmse_vec(truth[[i]], pred_hw[[i]])),
    ets     = sapply(1:4, \(i) rmse_vec(truth[[i]], pred_ets[[i]])),
    snaive  = sapply(1:4, \(i) rmse_vec(truth[[i]], pred_snaive[[i]])),
    sma     = sapply(1:4, \(i) rmse_vec(truth[[i]], pred_sma[[i]]))
) |>
    t() |>
    knitr::kable()

## Comparação por MASE
data.frame(
    sarima  = sapply(1:4, \(i) mase_vec(truth[[i]], pred_sarima[[i]])),
    sarimax = sapply(1:4, \(i) mase_vec(truth[[i]], pred_sarimax[[i]])),
    hw      = sapply(1:4, \(i) mase_vec(truth[[i]], pred_hw[[i]])),
    ets     = sapply(1:4, \(i) mase_vec(truth[[i]], pred_ets[[i]])),
    snaive  = sapply(1:4, \(i) mase_vec(truth[[i]], pred_snaive[[i]])),
    sma     = sapply(1:4, \(i) mase_vec(truth[[i]], pred_sma[[i]]))
) |>
    t() |>
    knitr::kable()

## Conclusão
verdadeiro |>
    mutate(sarima = as.numeric(pred_sarimax[[4]])) |>
    ggplot(aes(x = date, y = air_temperature)) +
    geom_line(color = "#93a1a1") +
    geom_line(aes(y = sarima), color = "#ff4040") +
    labs(x = "Hora", y = "Temperatura") +
    theme_moon
