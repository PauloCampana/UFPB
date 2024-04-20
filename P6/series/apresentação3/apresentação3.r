library(tidyverse)
library(tidymodels)
library(modeltime)
library(fable)
library(fabletools)
library(timetk)
library(smooth)

theme_set(theme_bw())

## Dados
data <- read_csv("drinks.csv") |>
    rename(date = DATE, sales = S4248SM144NCEN) |>
    filter(year(date) < 2020)
antes <- data |>
    filter(year(date) < 2019)
depois <- data |>
    filter(year(date) == 2019)

## Gráfico da série
data |>
    ggplot(aes(x = date, y = sales)) +
    geom_line() +
    theme_bw()

## Naïve
serie_antes <- ts(antes$sales, frequency = 12)
naive <- forecast::naive(serie_antes, h = 12)
autoplot(naive)

## Naïve
rmse_vec(depois$sales, naive$mean)
mae_vec(depois$sales, naive$mean)

## Naïve sazonal
naive_sazonal <- forecast::snaive(serie_antes, h = 12)
autoplot(naive_sazonal)
previsao <- forecast(naive_sazonal, h = 12)

## Naïve sazonal
rmse_vec(depois$sales, naive_sazonal$mean)
mae_vec(depois$sales, naive_sazonal$mean)

## Média global
media_global <- depois
media_global$sales <- mean(antes$sales)

data |>
    ggplot(aes(x = date, y = sales)) +
    geom_line() +
    geom_line(
        data = media_global,
        aes(x = date, y = sales),
        color = "red", linewidth = 1,
    ) +
    theme_bw()

## Média global
rmse_vec(depois$sales, media_global$sales)
mae_vec(depois$sales, media_global$sales)

## Média móvel simples
n <- nrow(data)
media_movel <- depois
media_movel$sales <- smooth::sma(antes$sales, order = 6, h = 12)$forecast

data |>
    ggplot(aes(x = date, y = sales)) +
    geom_line() +
    geom_line(
        data = media_movel,
        aes(x = date, y = sales),
        color = "red", size = 1,
    ) +
    theme_bw()

## Média móvel simples
rmse_vec(depois$sales, media_movel$sales)
mae_vec(depois$sales, media_movel$sales)


## Tendência global
modelo <- alm(sales ~ trend, data, subset = 1:324)
previsoes <- modelo |>
    forecast(tail(data, 12), h = 12) |>
    (\(x) {
        novo <- depois
        novo$sales <- x$mean
        novo
    })()

data |>
    ggplot(aes(x = date, y = sales)) +
    geom_line() +
    geom_line(
        data = previsoes,
        aes(x = date, y = sales),
        color = "red", linewidth = 1,
    ) +
    theme_bw()

## Tendência global
rmse_vec(depois$sales, previsoes$sales)
mae_vec(depois$sales, previsoes$sales)

## Suavização exponencial de Holt
model_fit_ets <- exp_smoothing(
    seasonal_period = 12, season = "none",
    trend = "multiplicative",
    error = "auto"
) |>
    set_engine("ets") |>
    fit(sales ~ date, data = antes)
model_tbl <- modeltime_table(model_fit_ets)

model_tbl |>
    modeltime_calibrate(new_data = depois) |>
    modeltime_forecast(
        new_data = depois,
        actual_data = data
    ) |>
    plot_modeltime_forecast(.interactive = FALSE, .title = "")

## Suavização exponencial de Holt
previsoes <- model_tbl |>
    modeltime_calibrate(new_data = depois)
rmse_vec(depois$sales, previsoes$.calibration_data[[1]]$.prediction)
mae_vec(depois$sales, previsoes$.calibration_data[[1]]$.prediction)


## Holt-Winters
model_fit_ets <- exp_smoothing(
    seasonal_period = 12,
    season = "multiplicative",
    trend = "multiplicative",
    error = "auto",
) |>
    set_engine("ets") |>
    fit(sales ~ date, data = antes)
model_tbl <- modeltime_table(model_fit_ets)

model_tbl |>
    modeltime_calibrate(new_data = depois) |>
    modeltime_forecast(
        new_data = depois,
        actual_data = data
) |>
plot_modeltime_forecast(.interactive = FALSE, .title = "")

## Holt-Winters
previsoes <- model_tbl |>
    modeltime_calibrate(new_data = depois)
rmse_vec(depois$sales, previsoes$.calibration_data[[1]]$.prediction)
mae_vec(depois$sales, previsoes$.calibration_data[[1]]$.prediction)
