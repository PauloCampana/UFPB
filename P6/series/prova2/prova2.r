library(ggplot2)
theme_set(theme_bw())
options(digits = 4)

library(forecast)
library(ggplot2)

chicken <- astsa::chicken
frequency(chicken)

autoplot(chicken) +
    labs(x = "Tempo", y = "Preço")

decomp <- decompose(chicken, type = "additive")
autoplot(decomp) +
    labs(x = "Tempo", title = NULL)

tseries::kpss.test(chicken)$p.value
tseries::adf.test(chicken)$p.value
tseries::pp.test(chicken)$p.value

seastests::qs(chicken)$Pval
seastests::fried(chicken)$Pval
seastests::kw(chicken)$Pval
seastests::seasdum(chicken)$Pval
seastests::welch(chicken)$Pval

arima <- auto.arima(chicken, seasonal = FALSE)
sarima <- auto.arima(chicken, seasonal = TRUE)
arima
sarima

lmtest::coeftest(arima)

lmtest::coeftest(sarima)

sarima <- Arima(
    chicken,
    order = c(2, 1, 0),
    seasonal = c(0, 0, 1),
    include.drift = FALSE,
)
sarima
lmtest::coeftest(sarima)

checkresiduals(arima, test = FALSE)
checkresiduals(sarima, test = FALSE)

Box.test(arima$residuals, type = "Box-Pierce")$p.value
Box.test(arima$residuals, type = "Ljung-Box")$p.value
Box.test(sarima$residuals, type = "Box-Pierce")$p.value
Box.test(sarima$residuals, type = "Ljung-Box")$p.value

shapiro.test(arima$residuals)$p.value
nortest::lillie.test(arima$residuals)$p.value
shapiro.test(sarima$residuals)$p.value
nortest::lillie.test(sarima$residuals)$p.value

n <- length(chicken)
chicken.trunc <- ts(chicken[1:(n - 12)], freq = 12, start = c(2001, 8))

arima.trunc <- auto.arima(chicken.trunc, seasonal = FALSE)
lmtest::coeftest(arima.trunc)
checkresiduals(arima.trunc, test = FALSE)

sarima.trunc <- Arima(
    chicken.trunc,
    order = c(2, 1, 0),
    seasonal = c(0, 0, 1),
    include.drift = FALSE,
)
lmtest::coeftest(sarima.trunc)
checkresiduals(sarima.trunc, test = FALSE)

arima.predict <- forecast(arima.trunc, h = 12)$mean
sarima.predict <- forecast(sarima.trunc, h = 12)$mean
arima.predict
sarima.predict

holt <- hw(chicken.trunc)
holt.predict <- forecast(holt, h = 12)$mean
holt.predict

snaive <- snaive(chicken.trunc, h = 12)
snaive.predict <- forecast(snaive, h = 12)$mean
snaive.predict

sma <- smooth::sma(chicken.trunc, order = 6, h = 12)
sma.predict <- forecast(sma, h = 12)$mean
sma.predict

verdadeiro <- ts(chicken[(n - 11):n], freq = 12, start = c(2015, 8))

yardstick::rmse_vec(verdadeiro, arima.predict)
yardstick::rmse_vec(verdadeiro, sarima.predict)
yardstick::rmse_vec(verdadeiro, holt.predict)
yardstick::rmse_vec(verdadeiro, snaive.predict)
yardstick::rmse_vec(verdadeiro, sma.predict)

yardstick::mape_vec(verdadeiro, arima.predict)
yardstick::mape_vec(verdadeiro, sarima.predict)
yardstick::mape_vec(verdadeiro, holt.predict)
yardstick::mape_vec(verdadeiro, snaive.predict)
yardstick::mape_vec(verdadeiro, sma.predict)
