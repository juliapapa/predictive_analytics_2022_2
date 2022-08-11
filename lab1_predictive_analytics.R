## PREDICTIVE ANALYTICS

## CLASS 4 - ARMA MODEL - 11/08/2022
## TIME SERIES LAB


library(readxl)
ipeadata_class4 <- read_excel("FGV/ipeadata_class4.xls")
View(ipeadata_class4)

exc <- ipeadata_class4$cambio

exc <- exc[242:length(exc)]

summary(exc)

plot(exc, type = 'l') ## l de line

lexc <- diff(log(exc))
plot(lexc, type = 'l')

acf(exc) # autocorrelation function
pacf(exc) # parcial autocorrelation function

par(mfrow = c(1,2))
acf(exc, lag = 50)
pacf(exc)

par(mfrow = c(1,2))
acf(lexc, lag = 50)
pacf(lexc)

Box.test(lexc, lag = 1, type = c('Ljung-Box')) # rejeita H0
Box.test(lexc, lag = 2, type = c('Ljung-Box')) # rejeita H0
Box.test(lexc, lag = 3, type = c('Ljung-Box')) # rejeita H0
Box.test(lexc, lag = 4, type = c('Ljung-Box')) # rejeita H0

model <- arima(lexc, order = c(2,0,0)) ## A specification of the non-seasonal part of 
# the ARIMA model:the three integer components (p, d, q)(p,d,q) are the AR order, 
# the degree of differencing, and the MA order.
t <- predict (model, h.ahead =10)
## to see if it is correct or not, we have to analyze the residuals
par(mfrow = c(1,2))
acf(model$residual)
pacf(model$residual) ## it cant have any kind of autocorrelation

## esses ultimos valores deram errado
Box.test(model$residual, lag = 7, type = c('Ljung-Box')) # nao rejeita H0
## escolheu 7 pq é o ponto fora dos resíduos 
Box.test(model$residual, lag = 8, type = c('Ljung-Box')) # nao rejeita H0
## ljung box é um teste rigoroso
## os residuos estao bem comportados 
