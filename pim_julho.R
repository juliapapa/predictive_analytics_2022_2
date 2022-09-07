### CASE STUDY 1 - PREDICTIVE ANALYTICS ###

library(astsa)
library(urca)
library(bayesforecast)
library(tibble)
library(MLmetrics)

## base de dados 
library(readxl)
pim <- read_excel("pim.xlsx")
View(pim)

## coluna com o log do pim
lpim <- log(pim$PIM)
pim$lpim <- lpim

## plots 
plot(pim$lpim, type = 'l')
plot(pim$PIM, type ='l')


##correlograma

#SEM LOG
acf(pim$PIM, lag = 20)
pacf(pim$PIM, lag = 20)

#COM LOG
acf(pim$lpim, lag = 20)
pacf(pim$lpim, lag = 20)

## testes

adf = urca::ur.df(pim$lpim, type = 'trend', selectlags = 'AIC')
summary(adf)

pp <- urca::ur.pp(pim$lpim, model ='trend')
summary(pp)

kpss <- urca::ur.kpss(pim$lpim, type = c('mu'))
summary(kpss)

## modelos

###STL

timeseries = ts(pim$lpim, frequency = 12)
timeseries
fit <- stl(timeseries, t.window=12, s.window="periodic")
plot(fit)

fit %>% forecast(method="arima", h = 1) %>%
  autoplot()

exp(4.532232)


# ARIMA

model = arima(lpim, order = c(0,1,0))
model

## SARIMA

auto.sarima(pim$lpim, seasonal = TRUE, chains = 2, iter = 500)
