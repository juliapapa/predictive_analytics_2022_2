## LAB 6

library(readxl)
forecasts <- read_excel("C:/Users/julia.papa/Downloads/forecasts.xlsx")
View(forecasts)
library(Metrics)
library(dplyr)
library(forecast)
library(MCS)
library(ForecastComb)
#for(i in 3:ncol(forecasts)){
#  if(i == 3){
#    t <- accuracy(forecasts[[i]], forecasts$real)
#    row.names(t) <- names(forecast([,i]))
#  } else {
#    tmp <- accuracy(forecasts[[i]], forecasts$real)
#    row.names(tmp) <- names(forecasts[,i])
#    t <- rbind(t, tmp)
#  }
#}

x1 = rmse(forecasts$forcast1, forecasts$real)
x2 = rmse(forecasts$forcast2, forecasts$real)
x3 = rmse(forecasts$forcast3, forecasts$real)
x4 = rmse(forecasts$forcast4, forecasts$real)
x5 = rmse(forecasts$forcast5, forecasts$real)
x6 = rmse(forecasts$forcast6, forecasts$real)
x7 = rmse(forecasts$forcast7, forecasts$real)
x8 = rmse(forecasts$forcast8, forecasts$real)
x9 = rmse(forecasts$forcast9, forecasts$real)
x10 = rmse(forecasts$forcast10, forecasts$real)
x11 = rmse(forecasts$forcast11, forecasts$real)
x12 = rmse(forecasts$forcast12, forecasts$real)
x13 = rmse(forecasts$forcast13, forecasts$real)
x14 = rmse(forecasts$forcast14, forecasts$real)
x15 = rmse(forecasts$forcast15, forecasts$real)
x16 = rmse(forecasts$forcast16, forecasts$real)

lista = c(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16)
max(lista) # x12
min(lista) # x1
order(lista)
#  1  5  2  9  3 10  4  6  8 16 13 15  7 14 11 12

## APLICAÇÃO DO TESTE DE DIABOLD E MARIANO
# H0: f1 = f2 = E(l1 -l2) = 0 # as previsões são iguais
# test stats = teste t 

## os melhores sao 1 e 5 --> comparar os 2

e1 = forecasts$real - forecasts$forcast1
e5 = forecasts$real - forecasts$forcast5
e12 = forecasts$real - forecasts$forcast12

## erros 1 e 5
dm.test(e1 = e1,
        e2 = e5,
        h = 3,
        power = 2,
        alternative = 'two.sided', # bicaudal 
        varestimator = 'acf')

## erros 1 e 12
dm.test(e1 = e1,
        e2 = e12,
        h = 3,
        power = 2,
        alternative = 'two.sided',
        varestimator = 'acf')
# rejeita a hipotese nula 
# erro pior
 
## MCS Model Confidence Set
# H0 = Todas as previsões são iguais; todas as previsoes sao
# conjuntamente iguais a um determinado modelo
# Estatisticas de Teste = 
# elimination rule = eliminar quem tem a pior estatistica de teste (o maior)

cbind(LossLevel(forecasts$real, forecasts$forcast1), e1^2)

# TMAX
Loss <- (forecasts$real - forecasts[, 3:ncol(forecasts)])^2
Loss <- as.matrix(Loss)

MCSprocedure(Loss, alpha = 0.01, B = 5000, statistic = "Tmax",
             cl = NULL, ram.allocation = TRUE, k = NULL, min.k = 3, 
             verbose = TRUE)
# aqui ele nao eliminou nenhum teste


Loss <- abs(forecasts$real - forecasts[,3:ncol(forecasts)])
Loss <- as.matrix(Loss)

mcs <- MCSprocedure(Loss, alpha = 0.01, B = 5000, statistic = "Tmax",
             cl = NULL, ram.allocation = TRUE, k = NULL, min.k = 3, 
             verbose = TRUE)

# Usado outra estatistica de teste (TR)

MCSprocedure(Loss, alpha = 0.01, B = 5000, statistic = "TR",
             cl = NULL, ram.allocation = TRUE, k = NULL, min.k = 3, 
             verbose = TRUE)

Loss <- abs(forecasts$real - forecasts[,3:ncol(forecasts)])
Loss <- as.matrix(Loss)

mcs2 <- MCSprocedure(Loss, alpha = 0.01, B = 5000, statistic = "TR",
                     cl = NULL, ram.allocation = TRUE, k = NULL, min.k = 3, 
                     verbose = TRUE)

## sobraram as previsoes 1,5 e 9

## COMBINATION
data <- foreccomb(forecasts$real,
                  forecasts[,c('forcast1','forcast5','forcast9')])
comb_SA(data)
# os 3 tem o mesmo peso
