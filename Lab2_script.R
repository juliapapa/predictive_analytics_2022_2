## CLASS 7 - LAB 2 - 23/08/2022

# Out-of-sample exercise ################################################## 

## Setting --------------------------------------------------------------------

rm(list = ls())
library(dplyr)
library(ggplot2)
library(forecast)
path <- dataset_lab2 <- read_excel("C:/Users/julia.papa/Downloads/dataset_lab2.xls")
  
## Import dataset --------------------------------------------------------------
data_orig <- readxl::read_xls(paste0("C:/Users/julia.papa/Downloads/dataset_lab2.xls"))

str(data_orig)
names(data_orig)

data <- data_orig %>% 
  mutate(date = strsplit(Data, "\\.")) %>% 
  mutate(year = lapply(date, function(x) x[1]) %>% unlist(),
         month = lapply(date, function(x) x[2]) %>% unlist() %>% as.numeric()) %>% 
  select(-date) %>% 
  mutate(month = case_when(month == "1" ~ "01",
                           month == "2" ~ "02",
                           month == "3" ~ "03",
                           month == "4" ~ "04",
                           month == "5" ~ "05",
                           month == "6" ~ "06",
                           month == "7" ~ "07",
                           month == "8" ~ "08",
                           month == "9" ~ "09",
                           month == "10" ~ "10",
                           month == "11" ~ "11",
                           month == "12" ~ "12"))  %>% 
  mutate(date = as.Date(paste0(year, "-",month, "-01"))) %>% 
  mutate(date = as.Date(date)) %>% 
  arrange(date) %>% 
  select(date, pim_ibge, selic, cambio, ipca) %>% 
  mutate(trend = 1:nrow(data_orig)) %>% 
  mutate(trend2 = trend^2,
         trend3 = trend^3) %>% 
  mutate(covid1 = ifelse(date == as.Date("2020-04-01"), 1, 0),
         covid2 = ifelse(date == as.Date("2020-05-01"), 1, 0),
         covid3 = ifelse(date == as.Date("2020-03-01"), 1, 0),
         subprime1 = ifelse(date == as.Date("2008-12-01"), 1, 0),
         subprime2 = ifelse(date == as.Date("2008-11-01"), 1, 0),
         d_dif = ifelse(date == as.Date("2018-05-01"), -1, 0),
         d_dif = ifelse(date == as.Date("2018-06-01"), 1, d_dif))

## Seasonality
# TS é a função de time series 
# para criar essa dummy sazonal voce basicamente precisa da frequencia dos dados
# nao colocar NA em series de tempo --> basicamente estraga tudo. se vc nao tem
# o dado x, voce nao pode atualizar os anteriores
# time series nao eh bom deletar, em cross section, como geralmente tem mt dados,
# nao costuma ter problema deletar
seas_dummy <- ts(data$pim_ibge, start = data[["date"]][1], frequency =12) %>% 
  forecast::seasonaldummy(.) %>% 
  as.data.frame()
names(seas_dummy) <- paste0("d_", names(seas_dummy))

data <- cbind(data, seas_dummy) 

# Descriptive statistics
apply(data[,2:ncol(data)], 2, function(x) summary(x))

## Plot series ----------------------------------------------------------
g1 <- ggplot(data) +
  geom_line(aes(x = date, y = pim_ibge), color = "black")
g2 <- ggplot(data) +
  geom_line(aes(x = date, y = selic), color = "black")
g3 <- ggplot(data) +
  geom_line(aes(x = date, y = cambio), color = "black")
g4 <- ggplot(data) +
  geom_line(aes(x = date, y = ipca), color = "black")
gridExtra::grid.arrange(g1,g2,g3,g4, ncol = 2)

## First difference -----------------------------------------------------
data <- data %>% 
  mutate(dpim = c(NA,diff(log(data$pim_ibge))),
         dselic = c(NA,diff(log(data$selic))),
         dcambio = c(NA,diff(log(data$cambio))),
         dipca = c(NA,diff(log(data$ipca))))

g1 <- ggplot(data) +
  geom_line(aes(x = date, y = dpim), color = "black")
g2 <- ggplot(data) +
  geom_line(aes(x = date, y = dselic), color = "black")
g3 <- ggplot(data) +
  geom_line(aes(x = date, y = dcambio), color = "black")
g4 <- ggplot(data) +
  geom_line(aes(x = date, y = dipca), color = "black")
gridExtra::grid.arrange(g1,g2,g3,g4, ncol = 2)


# ARMA trend ------------------------------------------------------------------ 

## Data treatment
data_orig_2 <- data

data <- data_orig_2 %>% 
  select(-c("dpim", "dselic", "dcambio", "dipca")) %>% 
  mutate(lpim = log(pim_ibge),
         lselic = log(selic),
         lcambio = log(cambio),
         lipca = log(ipca)) %>% 
  select(-c("pim_ibge", "selic", "cambio", "ipca")) %>% 
  select(date, lpim, lselic, lcambio, lipca, everything())
  
## Spliting sample
names(data)
## amostra de estimação
data_in <- data %>% ## data in é tudo que eu tenho de informação
  filter(date <= as.Date("2022-06-01"))
## amostra de previsão
data_out <- data %>% ## data out é tudo que não tenho de informação
  filter(date > as.Date("2022-06-01"))
## a amostra de teste fica entre a amostra de estimação e a de previsão

## Running over the windows
# n windows é quantas janelas de previsão eu quero
nwindow <- 12
hahead <- 1
# Hahead é quanto tempo vc quer pra frente -- nesse caso é de um 1 mes a frente
## JANELA DE ESTIMAÇÃO = JANELA DE PREVISÃO - 1

metrics <- matrix(NA, nrow = nwindow, ncol = 6)
colnames(metrics) <- c("window","ME","RMSE","MAE","MPE","MAPE")
metrics[,1] <- 1:nwindow 

for(t in 1:nwindow){

  # estimation sample
  data_in_t <- data_in[1:(nrow(data_in)-nwindow-hahead+1+(t-1)),]
  # test sample
  data_out_t <- data_in[(nrow(data_in)-nwindow-hahead+2+(t-1)):(nrow(data_in)-nwindow-hahead+1+hahead+(t-1)),]
  
  # model estimation
  ## criação de objeto de time series (y) 
   y <- ts(data_in_t$lpim, start = data[["date"]][1], frequency =12)
   # xreg são os regressores
  xreg = data_in_t[,-c(1,2)] %>% as.matrix()  
  model <- forecast::auto.arima(y, xreg = xreg, seasonal = FALSE)
  print(model)
  
  # Forecast
  # valor vem em log
  newxreg <- as.matrix(data_out_t[,-c(1,2)])
  forecast <- predict(model, newxreg = newxreg)[["pred"]]

  # metrics
  ## pra cada janela vem um modelo diferente
  metrics[t,2:ncol(metrics)] <- accuracy(exp(forecast), x = exp(data_out_t[["lpim"]]))

}

## antes de comparar valores, tem que agregar eles 
metrics_mean_armat <- colMeans(metrics[,-1])
metrics_med_armat <- apply(metrics[,-1], 2, median)
metrics_mean_armat
metrics_med_armat

## COMPARAR A MESMA METRICA PARA DIFERENTES MODELOS


# STL  ------------------------------------------------------------------ 

## Running over the windows

metrics <- matrix(NA, nrow = nwindow, ncol = 6)
colnames(metrics) <- c("window","ME","RMSE","MAE","MPE","MAPE")
metrics[,1] <- 1:nwindow 

for(t in 1:nwindow){
  
  # estimation sample
  data_in_t <- data_in[1:(nrow(data_in)-nwindow-hahead+1+(t-1)),]
  # test sample
  data_out_t <- data_in[(nrow(data_in)-nwindow-hahead+2+(t-1)):(nrow(data_in)-nwindow-hahead+1+hahead+(t-1)),]
  
  # model estimation (parte de modelagem)
  y <- ts(data_in_t$lpim, start = data[["date"]][1], frequency =12)
  xreg = data_in_t[,-c(1,2)] %>% as.matrix()  
  model <- stl(y, s.window = "periodic", t.window = 12)
  # t é uma janela movel para identificar tendencias
  # stl(y, s.window = "periodic" ,t.window = 12) %>% autoplot()
  ## formula para mostrar o modelo graficamente
  
  # Forecast
  forecast <- forecast(model, h = hahead)[["mean"]]
  
  # metrics
  metrics[t,2:ncol(metrics)] <- accuracy(exp(forecast), x = exp(data_out_t[["lpim"]]))
  
}

metrics_mean_stl <- colMeans(metrics[,-1])
metrics_med_stl <- apply(metrics[,-1], 2, median)
metrics_mean_stl
metrics_med_stl

# Simple average  ------------------------------------------------------------------ 
# modelo naive
## Running over the windows

metrics <- matrix(NA, nrow = nwindow, ncol = 6)
colnames(metrics) <- c("window","ME","RMSE","MAE","MPE","MAPE")
metrics[,1] <- 1:nwindow 

for(t in 1:nwindow){
  
  # estimation sample
  data_in_t <- data_in[1:(nrow(data_in)-nwindow-hahead+1+(t-1)),]
  # test sample
  data_out_t <- data_in[(nrow(data_in)-nwindow-hahead+2+(t-1)):(nrow(data_in)-nwindow-hahead+1+hahead+(t-1)),]
  
  # model estimation
  y <- ts(data_in_t$lpim, start = data[["date"]][1], frequency =12)
  xreg = data_in_t[,-c(1,2)] %>% as.matrix()  
  model <- forecast::Arima(y)
  # print(model)
  
  # Forecast
  forecast <- predict(model, n.ahead = hahead)[["pred"]]
  
  # metrics
  metrics[t,2:ncol(metrics)] <- accuracy(exp(forecast), x = exp(data_out_t[["lpim"]]))
  
}

metrics_mean_mean <- colMeans(metrics[,-1])
metrics_med_mean <- apply(metrics[,-1], 2, median)
metrics_mean_mean
metrics_med_mean


## Comparing all models 

overall_metrics <- cbind(metrics_mean_armat,
                         metrics_mean_mean,
                         metrics_mean_stl)
overall_metrics
## erro na tabela --> duas linhas iguais com metricas diferentes
## para resolver --> pedir para o autor resolver ou calcular na mao com a formula

# Final Forecast ------------------------------------------------
# arima com tendencia
# model estimation
y <- ts(data_in$lpim, start = data[["date"]][1], frequency = 12)
xreg = data_in[,-c(1,2)] %>% as.matrix()  
# xreg = data_in[,3] %>% as.matrix()  
model <- forecast::auto.arima(y, xreg = xreg, seasonal = FALSE)
print(model)

## previsao do data out

# Forecast
newxreg <- as.matrix(data_out[,-c(1,2)])
# newxreg <- as.matrix(data_out[,3])
forecast <- predict(model, newxreg = newxreg)[["pred"]]
forecast
# deu NA pq os dados ainda nao existem

pim_julho <- data.frame(date = as.Date("2022-07-01"),
                        pim_julho = exp(forecast[1]))
pim_julho

ggplot() +
  geom_line(data = data, aes(x = date, y = exp(lpim))) +
  geom_point(data = pim_julho, aes(x = date, y = pim_julho), 
            color = "red") +
  ylab("Brazilian industrial production") +
  xlab("Dates")
## o ponto vermelho é a previsão
