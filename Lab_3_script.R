# Lab 3 ------------------------------------------------------------------------

## Setting --------------------------------------------------------------------
rm(list = ls())
library(dplyr)
library(ggplot2)
library(forecast)
library(urca)
library(readxl)
dataset_lab3 <- read_excel("C:/Users/julia.papa/Downloads/dataset_lab3.xls")
View(dataset_lab3)
path <- "C:/Users/julia.papa/Downloads/dataset_lab3.xls"

## Import dataset --------------------------------------------------------------
data_orig <- readxl::read_xls(paste0("C:/Users/julia.papa/Downloads/dataset_lab3.xls"))

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
  select(date, pim_ibge, selic, cambio, ipca)

# Descriptive statistics ------------------------------------------------
apply(data[,2:ncol(data)], 2, function(x) summary(x))

## Plot series ----------------------------------------------------------
### Level
g1 <- ggplot(data) +
  geom_line(aes(x = date, y = pim_ibge), color = "black")
g2 <- ggplot(data) +
  geom_line(aes(x = date, y = selic), color = "black")
g3 <- ggplot(data) +
  geom_line(aes(x = date, y = cambio), color = "black")
g4 <- ggplot(data) +
  geom_line(aes(x = date, y = ipca), color = "black")
gridExtra::grid.arrange(g1,g2,g3,g4, ncol = 2)

### First difference 
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


### Clean dataset
data_orig_2 <- data

data <- data_orig_2 %>% 
  select(-c("dpim", "dselic", "dcambio", "dipca")) %>% 
  mutate(lpim = log(pim_ibge),
         lselic = log(selic),
         lcambio = log(cambio),
         lipca = log(ipca)) %>% 
  select(-c("pim_ibge", "selic", "cambio", "ipca")) %>% 
  select(date, lpim, lselic, lcambio, lipca, everything()) %>% 
  filter(!is.na(lpim))


### ACF

par(mfrow = c(1,2))
acf(data$lpim, lag = 50)
pacf(data$lpim, lag = 50)

par(mfrow = c(1,2))
acf(diff(data$lpim))
pacf(diff(data$lpim))



# Unity root tests --------------------------------------------------------

## ADF ----------------------------------------------------------------

### PIM
adf <- ur.df(data$lpim, type = "trend", selectlags = "AIC")
summary(adf)
tseries::adf.test()

adf <- ur.df(data$lpim, type ='drift', selectlags = 'AIC')
summary(adf)


## QUANDO VOCE TESTA COM TENDENCIA E ELA NAO É SIGNIFICATIVA, DIFICILMENTE
## ELA VAI SER SIGNIFICATIVA DEPOIS
# Primeiro testa a tendencia, depois a constante
## PP ------------------------------------------------------------------
pp <- urca::ur.pp(data$lpim,model = "constant",type =  "Z-tau")
summary(pp)

## esse daqui é com outro pacote
PP.test(data$lpim)

#ERS ---------------------------------------------------------------------
ers <- urca::ur.ers(,model = "")
summary(ers)


## KPSS -----------------------------------------------------------------
kpss <- urca::ur.kpss()
summary(kpss)


# Modeling --------------------------------------------------------------------

## ARIMA --------------------------------------------------------------------
xreg = data[,-c(1,2)] %>% as.matrix()  
y <- ts(data$lpim, start = data[["date"]][1], frequency =12)
model <- forecast::Arima(y, order = c(0,1,0), xreg = xreg)
summary(model)

par(mfrow = c(2,1))
plot(y)
plot(model$residuals)


par(mfrow = c(1,2))
acf(model$residuals)
pacf(model$residuals)


## ARIMA with additive seasonality ------------------------------------------
seas_dummy <- forecast::seasonaldummy(y) %>%
  as.data.frame()
names(seas_dummy) <- paste0("d_", names(seas_dummy))

data <- cbind(data, seas_dummy)

xreg = data[,-c(1,2)] %>% as.matrix()  
# xreg <- xreg[,grepl("d_",colnames(xreg))]
model <- forecast::Arima(y, order = c(0,1,0), xreg = xreg)
summary(model)

par(mfrow = c(2,1))
plot(y)
plot(model$residuals)


par(mfrow = c(1,2))
acf(model$residuals, lag = 50)
pacf(model$residuals, lag = 50)
## com lag = 50 fica mais controlado

# SARIMA ---------------------------------------------------------------------
xreg = data[,-c(1,2)] %>% as.matrix()  
xreg <- xreg[,!grepl("d_",colnames(xreg))]
colnames(xreg)
model <- forecast::Arima(y, 
                order = c(2,1,0),
                seasonal = c(1, 1, 0))
summary(model)

## para adicionar as covariadas, coloca um 'xreg'


par(mfrow = c(2,1))
plot(y)
plot(model$residuals)


par(mfrow = c(1,2))
acf(model$residuals)
pacf(model$residuals)

Box.test(model$residuals, lag = 13)


