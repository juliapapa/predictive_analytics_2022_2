library(tidyverse)
library(dplyr)
library(readxl)
library(urca)
library(astsa)
library(bayesforecast)
library(tibble)
library(MLmetrics)
library(forecast)
library(ggplot2)
library(lubridate)
library(gt)
library(base)
library(vars)
library(tsDyn)

###############################################################################

df = read_excel("~/FGV/ppp.xls")

df = df %>% mutate(Data = ym(Data))

data = df

df = subset(df, Data > as.Date("2000-01-01"))

g1 <- ggplot(df) +
  geom_line(aes(x = Data, y = `Taxa de câmbio - R$ / US$`), color = "black")
g2 <- ggplot(df) +
  geom_line(aes(x = Data, y = IPCA), color = "black")
g3 <- ggplot(df) +
  geom_line(aes(x = Data, y = `Taxa PPC`), color = "black")
g4 <- ggplot(df) +
  geom_line(aes(x = Data, y = `Exportações US$ (milhões)`), color = "black")
gridExtra::grid.arrange(g1,g2,g3,g4, ncol = 2)


### VEMOS QUE TODAS AS VARIÁVEIS SÃO NÃO ESTACIONÁRIAS


##############################  INTEGRAÇÃO  ###################################


data$dtaxa = c(NA,diff(log(data$`Taxa de câmbio - R$ / US$`)))
data$dipca = c(NA,diff(log(data$IPCA)))
data$dppc = c(NA,diff(log(data$`Taxa PPC`)))
data$dexp = c(NA,diff(log(data$`Exportações US$ (milhões)`)))

data$`Taxa de câmbio - R$ / US$` = NULL
data$IPCA = NULL
data$`Taxa PPC` = NULL
data$`Exportações US$ (milhões)` = NULL

data= subset(data, Data > as.Date("2000-01-01"))

g5 <- ggplot(data) +
  geom_line(aes(x = Data, y = dtaxa), color = "black")
g6 <- ggplot(data) +
  geom_line(aes(x = Data, y = dipca), color = "black")
g7 <- ggplot(data) +
  geom_line(aes(x = Data, y = dppc), color = "black")
g8 <- ggplot(data) +
  geom_line(aes(x = Data, y = dexp), color = "black")
gridExtra::grid.arrange(g5,g6,g7,g8, ncol = 2)


### APARENTEMENTE TODAS AS VARIÁVIES FICAM ESTACIONÁRIAS, OU SEJA, SÃO DE ORDEM I(1)


################################  KPSS  #######################################

kpss_dtaxa = urca::ur.kpss(data$dtaxa, type = c("tau"))
summary(kpss_dtaxa)

kpss_dipca = urca::ur.kpss(data$dipca, type = c("tau"))
summary(kpss_dipca)

kpss_dppc = urca::ur.kpss(data$dppc, type = c("tau"))
summary(kpss_dppc)

kpss_dexp = urca::ur.kpss(data$dexp, type = c("tau"))
summary(kpss_dexp)


### FAREMOS TESTES DE RAIZ UNITARIAS KPSS PARA NOS ASSEGURARMOS DE QUE REALMENTE SÃO ESTACIONÁRIAS. OS TESTES COMPROVAM NOSSA SUPOSIÇÃO


################################  VAR  ########################################

data$Data = NULL

VARselect(data, lag.max = 10, type = "both")

model = VAR(data, p = 1, type = "both")
model


### AO ESCOLHERMOS O MODELO DE MENOR ERRO SCHWARZ, SABEMOS QUE NOSSO VAR É DE ORDEM (1)


###########################  FUNÇÃO IMPULSO  ##################################


feir_dtaxa = irf(model, impulse = "dtaxa",
                 n.ahead = 30, runs = 100,
                 cumulative = FALSE)

plot(feir_dtaxa)


feir_dipca = irf(model, impulse = "dipca",
                 n.ahead = 30, runs = 100,
                 cumulative = FALSE)

plot(feir_dipca)


feir_dppc = irf(model, impulse = "dppc",
                n.ahead = 30, runs = 100,
                cumulative = FALSE)

plot(feir_dppc)


feir_dexp = irf(model, impulse = "dexp",
                n.ahead = 30, runs = 100,
                cumulative = FALSE)

plot(feir_dexp)


### AS FUNÇÕES IMPULSO-RESPOSTA MOSTRAM QUE AS VARIÁVEIS IMFLUENCIAM A TAXA DE CÂMBIO


###############################  COINTEGRAÇÃO  ################################


coint_trace = ca.jo(data, type = "trace", ecdet = "trend", K = 2)
summary(coint_trace)


coint_eigen = ca.jo(data, type = "eigen", ecdet = "trend", K = 2)
summary(coint_eigen)


### AO TESTARMOS AMBOS OS TESTES, DO TRAÇO E DO MÁXIMO AUTOVALOR, VEMOS QUE O MODELO POSSUI MAIS DO QUE 3 VETORES DE COINTEGRAÇÃO


###################################  VEC  #####################################


vec = cajorls(coint_trace, r = 3)
summary(vec)
vec["beta"]
