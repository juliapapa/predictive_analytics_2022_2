###### PREVISÃO DO PIM DE AGOSTO
###### Júlia Papa e Igor Negrão
###### Predictive Analytics - FGV RI - 2022.2
#Packages necessários:
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

#Lendo a base de dados:
pim_orig <- read_excel("PIM_Agosto (1).xlsx")

pim_orig$Data = as.Date(pim_orig$Data)

df = pim_orig


##df = subset(df, Data > as.Date("2013-11-01"))

g1 = ggplot(df) +
  geom_line(aes(x = Data, y = PIM), color = "black")
g2 = ggplot(df) +
  geom_line(aes(x = Data, y = IPCA), color = "black")
g3 = ggplot(df) +
  geom_line(aes(x = Data, y = `Exportações - industrias diversas`), color = "black")
g4 = ggplot(df) +
  geom_line(aes(x = Data, y = `IPP-EUA`), color = "black")
gridExtra::grid.arrange(g1,g2,g3,g4, ncol = 2)

###############################  KPSS  ########################################


kpss_PIM = urca::ur.kpss(df$PIM, type = c("tau"))
summary(kpss_PIM)

kpss_IPCA = urca::ur.kpss(df$IPCA, type = c("tau"))
summary(kpss_IPCA)

kpss_EXP = urca::ur.kpss(df$`Exportações - industrias diversas`, type = c("tau"))
summary(kpss_EXP)

kpss_IPP = urca::ur.kpss(df$`IPP-EUA`, type = c("tau"))
summary(kpss_IPP)



### VEMOS QUE NÃO É ESTACIONARIO

ddf = df

logpim = log(df$PIM)
logipca = log(df$IPCA)
logexp = log(df$`Exportações - industrias diversas`)
logipp = log(df$`IPP-EUA`)


ddf$dpim = c(NA,diff(logpim))
ddf$dipca = c(NA,diff(logipca))
ddf$dexp = c(NA,diff(logexp))
ddf$dipp = c(NA,diff(logipp))


ddf$PIM = NULL
ddf$IPCA = NULL
ddf$`Exportações - industrias diversas` = NULL
ddf$`IPP-EUA` = NULL

##df = subset(df, Data > as.Date("2005-01-01"))

g5 <- ggplot(ddf) +
  geom_line(aes(x = Data, y = dpim), color = "black")
g6 <- ggplot(ddf) +
  geom_line(aes(x = Data, y = dipca), color = "black")
g7 <- ggplot(ddf) +
  geom_line(aes(x = Data, y = dexp), color = "black")
g8 <- ggplot(ddf) +
  geom_line(aes(x = Data, y = dipp), color = "black")
gridExtra::grid.arrange(g5,g6,g7,g8, ncol = 2)


#######################  KPSS FD  ############################################


kpss_dpim = urca::ur.kpss(ddf$dpim, type = c("tau"))
summary(kpss_dpim)

kpss_dipca = urca::ur.kpss(ddf$dipca, type = c("tau"))
summary(kpss_dipca)

kpss_dexp = urca::ur.kpss(ddf$dexp, type = c("tau"))
summary(kpss_dexp)

kpss_dipp = urca::ur.kpss(ddf$dipp, type = c("tau"))
summary(kpss_dipp)


#######################  TESTE DE ERRO  #######################################


mes_4 = subset(ddf, Data < as.Date("2022-04-01"))
mes_3 = subset(ddf, Data < as.Date("2022-05-01"))
mes_2 = subset(ddf, Data < as.Date("2022-06-01"))
mes_1 = subset(ddf, Data < as.Date("2022-07-01"))


##############################  VAR  ##########################################


ddf$Data = NULL
ddf = na.omit(ddf)

VARselect(ddf, lag.max = 10, type = "both")

model = VAR(ddf, p = 2, type = "both")
model


### AO ESCOLHERMOS O MODELO DE MENOR ERRO SCHWARZ, SABEMOS QUE NOSSO VAR É DE ORDEM (2)


################# PREVENDO PARA O MES DE AGOSTO ###########################


model_pred = VAR(ddf, p = 2, type = "both")

var_pred = predict(model_pred, n.ahead = 1)
dtaxa_pred = lapply(var_pred$fcst, `[[`, 1)
dtaxa_pred

ddf[nrow(ddf) + 1,] = dtaxa_pred

exp(diffinv(ddf$dpim, xi = logpim[[1]]))[[248]]

plot(exp(diffinv(ddf$dpim, xi = logpim[[1]])), type = "l")



####################  IMPULSO RESPOSTA PARA SABER SE FAZ SENTIDO A PREVISÃO 


feir_dipca = irf(model, impulse = "dipca",
                 n.ahead = 30, runs = 100,
                 cumulative = FALSE)

plot(feir_dipca)

######### RELAÇÃO NEGATIVA, QUANTO MAIOR O IPCA MENOR O PIM

feir_dexp = irf(model, impulse = "dexp",
                n.ahead = 30, runs = 100,
                cumulative = FALSE)

plot(feir_dexp)

######### RELAÇÃO POSITIVA, QUANTO MAIOR A EXPORTAÇÃO MAIOR O PIM


feir_dipp = irf(model, impulse = "dipp",
                n.ahead = 30, runs = 100,
                cumulative = FALSE)

plot(feir_dipp)

######### RELAÇÃO POSITIVA, QUANTO MAIOR O IPP EXTERNO MAIOR O PIM

data = pim_orig

x = tail(data$Data, 10)
y_IPCA = tail(data$IPCA, 10)
y_EXP = tail(data$`Exporta??es - industrias diversas`, 10)
y_IPP = tail(data$`IPP-EUA`, 10)


### DIMINUIÇÃO DO PIM

### PEQUENA DIMINUIÇÃO DO PIM

### AUMENTO DO PIM

g9 <- ggplot() +
  geom_line(aes(x = x, y = y_IPCA), color = "black")
g10 <- ggplot() +
  geom_line(aes(x = x, y = y_EXP), color = "black")
g11 <- ggplot() +
  geom_line(aes(x = x, y = y_IPP), color = "black")
gridExtra::grid.arrange(g9,g10,g11, ncol = 2)


######  A PLAUSIVEL A HIPÓTESE DE QUE O PIM DIMINUANO PROXIMO MÊS


############# TESTANDO OS ERROS DO MODELO #############################


mes_4$Data = NULL
mes_3$Data = NULL
mes_2$Data = NULL
mes_1$Data = NULL

mes_4 = na.omit(mes_4)
mes_3 = na.omit(mes_3)
mes_2 = na.omit(mes_2)
mes_1 = na.omit(mes_1)


model_pred_4 = VAR(mes_4, p = 2, type = "both")
var_pred_4 = predict(model_pred_4, n.ahead = 1)
dtaxa_pred_4 = lapply(var_pred_4$fcst, `[[`, 1)
dtaxa_pred_4$dpim
tail(mes_3$dpim, 1)

RMSE_VAR_4 = RMSE(dtaxa_pred_4$dpim, tail(mes_3$dpim, 1))

a = (RMSE_VAR_4/mean(ddf$dpim))/100


model_pred_3 = VAR(mes_3, p = 2, type = "both")
var_pred_3 = predict(model_pred_3, n.ahead = 1)
dtaxa_pred_3 = lapply(var_pred_3$fcst, `[[`, 1)
dtaxa_pred_3$dpim
tail(mes_2$dpim, 1)

RMSE_VAR_3 = RMSE(dtaxa_pred_3$dpim, tail(mes_2$dpim, 1))

b = (RMSE_VAR_3/mean(ddf$dpim))/100


model_pred_2 = VAR(mes_2, p = 2, type = "both")
var_pred_2 = predict(model_pred_2, n.ahead = 1)
dtaxa_pred_2 = lapply(var_pred_2$fcst, `[[`, 1)
dtaxa_pred_2$dpim
tail(mes_1$dpim, 1)

RMSE_VAR_2 = RMSE(dtaxa_pred_2$dpim, tail(mes_1$dpim, 1))

c = (RMSE_VAR_2/mean(ddf$dpim))/100


model_pred_1 = VAR(mes_1, p = 2, type = "both")
var_pred_1 = predict(model_pred_1, n.ahead = 1)
dtaxa_pred_1 = lapply(var_pred_1$fcst, `[[`, 1)
dtaxa_pred_1$dpim
ddf$dpim[[246]]

RMSE_VAR_1 = RMSE(dtaxa_pred_1$dpim, ddf$dpim[[246]])

d = (RMSE_VAR_1/mean(ddf$dpim))/100


######################  MÉDIA DO SCATTER INDEX  ###############################


mean(c(a, b, c, d))


tabela_erros <- matrix(c(RMSE_VAR_4, a,
                         RMSE_VAR_3, b,
                         RMSE_VAR_2, c,
                         RMSE_VAR_1, d), ncol=2, byrow=TRUE)
colnames(tabela_erros) <- c('RMSE_VAR','SCATTER INDEX')
rownames(tabela_erros) <- c('VAR_4','VAR_3','VAR_2', 'VAR_1')
tabela_erros <- as.data.frame(tabela_erros)
x = apply(tabela_erros, FUN = mean, MARGIN = 2)
tabela_erros = rbind(tabela_erros, x)
rownames(tabela_erros)[rownames(tabela_erros) == "5"] = "MÉDIA"

gt(tabela_erros) %>%
  tab_options(column_labels.hidden = F) %>% 
  tab_header(
    title = "O nosso modelo é bom?",
    subtitle = ("Obtivemos 40% de erro conforme mostra o Scatter Index"))







