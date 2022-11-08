# Lab 4 ------------------------------------------------------------------------
# References:
  # https://www.jstatsoft.org/article/view/v086i03

## Setting --------------------------------------------------------------------
rm(list = ls())
library(dplyr)
library(ggplot2)
# install.packages('gets')
library(gets)

path <- ""

## Import dataset --------------------------------------------------------------
data_orig <- readxl::read_xls(paste0(path,"dataset_lab4.xls"))

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
  select(-c("month", "year", "Data", "selic...26")) %>% 
  select(date, everything()) %>% 
  rename(selic = "selic...4")

# Descriptive statistics ------------------------------------------------
apply(data[,2:ncol(data)], 2, function(x) summary(x))
str(data)

# INCLUDE Unit root to all variables;

## Plot series ----------------------------------------------------------
### Level
g1 <- ggplot(data) +
  geom_line(aes(x = date, y = pim_ibge), color = "black")
g1

### First difference 
data <- data %>% 
  mutate(dpim = c(NA,diff(log(data$pim_ibge))),
         dselic = c(NA,diff(log(data$selic))),
         dcambio = c(NA,diff(log(data$cambio))),
         dipca = c(NA,diff(log(data$ipca)))) %>% 
  select(-c("pim_ibge", "cambio", "selic", "ipca")) %>% 
  select(date, dpim, dselic, dcambio, dipca, everything()) %>% 
  na.omit()



# Dummy saturation
y <- data[["dpim"]]
x <- as.matrix(data[,3:ncol(data)])

iis <- isat(y, t.pval = 0.01, 
            iis = T, sis = F, tis = F)
iis

sis <- isat(y, t.pval = 0.01,
            iis = T, sis = T, tis = F)
sis

tis <- isat(yts, t.pval = 0.01,
            iis = T, sis = T, tis = T)
tis

xisat <- sis[["aux"]][["mX"]]
xisat <- xisat[,-1]


# gets - mean
newx <- cbind(x, xisat)

alpha = 0.05
m1 <- arx(y, ar = 1:12, mxreg= newx, mc=TRUE)
m1.sel <- getsm(m1, t.pval=alpha, print.searchinfo = FALSE, 
                keep = c(1,52,53,54,55), plot = T)
print(m1.sel)

newx_pred <- newx[,which(colnames(newx) %in% m1.sel[["aux"]][["mXnames"]])]

predict(m1.sel, n.ahead = 30, newmxreg = newx_pred[1:30,])

# More variables than rows
# Dummy saturation
y <- data[["dpim"]][1:40]
x <- as.matrix(data[,3:ncol(data)])[1:40,]

iis2 <- isat(y, t.pval = 0.01, uis = x,
            iis = T, sis = F, tis = F)
iis2

newx_pred <- x[,which(colnames(x) %in% iis2[["aux"]][["mXnames"]])]

predict(iis2, n.ahead = 30, newmxreg = newx_pred[1:30,])

# CUSUM
xnames <- iis2[["aux"]][["mXnames"]]
xnames <- xnames[2:length(xnames)]
xnames <- paste(xnames, collapse = "+")
form <- formula(paste0("dpim", "~" ,xnames))
ocus <- efp(form , type="OLS-CUSUM", data = data)
plot(ocus)
