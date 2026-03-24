#casova rada cpi
setwd(/Users/owner/Documents/AKM)
install.packages(readr)
library(readr)
cpi=read.csv("/Users/owner/Documents/AKM/cpi03.csv")
cpi_ts <- ts(cpi, frequency = 12, start = c(1996,1))
summary(cpi_ts)
plot(cpi_ts)
install.packages("ggplot2")
library(ggplot2)
install.packages("ggfortify")
library(ggfortify)
autoplot(cpi_ts, 
         colour = ('pink1'),
         size = 1, 
         ylab = "cpi", 
         xlab = "rok", 
         main = "Vývoj cpi v letech 1996-2020 (měsíčně)")

acf(cpi_ts)
acf(cpi_ts,plot=FALSE)
#pomalu klesa
pacf(cpi_ts)
#ostry rez po p 
install.packages("dplyr")
library(tibble)
library(dplyr)
mesic <- seq(as.Date("1996-01-01"), by = "month", length.out = length(cpi_ts))
#tibble
cpi_data <- tibble(
  date = mesic,
  cpi = as.numeric(cpi_ts)
)
#log diference, mezimesicni rust
cpi_data <- cpi_data %>%
  mutate(
    log_cpi = log(cpi),
    diff_log_cpi = (log(cpi) - log(lag(cpi))) * 100,
    cpi_growth= (cpi-lag(cpi))/lag(cpi)*100)
  )
library(ggplot2)
ggplot(data = cpi_data, aes(x = date)) +
  geom_line(aes(y = diff_log_cpi), color = "pink", linewidth = 1) +
  labs(title = "Log diference CPI", x = "Rok", y = "diference log(CPI)")
library(ggplot2)
ggplot(data = cpi_data, aes(x = date)) +
  geom_line(aes(y = cpi_growth), color = "pink", linewidth = 1) +
  labs(title = "měsiční růst CPI", x = "Rok", y = "růst CPI")
#ADF test
#H0: existence jednotkoveho korenu, casova rada neni stacionarni 
#H1: neexistuje jednotkovy koren, casova rada je stacionarni
install.packages("urca")
library(urca)
ur.df(cpi_ts, type = "none", selectlags = "AIC")
summary(ur.df(cpi_ts, type = "none", selectlags = "AIC"))
install.packages("tseries")
library(tseries)
adf.test(cpi_ts, alternative = "stationary")
diff_log <- na.omit(cpi_data$diff_log_cpi)
ur.df(diff_log, type = "none", selectlags = "AIC")
summary(ur.df(diff_log, type = "none", selectlags = "AIC"))
install.packages("forecast")
library(forecast)
#ARMA(1,1)
arma_1_1 <- Arima(diff_log, order = c(1, 0, 1))
summary(arma_1_1)
#ARMA(2,1)
arma_2_1 <- Arima(diff_log, order = c(2, 0, 1))
summary(arma_2_1)
#ARMA(2,2)
arma_2_2 <- Arima(diff_log, order = c(2, 0, 2))
summary(arma_2_2)
#srovnání AIC
data.frame(
  Model = c("ARMA(1,1)", "ARMA(2,1)", "ARMA(2,2)"),
  AIC = c(
    AIC(arma_1_1),
    AIC(arma_2_1),
    AIC(arma_2_2)
  )
) 

install.packages("readxl")      
library(readxl)
data2 <- read_excel("/Users/owner/Documents/AKM/DATA.xlsx")
#hdp casova rada
hdp_ts <- ts(data$HDP, start = c(1996, 1), frequency = 4)
library(ggfortify)
autoplot(hdp_ts,colour = "pink1", size = 1) +
  labs(
    title = "Vývoj HDP v období 1996–2015 (čtvrtletní)",
    x = "čtvrtletí",
    y = "HDP"
  )
#uprava casove rady
library(tibble)
data2 <- data2 %>%
  mutate(
    hdp_growth = (HDP - lag(HDP)) / lag(HDP) * 100
  )
hdp_growth <- ts(na.omit(data$hdp_growth), start = c(1996, 2), frequency = 4)
library(ggfortify)
autoplot(hdp_growth, colour = "pink1", size = 1) +
  labs(
    title = "růst HDP v období 1996–2015",
    x = "Čtvrtletí",
    y = "Růst HDP v procentech"
  )
#mezera produktu 
data2 <- data2 %>%
  mutate(t_index = 1:n())
model <- lm(log(HDP) ~ t_index, data = data2)  
data2 <- data2 %>%
  mutate(
    logpotencialniprodukt = predict(model),                
    potencialniprodukt = exp(logpotencialniprodukt),                   
    mezeraproduktu = (HDP - potencialniprodukt) / potencialniprodukt 
  )
mezeraproduktu_ts <- ts(data2$mezeraproduktu, start = c(1996, 1), frequency = 4)
library(ggfortify)
autoplot(mezeraproduktu_ts, colour = "pink1", size = 1) +
  labs(
    title = "mezera produktu 1996–2015",
    x = "Čtvrtletí",
    y = "mezera produktu"
  )
#mezera nezamestnanosti
data2 <- data2 %>%
  mutate(mezera_nezamestnanosti = u - lag(u))
mezera_u_ts <- ts(na.omit(data2$mezera_nezamestnanosti), start = c(1996, 2), frequency = 4)
library(ggfortify)

autoplot(mezera_u_ts, colour = "pink1", size = 1) +
  labs(
    title = "Mezera nezaměstnanosti",
    x = "Čtvrtletí",
    y = "Mezera nezaměstnanosti"
  )
#okuns law odhad
okun <- lm(mezeraproduktu ~ mezera_nezamestnanosti, data = data2)
summary(okun)
#box-jenkins
acf(hdp_growth)
acf(hdp_growth,plot=FALSE)
pacf(hdp_growth)
#acf pro rust skareda zkusim log diferenci 
data2 <- data2 %>%
  mutate(
    log_hdp = log(HDP),
    logdiff_hdp = (log(HDP) - log(lag(HDP)))  
  )
acf(na.omit(data2$logdiff_hdp), main = "ACF: log-dif (bez NA)")
pacf(na.omit(data2$logdiff_hdp), main = "PACF: log-dif (bez NA)")
logdiff_hdp_ts <- ts(na.omit(data2$logdiff_hdp), start = c(1996, 2), frequency = 4)
library(forecast)
logdiff_deseasonalized <- seasadj(stl(logdiff_hdp_ts, s.window = "periodic"))
acf(logdiff_deseasonalized)
pacf(logdiff_deseasonalized)
library(forecast)
fit_1_1 <- Arima(mezeraproduktu_ts, order = c(1, 0, 1))
fit_2_0 <- Arima(mezeraproduktu_ts, order = c(2, 0, 0))
fit_2_1 <- Arima(mezeraproduktu_ts, order = c(2, 0, 1))
fit_3_0 <- Arima(mezeraproduktu_ts, order = c(3, 0, 0))
fit_2_3 <- Arima(mezeraproduktu_ts, order = c(2, 0, 3))
AIC(fit_1_1, fit_2_0, fit_2_1, fit_3_0, fit_2_3)
#fit_2_3 nejnizsi hodnota
#rezidua jsou autokorelovana
checkresiduals(fit_2_3)
checkresiduals(fit_2_1)
checkresiduals(fit_1_1)
mezeraproduktu_diff <- diff(logdiff_deseasonalized)
library(tseries)
adf.test(na.omit(mezeraproduktu_diff))
library(forecast)
fit_d1 <- Arima(mezeraproduktu_diff, order = c(1, 0, 1))
fit_d2 <- Arima(mezeraproduktu_diff, order = c(2, 0, 1))
fit_d3 <- Arima(mezeraproduktu_diff, order = c(3, 0, 0))
AIC(fit_d1 ,fit_d2,fit_d3)
checkresiduals(fit_d1)
checkresiduals(fit_d2)
checkresiduals(fit_d3)
adf.test(na.omit(logdiff_deseasonalized))
         
         