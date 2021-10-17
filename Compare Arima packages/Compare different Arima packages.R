library(zoo)
library(stargazer)
library(xts)
library(gets)
library(aTSA)
library(forecast)
library(stats)
library(midasr)
library(tseries)


### Time series application: AR(IMA) for the Brazilian GDP
### Compare results with different packages.

#define y
y <- ts(data=teste_t$tri, frequency = 4, start=c(2003,2), end=c(2019,3))
d1 <- ts(data=teste_t$d1, frequency = 4, start=c(2003,2), end=c(2019,3))
d2 <- ts(data=teste_t$d2, frequency = 4, start=c(2003,2), end=c(2019,3))
d3 <- ts(data=teste_t$d3, frequency = 4, start=c(2003,2), end=c(2019,3))
# adf test: ho = has a unit root
adf.test(as.vector(y))
acf(y)
pacf(y)
nsdiffs(y) # numero de vezes que diferencia para sazonal


# Recursive AR 
storage_ar1 <- ts(frequency = 4, start=c(2014,2), end=c(2019,3))
for(t in 1:22){ 
  model_ar1 <- arima(y[1:44+t], order= c(1,0,0)) # 
  p_ar1 <- predict(model_ar1, newdata = y, n.ahead = 1, se.fit = TRUE)
  storage_ar1[t] <- p_ar1$pred
  print(storage_ar1)
}  

e_ar1 = (y[45:66] - storage_ar1)^(2)
RMSE_ar1 <- mean(e_ar1)
print(RMSE_ar1)

# Recursive AR with Gets package
storage_arx1 <- ts(frequency = 4, start=c(2014,2), end=c(2019,3))
for(t in 1:22){ 
  model_arx1 <- arx(y[1:44+t], mc = TRUE, ar=1:1) 
  p_arx1 <- predict(model_arx1, newdata = y, n.ahead = 1, se.fit = TRUE)
  storage_arx1[t] <- p_arx1
  # print(storage_arx1)
  print(t)
  print(model_arx1)
}  

e_arx1 = (y[45:66] - storage_arx1)^(2)
RMSE_arx1 <- mean(e_arx1)
print(RMSE_arx1)

# Recursive AUTO.ARIMA
storage_arima <- ts(frequency = 4, start=c(2014,2), end=c(2019,3))
for(t in 1:22){ 
  model_arima <- auto.arima(as.vector(y[1:44+t]),max.p=4, max.q=12,ic = "aic") #seasonal=TRUE
  p_arima <- predict(model_arima, newdata = y[1:45+t], n.ahead = 1, se.fit = TRUE)
  storage_arima[t] <- p_arima$pred
  print(storage_arima)
}  

e_arima = (y[45:66] - storage_arima)^(2)
RMSE_arima <- mean(e_arima)
print(RMSE_arima)



#Recursive ARIMA 
storage_ar1 <- ts(frequency = 4, start=c(2014,2), end=c(2019,3))
for(t in 1:22){ 
  model_ar1 <- arima(y[1:44+t], order= c(1,0,0), seasonal=list(order=c(1, 0, 0)), period=4)
  p_ar1 <- predict(model_ar1, newdata = y, n.ahead = 1, se.fit = TRUE)
  storage_ar1[t] <- p_ar1$pred
  print(storage_ar1)
}  

e_ar1 = (y[45:66] - storage_ar1)^(2)
RMSE_ar1 <- mean(e_ar1)
print(RMSE_ar1)


# Recursive AUTO.ARIMA
storage_arima <- ts(frequency = 4, start=c(2014,2), end=c(2019,3))
for(t in 1:22){ 
  model_arima <- auto.arima(as.vector(y[1:44+t]),max.p=12, max.q=12,ic = "aic", seasonal=TRUE)
  p_arima <- predict(model_arima, newdata = y, n.ahead = 1, se.fit = TRUE)
  storage_arima[t] <- p_arima$pred
  print(storage_arima)
}  

e_arima = (y[45:66] - storage_arima)^(2)
RMSE_arima <- mean(e_arima)
print(RMSE_arima)


