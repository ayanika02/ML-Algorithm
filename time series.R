#install.packages("quantmod")
library(quantmod)
#install.packages("tseries")
#install.packages("timeSeries")
#install.packages("forecast")
library(forecast)
#install.packages("xts")
library(xts)
library(ggplot2)
library(dplyr)
library(lubridate)
#install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

data("AirPassengers")
data <- AirPassengers
head(data)
data
windows(width=10,height=8)
plot(data)

#log transform
data <- log(data)
plot(data)
lag.plot(data, lags=6, do.lines=F)
acf(data)
pacf(data)  
 
#Differencing
diff(5:25,3)
d<-diff(data)
plot(d)

# Decomposition 
decomp <- decompose(data)
plot(decomp)

# ARIMA - Autoregressive Integrated Moving Average
model <- auto.arima(data)
 acf(model$residuals)
pacf(model$residuals)

# Box-Ljung test
Box.test(model$residuals, lag=20, type="Ljung-Box")

# Residual plot
hist(model$residuals,
     col = "lightblue",
     xlab = "Error",
     main = "Histogram of Residuals",
     freq = FALSE)
lines(density(model$residuals))
