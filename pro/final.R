library(Matrix)
library(forecast)
library(tseries)
library(caret)
library(ggplot2)
library(TSA)
library(TTR)
#reading and ploting data
pd <- read.delim("product_distribution_training_set.txt", header=FALSE , sep ="\t" , stringsAsFactor=FALSE)
class(pd)
#adding the column to get sale of each day
sumcol<-colSums(pd[,-1])
smatrix<-data.matrix(sumcol,rownames.force = NA)
#time series of data 
y <- ts(ts(data.frame(smatrix[c(2:118),1])) , frequency=365)
class(sumcol)
#ploting the time series
ts.plot(y, xlab = "day", ylab = "", main = " sale daily")
#making data stationary on mean
plot(diff(y),ylab='Sales')
#making and ploting data stationary on varience
plot(log10(y),ylab='Log (Sales)')
#making and ploting data stationary on mean and varience
plot(diff(log10(y)),ylab='Differenced Log (Sales)')
#ACF and PACF value
acf(ts(diff(log10(y))), main='ACF sales')
pacf(ts(diff(log10(y))),main='PACF Sales')
z<-log(y)
#arima model
arimafit <- auto.arima(z , approximation = FALSE)
summary(arimafit)
Box.test(residuals(arimafit),lag = 4 , type = "Ljung")
#predicting sales
forecast <- forecast(arimafit, n.ahead = 29, newxreg = NULL)
plot(forecast)
forecast
#ACF and PACF value of residual
acf(ts(arimafit$residuals),main='ACF Residual')
pacf(ts(arimafit$residuals),main='PACF Residual')







