#Time series analysis Models
library(rugarch, warn.conflicts = F, quietly = T)
library(forecast,warn.conflicts = F, quietly = T)
library(tseries, warn.conflicts = F, quietly = T)
library(tidyquant, warn.conflicts = F, quietly = T)
library(tidyverse, warn.conflicts = F, quietly = T)

#Simple Exponential Smoothing 
getSymbols("DAXEX.SW", from="2013-06-18", to="2024-06-18")
getSymbols("NPN.JO", from="2013-06-18", to="2024-06-18")

#group daily data into months
Daxex <- ts(DAXEX.SW$DAXEX.SW.Adjusted, frequency = 21)
Naspers<- ts(NPN.JO$NPN.JO.Adjusted, frequency = 21)
#Train data for 10 years 
train.data<- window(Daxex, end=c(120,20))
#Test data for 1 year
test.data<- window(Daxex, start=c(120,21))

#Exponential Smoothing
brown<- ses(train.data, h=200) #h is number of periods for forecasting
brown.data<- as.data.frame(brown)
plot(brown, main="Brown Exponential Smoothing", 
     ylab="Price", xlab="month",
     ylim=c(0,160))
lines(test.data, lty=3)

#AR I MA model
returns<- RETURN(DAXEX.SW$DAXEX.SW.Adjusted)
returns<-na.omit(returns)
adf.test(returns)
#if p-value less than 0.05, then we do not reject n0 
auto.arima(returns) #determines optimal number of parameters. Here ARIMA(2,0,0)

ARIMAModel<- arima(returns["2013::2024"], order=c(2,0,0))
ARIMAModel

#Check standard errors
et<- residuals(ARIMAModel)
acf(et) #Check for ACF 
#Test for zero mean
plot.ts(et)
#Check for distribution 
chart.Histogram(et,
                methods = c("add.density", "add.normal"),
                colorset = c('blue', 'green', 'red'))

#Forecasting ARIMA
forecast_ARIMA<- forecast(ARIMAModel,200)
returns.df<- as.data.frame(returns)
plot(forecast_ARIMA, xlab='Time', ylab='Returns',
     main="ARIMA forecast vs actual")
lines(returns.df, col='black')
