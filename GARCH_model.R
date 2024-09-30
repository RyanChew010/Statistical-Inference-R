#General Autoregressive Conditional Heteroscedasticity
library(rugarch, warn.conflicts = F, quietly = T)
library(forecast,warn.conflicts = F, quietly = T)
library(tseries, warn.conflicts = F, quietly = T)
library(tidyquant, warn.conflicts = F, quietly = T)
library(tidyverse, warn.conflicts = F, quietly = T)

getSymbols("NVDA", from="2019-07-21", to="2024-07-21")

returns<- RETURN(NVDA$NVDA.Adjusted)
returns<- na.omit(returns)

chart.RollingPerformance(R= returns,
                         width = 252,
                         FUN = "sd.annualized",
                         scale = 252, main = "Nvidias' yearly rolling volatility")

#Specifying GARCH model for NVDA, where Rt = miu + error
# and sigma^2 = error + a1(error^2t-1) + b1(sigma^2t-1)
spec<- ugarchspec(variance.model = list(model="sGARCH"),
                  mean.model = list(armaOrder=c(0,0)),
                  distribution.model = "norm")
fit<- ugarchfit(spec, data = returns)
fit
# Results 
#Rt = 0.003609 + Error(t), sigma^2 = 0.131621(error^2t-1) + 0.771128(sigma^2t-1)



















