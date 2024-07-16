#套件安裝
#install.packages("quantmod")
#install.packages("forecast")

#套件載入
library(quantmod)
library(forecast)

## 美國銀行(BAC) 飛利浦(PHG) 波音(BA) 西門子(SIE) 聯營投資管理集團(AMG)
## 期間：2000/01~2015/04 頻率：月
BAC = getSymbols("BAC", from = "2000-01-01", to = "2015-04-30", auto.assign = FALSE)
PHG = getSymbols("PHG", from = "2000-01-01", to = "2015-04-30", auto.assign = FALSE)
BA = getSymbols("BA", from = "2000-01-01", to = "2015-04-30", auto.assign = FALSE)
SIE = getSymbols("SIE.DE", from = "2000-01-01", to = "2015-04-30", auto.assign = FALSE)
AMG = getSymbols("AMG", from = "2000-01-01", to = "2015-04-30", auto.assign = FALSE)
BAC = to.monthly(BAC)
PHG = to.monthly(PHG)
BA = to.monthly(BA)
SIE = to.monthly(SIE)
AMG = to.monthly(AMG)

# 修改標的(target, ts)
target = "AMG"
timeseries = ts(AMG, start = c(2000, 1), frequency = 12)
StlStock = stl(Cl(timeseries), s.window = "periodic")

# ARIMA
# 文獻範例:聯營投資管理集團(AMG) 
autofit = auto.arima(StlStock$time.series[, "trend"])
Predictedvalues = forecast(autofit,h = 39)
autoplot(Predictedvalues,ylab = paste(target," Index"),xlab = "Year")
summary(Predictedvalues)

# Holt Winters
# 文獻範例:聯營投資管理集團(AMG) 
HWStockr = HoltWinters(StlStock$time.series[, "trend"])
Predictedvalues = forecast(HWStockr,h = 39)
autoplot(Predictedvalues,ylab = paste(target," Index"),xlab = "Year")
summary(Predictedvalues)

# ANN
# 文獻範例:聯營投資管理集團(AMG) 
trend = ts(StlStock$time.series[, "trend"], start = c(2000, 1), frequency = 12)
NETfitr = nnetar(trend)
Predictedvalues = forecast(NETfitr, h = 39)
autoplot(Predictedvalues,ylab = paste(target," Index"),xlab = "Year")
summary(Predictedvalues)

# TSLM
# 文獻範例:聯營投資管理集團(AMG) 
tslmModel = tslm(StlStock$time.series[, "trend"] ~ trend + season)
Predictedvalues = forecast(tslmModel, h = 39)
autoplot(Predictedvalues,ylab = paste(target," Index"),xlab = "Year")
summary(Predictedvalues)
