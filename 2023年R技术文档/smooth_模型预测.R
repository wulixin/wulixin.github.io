

library(smooth)

###########ARIMA models 
ssarima(AirPassengers, h=12, silent=FALSE)

ssarima(AirPassengers, orders=list(ar=c(0,1),i=c(1,0),ma=c(1,1)), lags=c(1,12), h=12)

auto.ssarima(AirPassengers, h=12)

auto.ssarima(AirPassengers, h=12, initial="backcasting")


auto.ssarima(AirPassengers, h=12, initial="optimal")

msarima(AirPassengers, orders=list(ar=c(0,0,1),i=c(1,0,0),ma=c(1,1,1)),lags=c(1,6,12),h=12, silent=FALSE)

testModel <- adam(AirPassengers, "MMM", lags=c(1,12), distribution="dnorm",
                  h=12, holdout=TRUE)
summary(testModel)


##########
par(mfcol=c(3,4))
plot(testModel,which=c(1:11))
par(mfcol=c(1,1))
plot(testModel,which=12)




stock_prices_xts<-as.xts(OHLCV(dat),order.by=ymd(dat$date))
###########ces  models 

ces(stock_prices_xts, h=12, holdout=TRUE, silent=FALSE)

auto.ces(BJsales, h=12, holdout=TRUE, interval="p", silent=FALSE)

auto.gum(AirPassengers, interval=TRUE, silent=FALSE)



head(BJsales)
str(BJsales)


