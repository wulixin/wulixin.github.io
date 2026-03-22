

#############################同花顺标准化版本
 {library(TSstudio)
#library(bsts)
library(forecastML)
library(DT)
library(dygraphs)
library(ggplot2)
library(plotly)
library(highcharter)
library(ggvis)
library(RColorBrewer)
#other packages
library(lubridate)
library(dplyr)
library(forcats)
library(MAPA)
library(magick)
library(nnfor)
library(data.table)

library(PerformanceAnalytics)
library(TTR)
library(nnfor)
#library(forecast)
library(xts)
library(zoo)
library(PortfolioAnalytics)
library(Quandl)
library(astsa)
library(flexdashboard)
library(dplyr)
#finance packages 
library(readr)
library(devtools)
library(foreach)
library(Tushare)
library(lubridate)
library(tidyquant)
#dat <- tq_get(params$symbol, get = "stock.prices", from = params$start, to = params$end)
library(quantmod)
library(TTR)
library(tidyquant)
library(Quandl)
library(fable)}

today<-ymd(Sys.Date())

pro_api<-function (token) {
  http_url <- "http://api.Tushare.pro"
  return(function(api_name, ...) {
    params <- list(token = token, api_name = api_name, params = list(...))
    r <- httr::POST(http_url, body = params, encode = "json")
    res <- httr::content(r, "parsed", "application/json")
    if (is.null(res$data)) {
      return(NULL)
    }
    columns <- res$data$fields
    items <- res$data$items
    df <- as.data.frame(data.table::rbindlist(items))
    colnames(df) <- columns
    return(df)
  }) }

pro <-pro_api(token ='fe8102bf83f5f83f6608aa46fa5e985c534c227786236a1192e5fd55')




###获取数据
dat<-pro(api_name = 'daily', ts_code='300102.SZ',start_date= today-years(1))

#####
dat<-dat%>%
  mutate(open=as.numeric(as.character(open)),
         high=as.numeric(as.character(high)),
         close=as.numeric(as.character(close)),
         low=as.numeric(as.character(low)),
         vol=as.numeric(as.character(vol)),
         pre_close=as.numeric(as.character(pre_close)))
colnames(dat)<-c("ts_code","date","open","high","low","close","pre_close","change","pct_change","volume","amount")


##################交易策略可视化

#从指标到信号到交易规则再到交易策略,四步走最后可视化,实现2*3策略!

{today<-ymd(Sys.Date())

stock_prices_xts<-as.xts(OHLCV(dat),order.by=ymd(dat$date))
# Rename
names(stock_prices_xts) <- c("Open", "High", "Low", "Close","Adjusted","Volume")
#SPY<- adjustOHLC(stock_prices)
S<-stock_prices_xts
##逃顶指标
ratio <- S$Close/((S$High + S$Low)/2)
avgratio <- SMA(ratio,n=2)
out<-(avgratio-1)*1000
dat$date<-ymd(dat$date)
S.TAOPAO<-xts(x=out,order.by =dat$date)

S.SMA.14 <- SMA(Cl(S), n = 14)
S.SMA.7 <- SMA(Cl(S), n = 7)
S.SMA.20 <- SMA(Cl(S), n = 20)
S.SMA.30 <- SMA(Cl(S), n = 30)
S.SMA.100 <- SMA(Cl(S), n = 100)
S.RSI.14 <- RSI(Cl(S))
S.RSI.SellLevel <- xts(rep(71, NROW(S)), index(S))
S.RSI.BuyLevel <- xts(rep(31, NROW(S)), index(S))
highchart(type = "stock") %>% 
  # create axis :)
  hc_yAxis_multiples(
    create_yaxis(3, height = c(2, 1, 1), turnopposite = TRUE)
  ) %>% 
  # series :D
  hc_add_series(S, yAxis = 0, name = "股票") %>% 
  hc_add_series(S.SMA.7, yAxis = 0, name = "Fast MA 7") %>%
  hc_add_series(S.SMA.14, yAxis = 0, name = "Fast MA 14") %>%
  hc_add_series(S.SMA.20, yAxis = 0, name = "Fast MA 20") %>%
  hc_add_series(S.SMA.30, yAxis = 0, name = "Fast MA 30") %>%
  hc_add_series(S.SMA.100, yAxis = 0, name = "Slow MA 100") %>% 
  hc_add_series(S$Volume, color = "gray", yAxis = 1, name = "Volume", type = "column") %>% 
  hc_add_series(S.RSI.14, yAxis = 2, name = "Osciallator", color = hex_to_rgba("green", 0.7)) %>%
  hc_add_series(S.RSI.SellLevel, color = hex_to_rgba("red", 0.7),
                yAxis = 2, name = "Sell level") %>% 
  hc_add_series(S.RSI.BuyLevel, color = hex_to_rgba("blue", 0.7),
                yAxis = 2, name = "Buy level") }



######################第二部分 多维度指标
{chartSeries(stock_prices_xts, type = "bars",title="多维度指标发出交易信号",theme = chartTheme('white',up.col='red',dn.col='green'))
addBBands()
###### 重要指标
addChVol()	#	Add Technical Indicator to Chart
###### 重要指标
#####这个有趣
addDEMA()	#	Add Moving Average to Chart
#addDPO()	#	Add Technical Indicator to Chart  重要指标
addCMF()	#	Add Technical Indicator to Chart
addCLV()	#	Add Technical Indicator to Chart
addCCI()	#	Add Commodity Channel Index
addEMA()	#	Add Moving Average to Chart
addEnvelope()	#	Add Technical Indicator to Chart
addEVWMA()	#	Add Moving Average to Chart
addKST()	#	Add Technical Indicator to Chart  重要指标
addExpiry()	#	Add Contract Expiration Bars to Chart

#####现金流
addMFI()	#		Add Technical Indicator to Chart、
##  反弹点位指标
addSAR()	#		Add Parabolic Stop and Reversal to Chart
##趋势指标    可以使用
addTDI()	#		Add Technical Indicator to Chart
#addTRIX()	#		Add Technical Indicator to Chart
## 趋势线指标  可以使用 
addZigZag()	#	Add Technical Indicator to Chart
addZLEMA()	#	Add Moving Average to Chart
}

######################################################################
#######################   重要性多维度模型

#"Forecast from auto.arima, ets, thetam, nnetar, stlm, and tbats model"
{ dat$date<-ymd(dat$date)
a<-dat%>%arrange(date)%>%filter(date>=ymd("2021-08-01"))
y<-ts(a$close,frequency=12)
library(forecastHybrid)
quickModel <- hybridModel(y)
par(mfrow = c(2, 3))     
plot(forecast(quickModel$ets))
plot(forecast(quickModel$auto.arima))
plot(forecast(quickModel$thetam))
plot(forecast(quickModel$nnetar))
plot(forecast(quickModel$stlm))
plot(forecast(quickModel$tbats))
par(mfrow = c(1,1)) } # reset to default



