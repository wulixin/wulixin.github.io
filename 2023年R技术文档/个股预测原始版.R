

#############################个股走势预测投票模型
library(TSstudio)
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
library(forecast)
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
library(xts)

today<-ymd(Sys.Date())
pro <-pro_api(token ='fe8102bf83f5f83f6608aa46fa5e985c534c227786236a1192e5fd55')

###   获取数据  标准化数据

dat<-pro(api_name = 'daily', ts_code='002672.SZ',start_date= today-years(1))

dat<-dat%>%
  mutate(open=as.numeric(as.character(open)),
         high=as.numeric(as.character(high)),
         close=as.numeric(as.character(close)),
         low=as.numeric(as.character(low)),
         vol=as.numeric(as.character(vol)),
         pre_close=as.numeric(as.character(pre_close)))
colnames(dat)<-c("ts_code","date","open","high","low","close","pre_close","change","pct_change","volume","amount")

dat$pct_type<-cut(dat$pct_change,breaks=c(-20,-4,0,3,8,20),labels=c("大跌","小跌","小涨","中涨","大涨"))

dat$vol_type<-quants(dat$volume, splits = 5, "labels")

library(lares)

plot_df(dat%>%select(-ts_code,-date))
plot_cats(dat%>%select(-ts_code,-date))


#####  连续变量转换为分类变量

#quants(dat$pct_change, splits = 5, "summary")
#quants(dft$Age, splits = 5, "labels")


library(funModeling)
library(showtext)
library(ggtext)
library(showtext)
showtext_auto(enable = TRUE)
font_add('Songti', 'Songti.ttc')

plotar(data=dat, target="pct_type", plot_type="histdens")

#c("pearson", "kendall", "spearman").

corr(
  dat%>%select(-ts_code,-date),
  method = "pearson")

###########这个相关系数表用在这个位置意义不大

freqs_df(dat%>%select(-ts_code,-date), plot = TRUE)

#####   分类变量与涨跌幅度的关系

freqs_plot(dat%>%select(-ts_code,-date),pct_type)




curr <- dat %>% tail(1)


library(tvthemes)
p <- dat %>%
  plot_ly(x = ~date, type="candlestick",
          open = ~open, close = ~close,
          high = ~high, low = ~low) %>%
  layout(title = "股票走势",
         xaxis = list(rangeslider = list(visible = F)))

p


##交易策略可视化

#从指标到信号到交易规则再到交易策略,四步走最后可视化,实现2*3策略!

today<-ymd(Sys.Date())

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
                yAxis = 2, name = "Buy level") 



## `r params$symbol` 交易卖出信号

#追涨杀跌,精准的逃顶指标!数学家高斯关于布兰运动的研究成果应用！

dygraph(data = S.TAOPAO,main="逃顶指标") %>% 
  dyHighlight(highlightCircleSize = 5, 
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = FALSE, highlightSeriesOpts = list(strokeWidth = 3)) %>%
  dyRangeSelector()



## `r params$symbol` 交易数据查询

df<-dat

df_index<-df%>%mutate(month=month(date))%>%
  mutate(pct_change=as.numeric(pct_change))%>%
  select(close,pct_change,volume,month)%>%
  group_by(month)%>%
  summarize(meanClose=mean(close),mean_vol=mean(volume))

medvol<-median(df_index$mean_vol)
maxvol<-max(df_index$mean_vol)
medclose<-median(df_index$meanClose)
maxclose<-max(df_index$meanClose)
datatbl<-df%>%select(date,high,low,close,pct_change,volume)
datatable(datatbl,filter = 'top', options = list(
  pageLength = 15, autoWidth = TRUE)) %>% 
  formatStyle('close',color = styleInterval(c(medclose,maxclose), c('green', 'blue', 'red')),
              fontWeight = styleInterval(12.5, c('normal', 'bold'))) %>%
  formatStyle('pct_change',color = styleInterval(c(0, 5), c('green', 'blue', 'red')),
              fontWeight = styleInterval(5, c('normal', 'bold'))) %>%
  formatStyle( 
    'volume',color = styleInterval(c(medvol, maxvol), c('green', 'blue', 'red')),
    backgroundColor = styleInterval(10000, c('gray', 'yellow'))) 



##  `r params$symbol` 交易量监控


p2 <- plot_ly(dat, x = ~date, y = ~ volume, type = 'bar') %>%
  layout(yaxis = list(title = '交易量'))

p2



## `r params$symbol`超级机器学习

library(nnfor)
library(fma)
a<-dat%>%arrange(date)#filter(trade_date>="2018-10-30")
y<-ts(a$close,frequency=252)


###Model-1 ELM
fit<-elm(y, m = frequency(y), hd = 30, type = "ridge", reps = 20, comb = "mean", lags =10,
         keep = NULL, difforder = NULL, outplot = FALSE,
         sel.lag = TRUE, direct = TRUE,allow.det.season = TRUE, det.type = "auto", 
         xreg = NULL, xreg.lags = NULL, xreg.keep = NULL, barebone = TRUE, model = NULL, retrain =  TRUE)
plot(fit)


library(fable)
library(fabletools)
autoplot(forecast(fit,h=9))

###Model-1 ELM_SPLINEF


fit<-splinef(y, h=10)
autoplot(fit)%>%ggtitle(subtitle = "ELM超级机器学习算法")

## 多维度模型

library(forecastHybrid)
quickModel <- hybridModel(y)
plot(quickModel, type = "fit")


## `r params$symbol`  ARIMA  MODELS

airforecast <- forecast(auto.arima(a$close), level = 95)
hchart(airforecast)%>%hc_add_theme(hc_theme_ffx())

########上面这两个模型预测出现错误

## `r params$symbol`  ETS  MODELS


x1 <- forecast(ets(a$close), h = 48, level = 95)
hchart(x1)%>%hc_add_theme(hc_theme_ffx())



## `r params$symbol`模拟人脑细胞预测
# install.packages("remotes")
#install_github("robjhyndman/forecast")
library(forecast)
library(fma)
library(fable)
fit2 <- nnetar(y, lambda=0.01)
autoplot(forecast(fit2,h=20))+
  ggtitle("NNFOR神经网络算法预测未来走势") +
  xlab("date") +
  ylab("price")

##  Meta AI人工智能预测


library(prophet)
library(lubridate)
history <- data.frame(ds =ymd(dat$date),
                      y = dat$close)
m <- prophet(history)
future <- make_future_dataframe(m, periods = 15)
forecast <- predict(m, future)
plot(m, forecast)+
  ggtitle("MetaAI人工智能算法预测走势") +
  xlab("date") +
  ylab("price")

## 多维度评估 
mapafit <- mapaest(dat$close,outplot=0)
mapacalc(m,mapafit,outplot=2)
#
plotmapa(mapafit)
fit <- mlp(y)
print(fit)
plot(fit)
frc <- forecast(fit,h=36)
plot(frc)

##  `r params$symbol` 量化指标体系

chartSeries(stock_prices_xts, type = "bars",theme = chartTheme('white',up.col='red',dn.col='blue'))
zoomChart('2018-4::')
addTA(EMA(Cl(stock_prices_xts)), on=1, col=6)
#addTA(OpCl(AAPL), col=4, type='b', lwd=2)
##Add Moving Average Convergence Divergence indicator to chart.
addMACD(fast = 12, slow = 26, signal = 9, type = "EMA", histogram = TRUE)
##Apply options or futures expiration vertical bars to current chart.
addExpiry(type = "options", lty = "dotted")
##William's percent R indiator to the current chart.
addWPR(n = 14)
addRSI(n = 14, maType = "EMA", wilder = TRUE)
##Add Parabolic Stop and Reversal indicator overlay to chart
addSAR(accel = c(0.02, 0.2), col = "black")
##
addBBands(n = 20, sd = 2, maType = "SMA", draw = 'bands', on = -1)
## ADD Commodity Channel Index
addCCI(n = 20, maType="SMA", c=0.015)
##Add Rate Of Change indicator to chart.可以用来查看涨停的次数
addROC(n = 1, type = c("discrete", "continuous"), col = "red")
##Add Directional Movement Index
addADX(n = 14, maType="EMA", wilder=TRUE)
zoomChart()

