

library(blotter)
library(quantstrat)
library(Quandl)
library(quantmod)
library(PerformanceAnalytics)
library(foreach)
library(xts)
library(TTR)
library(data.table)
library(dplyr)
library(Tushare)
library(tidyverse)
library(lubridate)
library(forcats)
library(MAPA)
library(magick)
library(nnfor)


library(astsa)
library(flexdashboard)
#plot packages 
library(dygraphs)
library(ggplot2)
library(plotly)
library(highcharter)
library(ggvis)
library(ggmap)
#finance packages 
#library(blotter)
#library(quantstrat)
library(readr)
library(devtools)
library(lubridate)



library(billboarder)
library(wordcloud2)
library(jiebaR)
library(tmcn)
library(gplots)
library(RColorBrewer)
library(heatmaply)
library(d3heatmap)
library(quantmod)
library(shiny)
library(shinyWidgets)
library(bs4Dash)
library(plotly)
library(shiny)
library(shinyWidgets)
library(bs4Dash)
library(DT)
library(shinydashboard)
library(leaflet)
library(dygraphs)
library(plotly)
library(visNetwork)
library(treemap)
library(viridis)
library(RColorBrewer)
library(highcharter)
library(ggplot2)
library(echarts4r)
library(ECharts2Shiny)
#library(echarts4r.maps)
library(data.table)
library(dplyr)
library(jsonlite)
library(Tushare)
library(xts)
library(lubridate)
today<-ymd(Sys.Date())
pro <-pro_api(token ='fe8102bf83f5f83f6608aa46fa5e985c534c227786236a1192e5fd55')
#stock_code<-'600362.SZ'
#stock_prices<-pro(api_name = 'daily', ts_code=stock_code,start_date= today-years(1))
stocks_names<-pro(api_name = 'stock_basic')


df = pro.daily(ts_code='000001.SZ', start_date='20180701', end_date='20180718')

df = pro.daily(ts_code='000001.SZ,600000.SH', start_date='20180701', end_date='20180718')

head(stocks_names)

#write.csv(stocks_names,"D:\\teachers\\stocks_names_202112.csv")

#stocks_names<-fread("D:\\teachers\\stocks_names_2021.csv")

unique(stocks_names$industry)



start_date<-Sys.Date()-years(1)
latest_week<-Sys.Date()-days(15)
latest_day<-Sys.Date()-days(1)
latest_week<-Sys.Date()-days(15)
latest_month<-Sys.Date()-days(30)
start_date<-Sys.Date()-days(120)
get_data<-function(ts_code){
  start_date=start_date
  data<-pro(api_name = 'daily', ts_code=ts_code, start_date=start_date)
}
##get basket stocks 
get_stock_prices <- function(ticker, return_format = "tibble", ...) {
  # Get stock prices
  stock_prices <- get_data(ts_code= ticker, ...)
  colnames(stock_prices)<-c("ts_code","Date","open","high","low","close","pre_close","change","pct_change","volume","amount")
  Date_new<-stock_prices$Date
  stock_prices_xts<-as.xts(OHLCV(stock_prices),order.by=ymd(stock_prices$Date))
  # Rename
  names(stock_prices_xts) <- c("Open", "High", "Low", "Close","Adjusted","Volume")
  # Return in xts format if tibble is not specified
  if (return_format == "tibble") {
    stock_prices <- stock_prices_xts %>%
      as_tibble() %>%
      mutate(Date=ymd(Date_new))
  } else {
    stock_prices <- stock_prices_xts
  }
  stock_prices
}

##获取股票的反弹数据
get_log_returns<-function(data){
  data%>%
    mutate(Log.Returns=dailyReturn(as.xts(as.numeric(Close),order.by=Date), subset=NULL, type='arithmetic',leading=TRUE))%>%
    select(Date,Log.Returns)%>%
    as.tibble()
}

head(stocks_names)

unique(stocks_names$industry)
###############Download daily closing price data


stocks_names%>%filter(name=="岳阳兴长")

library(stringr)

#tonghuashun<-fread('D:\\tonghuashun.csv')
#head(tonghuashun)
#stocks_name<-tonghuashun%>%filter(price<10 & pct_change >0.09)

stocks_names$list_date<-ymd(stocks_names$list_date)


#"小金属","铜",
stocks_name<-stocks_names%>%
  filter(industry %in% c("超市连锁","供气供热","石油加工","中成药"))

#"农药化肥","通信设备","化学制药","元器件","纺织","服饰"
#"元器件","电子元件","半导体"
#"医药商业","医疗保健","化学制药","生物制药",
#"焦炭加工","煤炭开采"
unique(stocks_name$industry)
#codes<-stocks_names%>%filter(name %in% stocks_name$name)

#ts_codes<-codes$ts_code

ts_codes<-stocks_name$ts_code

prices = Map(function(n)
{
  print(n)
  tryCatch(get_data(n)[,6], error = function(e) NA)
}, ts_codes)

N = length(prices)
# identify symbols returning valid data
i = ! unlist(Map(function(i) is.na(prices[i]), seq(N)))
# combine returned prices list into a matrix, one column for each symbol with valid data
prices = Reduce(cbind, prices[i])

colnames(prices) = stocks_name$name[i]

#colnames(prices) = codes$name[i]
##########clean up and transform data 
for(j in 1:ncol(prices)) prices[, j] = na.locf(prices[, j])       # fill in

prices = prices[, apply(prices, 2, function(x) ! any(is.na(x)))]

log_returns = apply(prices, 2, function(x) diff(log(x)))

head(log_returns)


X = cor(log_returns)
colnames(X)<-stocks_name$name[i]

#colnames(X)<-codes$name[i]


#library(highcharter)
#hchart(princomp(X, cor = TRUE))%>%hc_add_theme(hc_theme_ffx())

#hchart(X)

#X%>%
#  corrplot(method="ellipse",type="upper",order= "hclust",hclust.method="ward.D2",rect.col = "black", rect.lwd = 2, tl.pos = NULL,
#           tl.cex = 1, tl.col = "red", tl.offset = 0.4, tl.srt = 90,
#           cl.pos = NULL, cl.lim = NULL, cl.length = NULL, cl.cex = 0.8,
#           cl.ratio = 0.15, cl.align.text = "c", cl.offset = 0.5, number.cex = 1)

###############################Regularization
L = eigen(X, symmetric=TRUE)
plot(L$values, ylab="eigenvalues")
abline(v=10)

N = 10  # (use 1st 10 eigenvectors, set N larger to reduce regularization)
P = L$vectors[, 1:N] %*% ((1 / L$values[1:N]) * t(L$vectors[, 1:N]))
P = P / tcrossprod(sqrt(diag(P)))

library(corpcor)
suppressMessages(library(igraph))

threshold = 0.90
Q = P * (P > quantile(P, probs=threshold))                           # thresholded precision matrix
g = graph.adjacency(Q, mode="undirected", weighted=TRUE, diag=FALSE) # ...expressed as a graph

# The rest of the code lumps any singletons lacking edges into a single 'unassociated' group shown in gray
# (also assigning distinct colors to the other groups).
x = groups(cluster_louvain(g))
i = unlist(lapply(x, length))
d = order(i, decreasing=TRUE)
x = x[d]
i = i[d]
j = i > 1
s = sum(j)
names(x)[j] = seq(1, s)
names(x)[! j] = s + 1 
grp = as.integer(rep(names(x), i))
clrs = c(rainbow(s), "gray")[grp[order(unlist(x))]]
g = set_vertex_attr(g, "color", value=clrs)

library(threejs)
graphjs(g, vertex.size=0.2, vertex.shape=colnames(X), edge.alpha=0.5)

