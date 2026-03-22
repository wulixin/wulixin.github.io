
#plot packages
library(tvthemes)
library(dygraphs)
library(ggplot2)
library(plotly)
library(highcharter)
library(ggvis)
library(ggmap)
#other packages
library(lubridate)
library(dplyr)
library(forcats)
library(MAPA)
library(magick)
library(nnfor)
library(data.table)
library(quantmod)
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

pro <-pro_api(token ='fe8102bf83f5f83f6608aa46fa5e985c534c227786236a1192e5fd55')

stocks_names<-pro(api_name = 'stock_basic')

latest_week<-Sys.Date()-days(15)
latest_day<-Sys.Date()-days(1)
latest_week<-Sys.Date()-days(15)
latest_month<-Sys.Date()-days(30)
start_date<-Sys.Date()-days(552)
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
    mutate(Log.Returns=dailyReturn(as.xts(as.numeric(Close),order.by=Date),subset=NULL,type='arithmetic',leading=TRUE))%>%
    select(Date,Log.Returns)%>%
    as_tibble() }


dataprices = pro(api_name="daily",trade_date='20220825')

df_all<-dataprices%>%
  left_join(stocks_names)%>%
  #filter(industry=="化工原料")%>%
  filter(pct_chg > 7 & pct_chg < 21)%>%
  left_join(stocks_names)

head(df_all)

df_all$list_date<-ymd(df_all$list_date)
latest_month<-Sys.Date()-days(350)

###插入一个表格
library(flexpivot)
library(flextable)
library(janitor)
library(purrr)
df_all %>%
  tabyl(industry)%>%
  adorn_totals("row") %>%
  adorn_pct_formatting()%>%
  arrange(desc(n))%>%
  filter(n>5)%>%
  knitr::kable()

##
stocks_name<-stocks_names%>%
  #filter(industry %in% c("塑料","供气供热","化工原料","石油开采"))
  filter(name %in% df_all$name & ymd('20220301')>ymd(list_date))

library(purrr)
china_stocks <- stocks_name %>%
  mutate(
    stock.prices = map(ts_code, 
                       function(.x) get_stock_prices(.x)
    ),
    log.returns  = map(stock.prices, 
                       function(.x) get_log_returns(.x)),
    mean.log.returns = map_dbl(log.returns, ~ mean(.$Log.Returns)),
    sd.log.returns   = map_dbl(log.returns, ~ sd(.$Log.Returns)),
    n.trade.days = map_dbl(stock.prices, nrow)
  )%>%as_tibble()


ts_codes<-stocks_name$ts_code

prices = Map(function(n)
{
  #print(n)
  tryCatch(get_data(n)[,6], error = function(e) NA)
}, ts_codes)
N = length(prices)
# identify symbols returning valid data
i = ! unlist(Map(function(i) is.na(prices[i]), seq(N)))
# combine returned prices list into a matrix, one column for each symbol with valid data
prices = Reduce(cbind, prices[i])
colnames(prices) = stocks_name$name[i]
##########clean up and transform data 
for(j in 1:ncol(prices)) prices[, j] = na.locf(prices[, j])       # fill in
prices = prices[, apply(prices, 2, function(x) ! any(is.na(x)))]

log_returns = apply(prices, 2, function(x) diff(log(x)))

X = cor(log_returns)
colnames(X)<-stocks_name$name[i]
corr <- round(cor(log_returns), 1)



library(corrplot)
library(gplots)
library(RColorBrewer)
library(heatmaply)
library(d3heatmap)

library(purrr)
library(dplyr)
library(tidyverse)


#heatmapdata<-china_stocks%>%
  unnest(stock.prices)%>%
  select(Time,name,close)%>%
  mutate(Close=as.numeric(close),name=as.character(name))%>%
  spread(key=name, value=Close)%>%
  select(-Time)

  
heatmapdata<-china_stocks%>%
  unnest(stock.prices)%>%
  select(Date,name,Close)%>%
  mutate(Close=as.numeric(Close))%>%
  spread(key=name, value=Close)%>%
  select(-Date)

d3heatmap(heatmapdata,scale = "column", 
          show_grid = TRUE, anim_duration = 500,
          distfun = dist, hclustfun = hclust,height = 750,width=1350,main="天网作战地图")%>% 
  hmLegend(show = TRUE, title = "物以类聚人以群分", location = "tl") 


##################################


##这幅图很牛，之后可以做其它用途
#china_stocks %>%
  unnest(stock.prices)%>%
  select(Date,Close,name)%>%
  group_by(name) %>%
  do(p = plot_ly(., x = ~desc(ymd(Date)), y = ~Close,name=~name)%>%add_lines) %>%
  subplot(nrows =5, shareX = TRUE)

data_stocks<-china_stocks %>%
  unnest(stock.prices)%>%
  select(Date,Close,name,market)

library(plotly)
library(trelliscopejs)
#############
#qplot(Date, Close,data=data_stocks) +
  theme_bw() +
  facet_trelliscope(~ market + name, nrow = 4, ncol = 7, 
                    width = 300,scales="free")

#################################plotly效果更好一些

china_stocks %>%
    unnest(stock.prices)%>%
    select(Date,Close,name)%>%
    group_by(name) %>%
    do(p = plot_ly(., x = ~desc(ymd(Date)), y = ~Close,name=~name)%>%
         add_lines) %>%
    subplot(nrows =8, shareX = TRUE)%>%
  layout(title = "天网作战系统之全局走势图")

library(rbokeh)
library(dygraphs)
library(plotrix)
library(plotly)
library(echarts4r)


##################################反欺诈算法
library(tidyr)
library(tidyverse)
library(timetk)
library(purrr)
library(dplyr)


china_stocks %>%
  unnest(stock.prices)%>%head(5)

anoma_data<-china_stocks %>%
  unnest(stock.prices)%>%
  filter(ymd(list_date)<ymd('20220701'))%>%
  select(Date,Volume,name)%>%
  group_by(name) %>%
  tk_anomaly_diagnostics(Date,Volume)%>%
  filter(Date>ymd(today()-3))

library(DT)
datatable(anoma_data)

head(anoma_data)
anoma_name<-anoma_data%>%filter(anomaly=="Yes")

anoma_data%>%filter(anomaly=="Yes")%>%select(name,anomaly,Date)%>%knitr::kable()

head(df_all)
anoma_data%>%
  filter(anomaly=="Yes")%>%
  select(name,anomaly,Date)%>%
  left_join(df_all)%>%
  select(name,anomaly,Date,close,pct_chg)%>%
  arrange(desc(pct_chg))%>%
  knitr::kable()

anoma_namedata<-anoma_data%>%
  filter(anomaly=="Yes")%>%
  select(name,anomaly,Date)%>%
  left_join(df_all)%>%
  select(name,anomaly,Date,close,pct_chg)%>%
  arrange(desc(pct_chg))

china_stocks %>%
  unnest(stock.prices)%>%
  select(Date,Close,name)%>%
  filter(name %in% anoma_namedata$name)%>%
  group_by(name) %>%
  do(p = plot_ly(., x = ~desc(ymd(Date)), y = ~Close,name=~name)%>%
       add_lines) %>%
  subplot(nrows =5, shareX = TRUE) 


library(DT)
colnames(anoma_namedata)<-c('公司','是否异常','时间','收盘价','涨跌幅')
datatable(anoma_namedata,caption = htmltools::tags$caption(
  style = 'caption-side: top; text-align: center;',
  '反欺诈表: ', htmltools::em('主力异常行为数据')),extensions = 'Buttons', 
  options = list(dom = 'Bfrtip',pageLength = 20,
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': 'lightpink', 'color': 'brown'});",
      "$(this.api().table().body()).css({'background-color': 'gray', 'color': 'red'});",
      "}"),searchHighlight = TRUE, filter = 'top'
  ))%>%
  formatStyle('涨跌幅',
              color = styleInterval(4, c('red', 'brown')),
              backgroundColor = styleInterval(2, c('yellow', 'lightblue')))%>%
  formatStyle(
    '是否异常',
    transform = 'rotateX(15deg) rotateY(1deg) rotateZ(1deg)',
    backgroundColor = styleEqual(
      unique(anoma_namedata$是否异常), c('lightgreen')
    ) )


#'background-color': '#000', 'color': '#fff'
plot_ly(anoma_namedata) %>% 
  add_table()%>%
  layout(title='反欺诈算法数据')

library(dplyr)
library(purrr)

library(timetk)

china_data<-china_stocks %>%
  unnest(stock.prices)
#####################OK的
library(showtext)
showtext_auto(enable=TRUE)

china_stocks %>%
  unnest(stock.prices)%>%
  select(Date,Volume,name)%>%
  filter(name %in% anoma_namedata$公司)%>%
  group_by(name) %>%
  plot_anomaly_diagnostics(Date, Volume,
                           .message = FALSE,
                           .facet_ncol = 4,
                           .ribbon_alpha = 0.25,
                           .interactive = FALSE)

colnames(anoma_data)<-c("名称","日期","观察次数","周期性","趋势性","提示信号","季节调整","一级水平","二级水平","异常","一级维度","二级维度")

####################################
datatable(anoma_data)

a_plot<-china_stocks %>%
  unnest(stock.prices)%>%
  select(Date,Volume,name)%>%
  filter(name %in% anoma_namedata$公司)%>%
  group_by(name) %>%
  plot_anomaly_diagnostics(Date, Volume,
                           .message = FALSE,
                           .facet_ncol = 3,
                           .ribbon_alpha = 0.25,
                           .interactive = FALSE)
ggplotly(a_plot)%>%layout(title = "天网作战地图之反欺诈算法")


#########################

