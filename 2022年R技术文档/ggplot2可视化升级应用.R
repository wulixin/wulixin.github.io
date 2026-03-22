
library(ggrepel)
library(lubridate)
library(forcats) 
library(proto)
library(ggmap)
library(DT)
library(highcharter)
library(viridisLite)
library(treemap)
library(flexdashboard)
library(RJSONIO)
library(tvthemes)
library(dygraphs)
library(ggplot2) 
library(plotly)
library(highcharter)
library(ggvis)
library(ggmap)
#other packages
library(dplyr)
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
#library(d3heatmap)
library(quantmod)
library(shiny)
library(shinyWidgets)
library(bs4Dash)
library(plotly)
library(shiny)
#library(fontawesome)
library(DT)
library(shinydashboard)
library(leaflet)
library(dygraphs)
library(visNetwork)
library(treemap)
library(viridis)
library(RColorBrewer)
library(highcharter)
library(ggplot2)
library(echarts4r)
library(ECharts2Shiny)
library(data.table)
library(dplyr)
library(jsonlite)
library(Tushare)
library(xts)


fntltp <- JS("function(){
  return this.point.x + ' ' +  this.series.yAxis.categories[this.point.y] + ': ' +
  Highcharts.numberFormat(this.point.value, 2);
}")

plotline <- list(
  color = "#fde725", value = 1963, width = 2, zIndex = 5,
  label = list(
    text = "Vaccine Intoduced", verticalAlign = "top",
    style = list(color = "#606060"), textAlign = "left",
    rotation = 0, y = -5
  )
)

#### 用来发现几年内没有爆发的股票，锁定长期大牛股
#### 用来发现跌倒底部的次新黄金坑个股

today<-ymd(Sys.Date())

pro <-pro_api(token ='fe8102bf83f5f83f6608aa46fa5e985c534c227786236a1192e5fd55')

#stocks_names<-pro(api_name = 'stock_basic')
stocks_names<-pro(api_name = 'stock_basic',fields='ts_code,symbol,name,area,industry,list_date')

ts_name<-stocks_names%>%select(ts_code)
start_date<-Sys.Date()-years(1)
ts_codes<-ts_name$ts_code

dataprices = pro(api_name="daily",trade_date='20220112')


df_all<-dataprices%>%left_join(stocks_names)
df_all$list_date<-ymd(df_all$list_date)
latest_month<-Sys.Date()-days(30)

####特定板块的次新股

cixin<- df_all%>%
  #filter(list_date>ymd(latest_month) & market=="科创板")%>%
  filter(list_date>ymd(latest_month))%>%
  select(ts_code,name,trade_date,industry,close,pct_chg,vol,change)%>%
  arrange(desc(pct_chg))

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
    as.tibble()
}


#stocks_name<-stocks_names%>%filter(industry=="银行")
stocks_name<-stocks_names%>%filter(name %in% cixin$name)

china_stocks <- stocks_name%>%
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


####图形

library(showtext)
library(sysfonts)
library(showtextdb)
library(ggplot2)

showtext_auto(enable = TRUE)
font_add('Songti','Songti.ttc')
theme(text = element_text(family = 'Songti'))


treemap_data <- china_stocks%>%
  unnest(stock.prices)

treemap_data<-treemap_data%>%
  group_by(name)%>%
  mutate(DD=desc(Date))%>%
  ungroup()

treemap_data%>%group_by(name)%>%
               ggplot(aes(x=DD,y=Close,fill=name))+
               geom_line(show.legend = TRUE) +
               facet_wrap(~name+market,scales="free") +
               #coord_flip()+
               theme(axis.text.x = element_text(angle = 60, hjust = 1))
  
  
devtools::install_github("metrumresearchgroup/ggedit",subdir="ggedit")


library(ggplot2)
library(patchwork)
library(lemon)
p1 <- ggplot(mtcars) + geom_point(aes(mpg, disp))
p2 <- ggplot(mtcars) + geom_boxplot(aes(gear, disp, group = gear))

p1 + p2

p3 <- ggplot(mtcars) + geom_smooth(aes(disp, qsec))
p4 <- ggplot(mtcars) + geom_bar(aes(carb))

(p1 | p2 | p3) /
  p4

(p <- ggplot(mtcars, aes(x=as.factor(cyl), y=mpg)) + 
    geom_point(position=position_jitter(width=0.1)) + 
    coord_flex_cart(bottom=brackets_horisontal(), left=capped_vertical('both')) +
    theme_light() + theme(panel.border=element_blank(), axis.line = element_line())
)


library(grid)
dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
d <- ggplot(dsamp, aes(carat, price)) +
  geom_point(aes(colour = clarity))

###图形做个处理
reposition_legend(d, 'top left')

legend <- g_legend(d)


dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
p1 <- qplot(carat, price, data = dsamp, colour = clarity)
p2 <- qplot(cut, price, data = dsamp, colour = clarity)
p3 <- qplot(color, price, data = dsamp, colour = clarity)
p4 <- qplot(depth, price, data = dsamp, colour = clarity)
grid_arrange_shared_legend(p1, p2, p3, p4, ncol = 2, nrow = 2)








