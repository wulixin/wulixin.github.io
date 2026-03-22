
#######################################################################
#
#                 同花顺数据分析报告专用
#
#
########################################################################

{
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
## 分布全景视图
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
library(dplyr)
library(tidyr)
library(tidyverse)
library(purrr)
library(Tushare) }


library(Quandl)
Quandl.api_key("5tLBvJCNq1-k9UmG5x22")
library(quantmod)



pro_api<-function (token) 
{
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
  })
}

pro <-pro_api(token ='fe8102bf83f5f83f6608aa46fa5e985c534c227786236a1192e5fd55')

stocks_names<-pro(api_name = 'stock_basic')
#write.csv(stocks_names,'//Users//wulixin//Downloads//stocksnames20260127.csv')

url<-'api.Tushare.pro'

#api <- Tushare::pro_api(token = 'fe8102bf83f5f83f6608aa46fa5e985c534c227786236a1192e5fd55')
#api(api_name = 'stock_basic')


latest_week<-Sys.Date()-days(15)
latest_day<-Sys.Date()-days(1)
latest_week<-Sys.Date()-days(15)
latest_month<-Sys.Date()-days(30)
start_date<-Sys.Date()-days(150)

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

##=================当返回数值少于2个时会出现命名错误,因此需要优化
## 增强代码适用性
library(tidyverse)
library(xts)
library(lubridate)

get_stock_prices <- function(ticker, return_format = "tibble", ...) {
  # Step 1: 获取原始数据
  stock_prices <- get_data(ts_code = ticker, ...)
  
  # Step 2: 安全处理：确保是 data.frame 且至少有 0 行
  if (is.null(stock_prices) || !is.data.frame(stock_prices) || nrow(stock_prices) == 0) {
    # 返回空但结构正确的 tibble（或 xts）
    empty_tibble <- tibble(
      Date = as.Date(character(0)),
      Open = numeric(0),
      High = numeric(0),
      Low = numeric(0),
      Close = numeric(0),
      Adjusted = numeric(0),
      Volume = numeric(0)
    )
    if (return_format == "tibble") {
      return(empty_tibble)
    } else {
      return(as.xts(empty_tibble[, -1], order.by = empty_tibble$Date))
    }
  }
  
  # Step 3: 确保列数匹配（防止 get_data 返回列数不一致）
  expected_cols <- 11  # 根据你设定的 colnames
  if (ncol(stock_prices) != expected_cols) {
    warning(paste("Unexpected column count for ticker:", ticker, "; got", ncol(stock_prices)))
    # 可选择跳过或填充，这里保守处理：返回空
    empty_tibble <- tibble(
      Date = as.Date(character(0)),
      Open = numeric(0),
      High = numeric(0),
      Low = numeric(0),
      Close = numeric(0),
      Adjusted = numeric(0),
      Volume = numeric(0)
    )
    if (return_format == "tibble") return(empty_tibble) else return(as.xts(empty_tibble[, -1], order.by = empty_tibble$Date))
  }
  
  # Step 4: 设置列名（仅在 data.frame 且有列时）
  colnames(stock_prices) <- c("ts_code", "Date", "open", "high", "low", "close", 
                              "pre_close", "change", "pct_change", "volume", "amount")
  
  # Step 5: 转换为 xts
  # 先确保 Date 是 Date 类型
  stock_prices$Date <- ymd(stock_prices$Date)
  
  # 构造 OHLCV 数据（注意：你原代码中用 OHLCV()，但未定义；这里手动构造）
  ohlcv_data <- stock_prices %>%
    select(open, high, low, close, volume) %>%
    mutate(Adjusted = close) %>%  # 你原逻辑把 close 当作 Adjusted
    as.matrix()
  
  # 创建 xts 对象
  stock_prices_xts <- xts(ohlcv_data, order.by = stock_prices$Date)
  names(stock_prices_xts) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
  
  # Step 6: 返回格式
  if (return_format == "tibble") {
    result <- stock_prices_xts %>%
      as_tibble(rownames = "Date") %>%
      mutate(Date = ymd(Date)) %>%
      relocate(Date)
    return(result)
  } else {
    return(stock_prices_xts)
  }
}
dat_1<-get_stock_prices('000001.SZ')
#####################






##获取股票的反弹数据
get_log_returns<-function(data){
  data%>%
    mutate(Log.Returns=dailyReturn(as.xts(as.numeric(Close),order.by=Date),subset=NULL,type='arithmetic',leading=TRUE))%>%
    select(Date,Log.Returns)%>%
    as_tibble() }




## 获取数据  时间  
dataprices = pro(api_name="daily",trade_date='20260127')
library(dplyr)
#library(data.table)
#stocks_names<-fread("//Users//wulixin//Desktop//stocksnames20240112.csv",drop=1)


head(dataprices)

df_all<-dataprices%>%
  filter(pct_chg < 21)%>%
  left_join(stocks_names)

library(janitor)
library(lares)
library(lubridate)
df_all$list_date<-as.Date(ymd(df_all$list_date))
#时间变量的生成
df_all$list_date<-ymd(df_all$list_date)
df_all$year<-year(df_all$list_date)
df_all$mon<-as.factor(paste('上市月份',month(df_all$list_date),seq=''))



library(viridis)
library(viridisLite)



####### 上市时间分组  年+月分组  

df_all$Time<-as.numeric(today()-df_all$list_date)

df_all$Timegroup = as.factor(paste('上市时间区间',cut(df_all$Time, c(0, 15, 75, 300,700)),seq=''))



head(df_all$Timegroup)

treemap_data1 <- df_all%>%
  mutate(category = gsub(" ", "-",industry),
         subcategory = gsub(" ", "-",Timegroup),
         ratio=as.numeric(pct_chg),
         Volume=as.numeric(vol)) %>%
  select(category, subcategory,name,Volume,ratio)

##########THEME主题背景的设定
lvl_opts <-  list(
  list(
    level = 1,
    borderWidth = 2,
    borderColor = "brown",
    dataLabels = list(
      enabled = TRUE,
      align = "left",
      verticalAlign = "top",
      style = list(
        fontSize = "20px", 
        textOutline = FALSE,
        color = "red",
        fontWeight = "bold"
      )
    )
  ),
  list(
    level = 2,
    borderWidth = 2,
    borderColor = "white",
    colorVariation = list(key = "brightness", to = 0.250),
    dataLabels = list(enabled = TRUE,align = "right",
                      verticalAlign = "bottom",
                      style = list(fontSize = "15px",
                                   textOutline = FALSE, 
                                   color = "red", 
                                   fontWeight = "blod")
    )
  ),
  list(
    level = 3,
    borderWidth = 2,
    borderColor = "brown",
    colorVariation = list(key = "brightness", to = 0.50),
    dataLabels = list(enabled = TRUE,
                      style = list(fontSize = "12px",
                                   textOutline = FALSE, 
                                   color = "white", 
                                   fontWeight = "normal")
    )
  )
)

pkmn_min <- treemap_data1 %>% 
  select(category,subcategory,name) %>%
  mutate(category = stringr::str_to_title(category)) %>% 
  mutate(subcategory= ifelse(is.na(subcategory), category, paste(category, "上市时间区间", subcategory))) %>%
  mutate(val = 1)

cols <- pkmn_min %>% 
  count(category,subcategory, sort = TRUE) %>% 
  pull(subcategory) %>% 
  unique()

#######图形一：全局操盘视图  行业————时间维度
#

library(dplyr)

hchart(
  data_to_hierarchical(treemap_data1,c("category","subcategory","name"), size=Volume, 
                       colors =rev(viridis(8))),type = "treemap",allowDrillToNode = TRUE,
  tooltip = list(valueDecimals = FALSE),levels = lvl_opts) %>% 
  hc_chart(
    style = list(fontFamily = "Gloria Hallelujah")) %>% 
  hc_title(text = "全局作战操盘地图可视化",
           style = list(fontFamily = "Gloria Hallelujah",fontSize = "30px", color = "red", fontWeight = "bold")) %>% 
  hc_size(height = 750,width = 1350) 



#######图形三  市值大盘中小盘维度

df_all$ShiZhi<-cut(df_all$amount,breaks=quantile(df_all$amount),
                   labels=c("迷你盘","小盘股","中盘股","大盘股"))

treemap_data2 <- df_all%>%
  mutate(category = gsub(" ", "-",industry),
         subcategory = gsub(" ", "-",ShiZhi),
         ratio=as.numeric(pct_chg),
         Volume=as.numeric(vol)) %>%
  select(category, subcategory,name,Volume,ratio)

pkmn_min <- treemap_data2 %>% 
  select(category,subcategory,name) %>%
  mutate(category = stringr::str_to_title(category)) %>% 
  mutate(subcategory= ifelse(is.na(subcategory), category, paste(category, "市值规模", subcategory))) %>%
  mutate(val = 1)

hchart(
  data_to_hierarchical(treemap_data2,c("category","subcategory","name"), size=ratio, 
                       colors =rev(viridis(8))),type = "treemap",allowDrillToNode = TRUE,
  tooltip = list(valueDecimals = FALSE),levels = lvl_opts) %>% 
  hc_chart(
    style = list(fontFamily = "Gloria Hallelujah")) %>% 
  hc_title(text = "全局作战操盘地图可视化",
           style = list(fontFamily = "Gloria Hallelujah",fontSize = "30px", color = "red", fontWeight = "bold")) %>% 
  hc_size(height = 750,width = 1350)


#===============================================================================
#
#            地理维度——区域经济产业链特色
#
#===============================================================================
unique(df_all$area)

treemap_data4 <- df_all%>%
  mutate(category = gsub(" ", "-",area),
         subcategory = gsub(" ", "-",industry),
         ratio=as.numeric(pct_chg),
         Volume=as.numeric(vol)) %>%
  select(category, subcategory,name,Volume,ratio)

pkmn_min <- treemap_data4 %>% 
  select(category,subcategory,name) %>%
  mutate(category = stringr::str_to_title(category)) %>% 
  mutate(subcategory= ifelse(is.na(subcategory), category, paste(category, "行业", subcategory))) %>%
  mutate(val = 1)


hchart(
  data_to_hierarchical(treemap_data4,c("category","subcategory","name"), 
                       size=Volume, colors =rev(viridis(12))),type = "treemap",
  allowDrillToNode = TRUE,tooltip = list(valueDecimals = FALSE),levels = lvl_opts) %>% 
  hc_chart(
    style = list(fontFamily = "Gloria Hallelujah")) %>% 
  hc_title(text = "天网作战地图之区域产业分布",
           style = list(fontFamily = "Gloria Hallelujah",fontSize = "30px", color = "red", fontWeight = "bold")) %>% 
  hc_size(height = 750,width = 1350)




#===============================================================================
#                      
#                          时间维度 年、月 
#
#===============================================================================

treemap_data3 <- df_all%>%
  mutate(category = gsub(" ", "-", year),
         subcategory = gsub(" ", "-",mon),
         ratio=as.numeric(pct_chg),
         Volume=as.numeric(vol)) %>%
  select(category, subcategory,name,Volume,ratio)

pkmn_min <- treemap_data3 %>% 
  select(category,subcategory,name) %>%
  mutate(category = stringr::str_to_title(category)) %>% 
  mutate(subcategory= ifelse(is.na(subcategory), category, paste(category, "上市月份", subcategory))) %>%
  mutate(val = 1)

hchart(
  data_to_hierarchical(treemap_data3,c("category","subcategory","name"), 
                       size=Volume, colors =rev(viridis(12))),type = "treemap",
  allowDrillToNode = TRUE,tooltip = list(valueDecimals = FALSE),levels = lvl_opts) %>% 
  hc_chart(
    style = list(fontFamily = "Gloria Hallelujah")) %>% 
  hc_title(text = "上市公司行业分布",
           style = list(fontFamily = "Gloria Hallelujah",fontSize = "30px", color = "red", fontWeight = "bold")) %>% 
  hc_size(height = 750,width = 1350)



#===============================================================================
#
#
#                      次新股缩量图形
#
#
#===============================================================================

df_all<-dataprices%>%
  left_join(stocks_names)%>%
  filter(industry %in% c("银行"))%>%
  filter(pct_chg > 6 & pct_chg < 21)

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
  filter(n>6)%>%
  knitr::kable()


#===============================================================================
#  
#                      获取数据，设置start_date开始日期
#
#===============================================================================
library(dplyr)

stocks_name<-df_all%>%
  filter(pct_chg>8 & list_date<ymd('20260101'))

colnames(stocks_name)
  
library(purrr)

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



library(highcharter)
library(purrr)
library(tidyr)
library(tidyverse)
library(dplyr)
#===============================================================================
#  
#                      针对某个具体板块的缩量放量分析
#
#===============================================================================
china_stocksdata<-china_stocks%>%
  unnest(stock.prices)%>%
  mutate(Time=as.numeric(today()-ymd(list_date)))

#===============================================================================
#
#       每个行业龙头个股
#
#===============================================================================

NEWD<-china_stocksdata%>%group_by(name)%>%
  mutate(return=(Close-Open)/Open)%>%
  ungroup(name)

NEWD$return_type<-cut(NEWD$return, breaks=c(-0.1,-0.05,-0.015,0,0.03,0.06,0.08,0.12), 
                      labels =c("大跌","中跌","微跌","小涨","中涨","大涨次数","游资偏好"))


library(janitor)
library(lares)
library(DT)
library(stringi)
library(stringr)
COUNTC<-NEWD%>%
  filter(Date>ymd(Sys.Date()-300))%>%
  select(name,return_type)%>%left_join(df_all%>%select(name,ShiZhi,Timegroup))


COUNTC %>%
  tabyl(name, return_type)%>%
  select(name,大涨次数,游资偏好)%>%
  arrange(desc(游资偏好))%>%
  datatable(filter = 'top', options = list(
  pageLength = 30, autoWidth = TRUE),caption = htmltools::tags$caption(
    style = 'caption-side: Top; text-align: center;',
    '游资偏好', htmltools::em('"博弈精髓,把握涨停股"')
  ))

fntltp <- JS("function(){
  return this.point.x + ' ' +  this.series.yAxis.categories[this.point.y] + ': ' +
  Highcharts.numberFormat(this.point.value, 2);
}")

plotline <- list(
  color = "#fde725", value = 1963, width = 4, zIndex = 5,
  label = list(
    text = "Vaccine Intoduced", verticalAlign = "top",
    style = list(color = "#606060"), textAlign = "left",
    rotation = 0, y = -5
  )
)

hchart(
  china_stocksdata%>%filter(Date>ymd('20220101')), 
  "heatmap", hcaes(x =Date,y = name, value = Volume) ) %>%
  hc_colorAxis(
    stops = color_stops(10, viridisLite::inferno(10, direction = -1)),type = "logarithmic") %>%
  hc_yAxis(title = list(text = ""),reversed = TRUE, offset = -20,tickLength = 0,
    gridLineWidth = 0, minorGridLineWidth = 0,labels = list(style = list(fontSize = "15px"))) %>%
  hc_tooltip(formatter = fntltp) %>%
  hc_xAxis( plotLines = list(plotline)) %>%
  hc_title(text = "黄金坑战法监测系统") %>%
  hc_subtitle( text = "首战即决战，一战定乾坤，干大战定天下——LiXin Wu") %>% 
  hc_legend(layout = "horizontal", verticalAlign = "top", align = "left",valueDecimals = 0) %>%
  hc_size(height = 750,width = 1350)


#############################################反欺诈算法过滤可疑对象
library(tidyr)
library(tidyverse)
library(timetk)
library(purrr)
library(dplyr)


china_stocks %>%
  unnest(stock.prices)%>%head(5)


anoma_data<-china_stocks %>%
  unnest(stock.prices)%>%
  select(Date,Volume,name)%>%
  group_by(name) %>%
  tk_anomaly_diagnostics(Date,Volume)%>%
  filter(Date>ymd(today()-10))

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


##################################   针对某个板块异常特定的走势图
library(thematic)
library(tvthemes)
library(highcharter)
china_stocks %>%
  unnest(stock.prices)%>%
  arrange(desc(Date))%>%
  ggplot(aes(x=desc(Date),y=as.numeric(Close)),color=name)+
  geom_line()+
  facet_wrap(~name,ncol=5,scales = "free_y")+
  labs(x="日期",y="价格",title="西藏个股走势图")
  #theme_hildaDay(text.font = "Times", title.font = "Times",
                 legend.font = "Times")


library(plotly)
#subplot出现了BUG
china_stocks %>%
  unnest(stock.prices)%>%
  select(Date,Close,name)%>%
  group_by(name) %>%
  do(p = plot_ly(., x = ~desc(ymd(Date)), y = ~Close,name=~name)%>%
       add_lines) %>%
  subplot(nrows =5, shareX = TRUE)%>%
  layout(title = "天网作战系统之全局走势图")



#################################防欺诈算法异常数据表格

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
#####################################OK的
library(showtext)
showtext_auto(enable=TRUE)

a_plot1<-china_stocks %>%
  unnest(stock.prices)%>%
  select(Date,Volume,name)%>%
  filter(name %in% anoma_namedata$公司[1:12])%>%
  group_by(name) %>%
  plot_anomaly_diagnostics(Date, Volume,
                           .message = FALSE,
                           .facet_ncol = 3,
                           .ribbon_alpha = 0.25,
                           .interactive = FALSE)
a_plot1

ggplotly(a_plot1)%>%layout(title = "天网作战地图之反欺诈算法")

colnames(anoma_data)<-c("名称","日期","观察次数","周期性","趋势性","提示信号","季节调整","一级水平","二级水平","异常","一级维度","二级维度")

datatable(anoma_data)



##########################针对某个板块特定的走势图
library(plotly)
library(lubridate)
library(timetk)

unique(china_stocksdata$industry)
a_plot<-china_stocks %>%
  unnest(stock.prices)%>%
  #filter(industry %in% c("服饰","家居用品","食品","家用电器","乳制品","日用化工","白酒","化工原料","超市连锁"))%>%
  select(Date,Volume,name)%>%
  filter(name %in% anoma_namedata$公司)%>%
  group_by(name) %>%
  plot_anomaly_diagnostics(Date, Volume,
                           .message = FALSE,
                           .facet_ncol = 6,
                           .ribbon_alpha = 0.25,
                           .interactive = FALSE)
a_plot
ggplotly(a_plot)%>%layout(title = "天网作战地图之反欺诈算法")


################################聚类算法
library(corrplot)
library(gplots)
library(RColorBrewer)
library(heatmaply)
library(d3heatmap)
library(purrr)
library(dplyr)
library(tidyverse)

anoma_namedata$公司
####异常公司聚类
heatmapdata<-china_stocks%>%
  unnest(stock.prices)%>%
  select(Date,name,Close)%>%
  mutate(Close=as.numeric(Close))%>%
  spread(key=name, value=Close)%>%
  select(-Date)

heatmapdata<-china_stocks%>%
  unnest(stock.prices)%>%
  select(Date,name,Close)%>%
  mutate(Close=as.numeric(Close))%>%
  spread(key=name, value=Close)%>%
  select(-Date)

d3heatmap(heatmapdata,scale = "column", 
          show_grid = TRUE, anim_duration = 500,
          distfun = dist, hclustfun = hclust,height = 750,width=1350,main="天网作战聚类算法可视化")%>% 
  hmLegend(show = TRUE, title = "物以类聚人以群分", location = "tl") 



##############################规则算法及其相关系数分析

library(lubridate)
#
dataprices = pro(api_name="daily",trade_date='20250826')
df_all<-dataprices%>%
  left_join(stocks_names)%>%
  #filter(area %in% c('重庆','四川'))%>%
  filter(pct_chg > 7 & pct_chg < 21)
head(df_all)

df_all<-dataprices%>%left_join(stocks_names)%>%
   filter(industry %in% c("银行"))

library(stringr)
stocks_names$list_date<-ymd(stocks_names$list_date)

#df_all<-dataprices%>%left_join(stocks_names)

#"小金属","铜",
stocks_name<-df_all%>%filter(industry=="白酒")#%>%
  #filter(ymd(list_date) < ymd("20241001"))

stocks_name<-df_all%>%filter(ymd(list_date) < ymd("20240101"))


#stocks_name<-df_all%>%filter(name %in% unique(anoma_namedata$公司))
#带龙字的个股
#stocks_name<-df_all%>%
#  mutate(Long=stri_detect_fixed(name,"龙"))%>%
#  filter(Long==TRUE)


unique(stocks_name$industry)
#codes<-stocks_names%>%filter(name %in% stocks_name$name)

#ts_codes<-codes$ts_code

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

#colnames(prices) = stocks_name$name[i]

# 出现bug后增加的内容先检查维度是否匹配
length(stocks_name$name)
#ncol(prices)

# 如果数量一致，直接赋值
colnames(prices) <- stocks_name$name

# 或者只取前 n 个（防止越界）
n <- ncol(prices)
colnames(prices) <- stocks_name$name[1:n]



##########clean up and transform data 
for(j in 1:ncol(prices)) prices[, j] = na.locf(prices[, j])       # fill in

prices = prices[, apply(prices, 2, function(x) ! any(is.na(x)))]

log_returns = apply(prices, 2, function(x) diff(log(x)))

head(log_returns)


X = cor(log_returns)

#colnames(X)<-stocks_name$name[i]

colnames(X)<-stocks_name$name[1:n]

###############################Regularization
L = eigen(X, symmetric=TRUE)
plot(L$values, ylab="eigenvalues")
abline(v=10)

N = 10  # (use 1st 10 eigenvectors, set N larger to reduce regularization)
P = L$vectors[, 1:N] %*% ((1 / L$values[1:N]) * t(L$vectors[, 1:N]))
P = P / tcrossprod(sqrt(diag(P)))

library(corpcor)
library(igraph)

threshold = 0.92

Q = P * (P > quantile(P, probs=threshold))  

# thresholded precision matrix
g = graph.adjacency(Q, mode="undirected", weighted=TRUE, diag=FALSE) # ...expressed as a graph

# The rest of the code lumps any singletons lacking edges into a single 'unassociated' group shown in gray
# (also assigning distinct colors to the other groups).
#聚类分析
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

#############################天网作战系统之今日牛股社交网络

library(threejs)
graphjs(g, vertex.size=0.25, vertex.shape=colnames(X), edge.alpha=0.5,
        width=1350,height=950,main="天网作战地图之社交关系网",
        bg = "black")



graphjs(g, vertex.size=0.25, vertex.shape=colnames(X), edge.alpha=0.5,
        width=1350,height=850,main="天网作战地图之社交关系网",
        bg = "white")

graphjs(g, vertex.size=0.25, vertex.shape=colnames(X), edge.alpha=0.5,
        width=1350,height=850,main="天网作战地图之社交关系网",
        bg = "pink")

##############################主成分分析图

library(highcharter)
hchart(princomp(X, cor = TRUE))%>%hc_add_theme(hc_theme_ffx())%>%
  hc_yAxis( title = list(text = ""),
            reversed = TRUE, offset = -20,tickLength = 0,
            gridLineWidth = 0, 
            minorGridLineWidth = 0,
            labels = list(style = list(fontSize = "15px")) ) %>%
  #hc_tooltip(formatter = fntltp) %>%
  #hc_xAxis(plotLines = list(plotline)) %>%
  hc_title(text = "上市公司主成分分析") %>%
  hc_subtitle(text = "首战即决战，一战定乾坤，干大战定天下——LiXin Wu" ) %>% 
  hc_legend(layout = "horizontal",verticalAlign = "top",align = "left",valueDecimals = 0) %>%
  hc_size(height = 750,width = 800)



##############################相关系数分析图形
hchart(X)%>%
  hc_title(text = "天网作战系统之相关系数分析") %>%
  hc_subtitle(text = "首战即决战，一战定乾坤，干大战定天下——LiXin Wu" ) %>% 
  hc_legend(layout = "horizontal",verticalAlign = "top",align = "left",valueDecimals = 0) %>%
  hc_size(height = 800,width = 800)

library(corrplot)
testRes = cor.mtest(X, conf.level = 0.95)
corrplot(X, p.mat = testRes$p, sig.level = 0.10, order = 'hclust', addrect = 2)

corrplot(X, p.mat = testRes$p, method = 'circle', type = 'lower', insig='blank',
         addCoef.col ='black', number.cex = 0.8, order = 'AOE', diag=FALSE)

library(lares)


library(threejs)
graphjs(g, vertex.size =0.5, vertex.shape=colnames(X),
        edge.alpha=0.5,edge.width =5,
        font.main="96px Arial",vertex.label = colnames(X))


a_plot
##############################交叉相关性可视化
#install.packages("correlationfunnel") 
library(correlationfunnel)
dftt<-as.data.frame.array(X)

head(dftt)
corr(dftt, method = "spearman")

data(dft) # Titanic dataset

# Only data with no plot
corr_cross(dftt, plot = TRUE, top = 3)

# Show only most relevant results filtered by pvalue
corr_cross(dftt, rm.na = TRUE, max_pvalue = 0.05, top = 15)

# Cross-Correlation for certain variables

# Cross-Correlation max values per category
corr_cross(dftt, type = 2, top =NA)


dft%>%plot_correlation_funnel()

# Cross-Correlation for certain variables
corr_cross(dft, contains = c("Survived", "Fare"))



##############################词云图

yesterday1<-str_remove(str_remove(Sys.Date()-1, "-"),"-")
yesterday2<-str_remove(str_remove(Sys.Date()-2, "-"),"-")
yesterday3<-str_remove(str_remove(Sys.Date()-5, "-"),"-")

yesterday4<-str_remove(str_remove(Sys.Date()-6, "-"),"-")
dataprices = pro(api_name="daily",trade_date=today)%>%filter(pct_chg>1)%>%left_join(stocks_names)%>%filter(area %in% c('四川','重庆'))
dataprices1 <- pro(api_name="daily",trade_date=yesterday1)%>%filter(pct_chg>1)%>%left_join(stocks_names)%>%filter(area %in% c('四川','重庆'))
dataprices2 <- pro(api_name="daily",trade_date=yesterday2)%>%filter(pct_chg>1)%>%left_join(stocks_names)%>%filter(area %in% c('四川','重庆'))
dataprices3 <- pro(api_name="daily",trade_date=yesterday3)%>%filter(pct_chg>1)%>%left_join(stocks_names)%>%filter(area %in% c('四川','重庆'))

library(jiebaR)

dataprices_all<-rbind(dataprices,dataprices1,dataprices2)

demoFreq<-freq(dataprices_all$name)%>%filter(freq>=2)

library("d3wordcloud")
d3wordcloud(demoFreq$word, demoFreq$freq,size.scale = "linear")
d3wordcloud(demoFreq$word, demoFreq$freq ,size.scale = "log",
            rotate.min = 15, rotate.max = 30,rangesizefont = c(10, 20))


#####################################
# Changing number of digits for correlation coeffcient
# --------------------------------
library(ggcorrplot)
library(corrplot)
library(showtext)
library(plotly)

showtext_auto(enable=TRUE)
font_add('Songti','Songti.ttc')
font_families()
corrplot(X, type = 'lower', order = 'hclust', tl.col = 'black',
         cl.ratio = 0.2, tl.srt = 45, col = COL2('PuOr', 10)) 



a<-ggcorrplot(X,
              hc.order = TRUE, type = "lower",
              outline.color = "white",
              ggtheme = ggplot2::theme_gray,
              colors = c("#6D9EC1", "white", "#E46726"))

a

ggplotly(a)

a<-ggcorrplot(X, hc.order = TRUE, 
              type = "lower", 
              lab = TRUE, 
              lab_size = 3, 
              method="circle", 
              colors = c("tomato2", "white", "springgreen3"), 
              title="皮尔逊相关系数分析", 
              ggtheme=theme_hildaDay(ticks = TRUE,
                                     legend.position = "left"))
ggplotly(a)

??corr_cross

head(X)
dftt<-as.data.frame.array(X)

head(dftt)
corr(dftt, method = "spearman")

data(dft) # Titanic dataset

# Only data with no plot
corr_cross(dftt, plot = TRUE, top = 3)

# Show only most relevant results filtered by pvalue
corr_cross(dftt, rm.na = TRUE, max_pvalue = 0.05, top = 15)

# Cross-Correlation for certain variables

# Cross-Correlation max values per category
corr_cross(dftt, type = 2, top = NA)

