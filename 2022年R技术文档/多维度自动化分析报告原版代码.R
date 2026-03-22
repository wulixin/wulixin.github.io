

#####构建动量理论下的多头策略

##动量效应（Momentum effect）一般又称“惯性效应”。
#动量效应是由Jegadeesh和Titman（1993）提出的，是指股票的收益率有延续原来的运动方向的趋势，
#即过去一段时间收益率较高的股票在未来获得的收益率仍会高于过去收益率较低的股票。


###############  周热度排行榜
##############   月热度排行榜
#### 行业维度 交易量，上涨公司数量
#### 大趋维度     四个维度涨跌幅数量
### 板块维度
### 事件驱动，热门概念 
library(stringi)
library(stringr)
library(Tushare)
library(highcharter)
library(dplyr)
library(tidyverse)
library(tidyr)
pro <-pro_api(token ='fe8102bf83f5f83f6608aa46fa5e985c534c227786236a1192e5fd55')


today<-str_remove(str_remove(Sys.Date(), "-"),"-")
yesterday<-str_remove(str_remove(Sys.Date()-3, "-"),"-")
yesterday1<-str_remove(str_remove(Sys.Date()-3, "-"),"-")
yesterday2<-str_remove(str_remove(Sys.Date()-4, "-"),"-")
yesterday3<-str_remove(str_remove(Sys.Date()-5, "-"),"-")
yesterweek<-str_remove(str_remove(Sys.Date()-7, "-"),"-")
latest_month<-str_remove(str_remove(Sys.Date()-30, "-"),"-")


stocks_names<-pro(api_name = 'stock_basic')
dataprices <- pro(api_name="daily",trade_date=today)#%>%filter(pct_chg>1)

dataprices$pct_type<-cut(dataprices$pct_chg,breaks=c(-20,-4,0,3,8,20),labels=c("大跌","小跌","小涨","中涨","大涨"))

#####大趋势维度 #####################加入了仓位控制提醒
library(janitor)
library(gt)
t1<-dataprices%>%tabyl(pct_type)

dznum<-t1%>%filter(pct_type %in% c("小涨","中涨","大涨"))%>%select(n)%>%sum()
subtitle1<-print(ifelse(dznum>2500,"投资策略：近期大盘形势很好,放手操作",
                        ifelse(dznum>2000,"投资策略：大盘处于震荡阶段不好操作",
                               ifelse(dznum>1000,"投资策略：大盘形势转差,控制仓位,谨慎操作",
                                      ifelse(dznum>500,"投资策略：投资情绪低迷,控制仓位","降低仓位到三分之一or空仓！")))))
qstable<-t1 %>%
  adorn_totals("row") %>%
  adorn_pct_formatting()
colnames(qstable)<-c("涨跌类型","数量","百分比","交叉百分比")

qstable%>%gt()%>%
  gt_preview()%>%tab_header(title = "大趋势分析表",subtitle =subtitle1)%>%
  tab_style(
    style = list(
      cell_fill(color = "lightcyan"),
      "font-variant: small-caps;"
    ),
    locations = cells_body(columns ='数量')
  )%>%
  tab_footnote(
    footnote = "首战即决战，一战定乾坤，干大战定天下。——LIXIN WU ",
    locations = cells_column_labels(
      columns = '数量' ) )
  
########################################################

#datapricesAll<-rbind(dataprices,dataprices1,dataprices2,dataprices3)
df_all<-dataprices%>%left_join(stocks_names)


############  多头策略
yesterday<-str_remove(str_remove(Sys.Date()-3, "-"),"-")
yesterday1<-str_remove(str_remove(Sys.Date()-1, "-"),"-")
yesterday2<-str_remove(str_remove(Sys.Date()-4, "-"),"-")
yesterday3<-str_remove(str_remove(Sys.Date()-5, "-"),"-")
yesterweek<-str_remove(str_remove(Sys.Date()-7, "-"),"-")


dataprices1 <- pro(api_name="daily",trade_date=yesterday1)%>%filter(pct_chg>2)
dataprices2 <- pro(api_name="daily",trade_date=yesterday2)%>%filter(pct_chg>2)
dataprices3 <- pro(api_name="daily",trade_date=yesterday3)%>%filter(pct_chg>2)

Top20<-dataprices%>%
  filter(ts_code %in% dataprices1$ts_code)%>%
  filter(ts_code %in% dataprices2$ts_code)%>%
  filter(ts_code %in% dataprices3$ts_code)%>%
  left_join(stocks_names)%>%
  select(pct_chg,name,industry,ts_code,market,trade_date)%>%
  arrange(desc(pct_chg))

colnames(Top20)<-c("热度","名称","行业","代码","板块","交易时间")

datatable(Top20,filter = 'top', options = list(
           pageLength = 20, autoWidth = TRUE),caption = htmltools::tags$caption(
         style = 'caption-side: Top; text-align: center;',
        '个股热度排名', htmltools::em('"近期多头策略个股热度排行"')
       ))

library(flexpivot)
library(flextable)
library(janitor)
library(purrr)

######################板块维度
Top20%>%
  tabyl(market)%>%
  adorn_totals("row") %>%
  adorn_pct_formatting()%>%
  arrange(desc(n))%>%
  knitr::kable()


cxcount<-as.data.frame(Top20%>%count(market))
cxcount$year<-as.factor(cxcount$market)
cxcount$n<-as.numeric(cxcount$n)

####################板块偏好度

hchart(cxcount,type="column",hcaes(x=market,y=n),
       ptions3d = list(enabled = TRUE, beta = 15, alpha = 15))%>%
  hc_subtitle(text = "最近三个交易日热门股票板块分布",align = "left",style = list(color = "#2b908f", fontWeight = "bold"))%>%
  hc_legend(enabled = TRUE) %>%
  hc_plotOptions(series = list(
    boderWidth = 0,
    dataLabels = list(enabled = TRUE)))%>%
  hc_add_theme(hc_theme_darkunica())%>%
  hc_labels(items = list(
    list(html = "<p>QuandlFinance <b>lixin wu</b><br></p>",
         style = list(left = "150%",top = "150%" )) ) )%>%
  hc_rangeSelector(
    verticalAlign = "bottom",selected = 4)

################获取成交量数据
dataprices <- pro(api_name="daily",trade_date=yesterday)

df_all<-dataprices%>%left_join(stocks_names)

cxcount2<-as.data.frame(df_all%>%count(industry))
cxcount2$pecent<-(cxcount2$n/sum(cxcount2$n))
head(cxcount2)

highchart() %>% 
  # Data
  hc_add_series(cxcount, "column",
                hcaes(x = market,y = n), name = "公司数量") %>%
  hc_add_series(cxcount2,"pie",
                hcaes(name = industry,y = pecent), name = "行业占比") %>%
  # Options for each type of series
  hc_plotOptions(
    series = list(showInLegend = TRUE,
                  pointFormat = "{point.y}",colorByPoint = TRUE),
    pie = list(center = c('70%', '25%'),size = 200,
               dataLabels = list(enabled = TRUE) )  ) %>%
  # Axis
  hc_yAxis(title = list(text = "上市公司数量"),
           labels = list(format = "{value}") ) %>% 
  hc_xAxis( categories = cxcount$market) %>%
  hc_plotOptions(series = list(boderWidth = 0,dataLabels = list(enabled = TRUE))) %>%
  # Titles, subtitle, caption and credits
  hc_title(text = "羊群战略——板块维度+行业维度") %>% 
  hc_subtitle(text = "发现羊群最新动态,把握群体共性与个体差异性") %>%
  hc_caption(text = "金融行为学学派</br>武利鑫————首战即决战，一战定乾坤，专注于为大资金干大战定天下") %>% 
  hc_credits(enabled = TRUE, text = "来自QuandlChina",
             href = "https://wulixin.github.io//QuandlFinance//home.html",style = list(fontSize = "12px") ) %>% 
  hc_size( height = 600)%>%
  hc_add_theme(hc_theme_sandsignika())



##########################行业+中小盘

df_all$ShiZhi<-cut(df_all$amount,breaks=quantile(df_all$amount),
                   labels=c("迷你盘","小盘股","中盘股","大盘股"))
### 统计数据    可以使用的第一个大图   

cxcount3<-as.data.frame(df_all%>%filter(pct_chg>3)%>%count(industry)%>%arrange(desc(n)))
cxcount3$industry<-as.factor(cxcount3$industry)
cxcount3$n<-as.numeric(cxcount3$n)

cxcount4<-as.data.frame(df_all%>%filter(pct_chg>3)%>%count(ShiZhi))
cxcount4$pecent<-(cxcount4$n/sum(cxcount4$n))

highchart() %>% 
  hc_add_series(cxcount3, "column",
                hcaes(x = industry,y = n), name = "主力流入行业") %>%
  hc_add_series(cxcount4,"pie",
                hcaes(name = ShiZhi,y = pecent), name = "资金偏好公司特征") %>%
  # Options for each type of series
  hc_plotOptions(
    series = list(showInLegend = TRUE,pointFormat = "{point.y}",colorByPoint = TRUE),
    pie = list(center = c('70%', '25%'),size = 200,dataLabels = list(enabled = TRUE) )  ) %>%
  # Axis
  hc_yAxis(title = list(text = "涨幅超过3%的公司数量"),labels = list(format = "{value}") ) %>% 
  hc_xAxis( categories = cxcount3$industry) %>%
  hc_plotOptions(series = list(boderWidth = 0,dataLabels = list(enabled = TRUE))) %>%
  # Titles, subtitle, caption and credits
  hc_title(text = "羊群战略——板块维度+中小盘维度") %>% 
  hc_subtitle(text = "发现羊群最新动态,把握群体共性与个体差异性") %>%
  hc_caption(text = "金融行为学学派</br>武利鑫————首战即决战，一战定乾坤，专注于为大资金干大战定天下") %>% 
  hc_credits(enabled = TRUE, text = "来自QuandlChina",
             href = "https://wulixin.github.io//QuandlFinance//home.html",style = list(fontSize = "12px") ) %>% 
  hc_size( height = 600)%>%
  hc_add_theme(hc_theme_sandsignika())


###########################################################################


###########################行业+热门强势个股排行


#latest_week<-Sys.Date()-days(15)
#latest_day<-Sys.Date()-days(1)
#latest_week<-Sys.Date()-days(15)
#latest_month<-Sys.Date()-days(30)


##最近三个交易日数据的获取

start_date<-str_remove(str_remove(Sys.Date()-300, "-"),"-")


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

Top<-dataprices%>%filter(pct_chg>3)%>%left_join(stocks_names)
stocks_name<-stocks_names%>%
     filter(name %in% Top$name)
  

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





###  超级强势股 连续大涨连两天的股票


### 超级强势股 连续小涨两天的股票


library(stringi)
library(stringr)
head(dataprices)
sum(dataprices$vol)
sum(dataprices$amount)*1000

#时间变量的生成
df_all$list_date<-ymd(df_all$list_date)
df_all$year<-year(df_all$list_date)
df_all$mon<-as.factor(paste('上市月份',month(df_all$list_date),seq=''))




