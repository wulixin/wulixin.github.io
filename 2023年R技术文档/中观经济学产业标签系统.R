#######################################################
#
#              中观经济学/产业经济学看资本市场
#
######################################################

{library(tvthemes)
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

today<-str_remove(str_remove(Sys.Date(), "-"),"-")}


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


dataprices = pro(api_name="daily",trade_date='20230215')
stocks_names<-pro(api_name = 'stock_basic')

#write.csv(stocks_names,"//Users//wulixin//Desktop//stocksfull.csv")

#第三节：按产业地位分类的产业

#第一：基础产业 产业结构体系中为其它产业的发展提供基本条件并为大多数产业提供服务的产业；
#第二：瓶颈产业  产业结构体系中未得到应有发展而已经严重制约其它产业和国民经济发展的产业
#第三：支柱产业   在产业结构体系的总产出占比较大的产业
#第四：主导产业   在产业结构体系中处于主体地位并起着产业发展的引导和支撑作用
#第五：先行产业：  在产业结构体系中因关系到国民经济发展而必须优先发展的产业

unique(stocks_names$industry)


##发展趋势
XianXing<-c("互联网","机床制造","半导体","软件服务","IT设备","元器件","汽车服务",
            "汽车配件","通信设备","新型电力","汽车整车","电气设备","船舶","航空","生物制药","化学制药","仓储物流","医药商业")

stocks_names$CY_FaZhanQS<-ifelse(stocks_names$industry %in% XianXing,"政策支持行业","行业成熟格局稳定")

head(stocks_names)
                                                                  
#技术先进
GaoXinJiShu<-c("机床制造","半导体","IT设备","元器件","通信设备","新型电力","电气设备","工程机械","专用机械","航空","生物制药","化学制药")
stocks_names$CY_JiShuXJ<-ifelse(stocks_names$industry %in% GaoXinJiShu,"高新技术产业","传统行业产业")
head(stocks_names)

#产品供求状况
ChangXian<-c("黄金","服饰","家居用品","房产服务","全国地产","区域地产","家用电器","电器连锁","林业","水务",
             "摩托车","化纤","橡胶","玻璃","汽车整车","电气设备","建筑工程","其他建材","装修装饰","水泥","染料涂料",
             "化工机械","纺织机械","工程机械","农用机械","轻工机械","机械基件","专用机械","化工原料","电器仪表","机床制造","运输设备")

DuanXian<-c("啤酒", "食品","软饮料","乳制品","白酒","红黄酒","饲料","生物制药","医疗保健","医药商业","中成药","化学制药",
            "日用化工","造纸", "塑料","汽车配件","石油加工","石油贸易","农药化肥","农业综合","渔业","种植业","铝",
            "小金属","铜","钢加工","陶瓷","普钢","焦炭加工","矿物制品","特种钢","供气供热","煤炭开采","铅锌","纺织")

stocks_names$CY_ChanPinGQ<-ifelse(stocks_names$industry %in% ChangXian,"长寿命周期产品",
                                  ifelse(stocks_names$industry %in% DuanXian,"短期消费服务","其它服务"))

#产业类型

# 1 劳动密集型 服装纺织，酒店餐饮，食品加工，文体教育用品，金属制品加工，日用百货业=
LaoDongMJ<-c("广告包装","影视音像","超市连锁","旅游景点","酒店餐饮","旅游服务","文教休闲","出版业","商品城","百货","中成药",
             "商贸代理","批发业","其他商业","仓储物流","电信运营","综合类" ,"渔业" ,"种植业" ,"林业","水务","农业综合",
             "啤酒", "食品","软饮料","乳制品","白酒","红黄酒","饲料","黄金","服饰","家居用品","房产服务")


# 2 资本密集型交通，钢铁，机械，石油，化学，基础工业和重化工业

ZiBenMJ<-c("铁路","公路","公共交通","路桥","机场","港口","空运","水运","铝","小金属","铜","钢加工","陶瓷","普钢","焦炭加工","矿物制品",
           "特种钢","供气供热","煤炭开采","铅锌", "水力发电","火力发电","石油开采","石油加工","石油贸易" ,"化工机械","纺织机械","工程机械",
           "农用机械","轻工机械","机械基件","专用机械","农药化肥","化工原料","染料涂料","汽车整车","电器仪表","运输设备",
           "汽车配件","纺织","日用化工","造纸", "塑料","建筑工程","其他建材","装修装饰","水泥",
           "医疗保健","医药商业","全国地产","区域地产","证券","银行","保险","多元金融","园区开发","环境保护")


# 3 技术/知识密集型 

GKeJiMJ<-c("互联网","机床制造","半导体","IT设备","元器件","通信设备",
           "新型电力","电气设备","工程机械","专用机械","航空","软件服务","家用电器","电器连锁","生物制药","化学制药")


stocks_names$CY_ChanYeType<-ifelse(stocks_names$industry %in% LaoDongMJ,"劳动密集型",
                                  ifelse(stocks_names$industry %in% ZiBenMJ,"资本密集型",
                                         ifelse(stocks_names$industry %in% GKeJiMJ,"知识技术密集型","其它行业")))


#七：产业关联方式分类法：
#向前关联效应，向后关联效应，上游产业链，
# 1 技术关联分类法；2原料关联分类法：3用途关联分类法：4方向关联分类法：5战略关联分类法：
## 这个位置将生成 所属产业链以及所属产业链的地位


latest_week<-Sys.Date()-days(15)
latest_day<-Sys.Date()-days(1)
latest_week<-Sys.Date()-days(15)
latest_month<-Sys.Date()-days(30)
start_date<-Sys.Date()-days(552)

##  数据的获取
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

head(stocks_names)

ts_name<-stocks_names%>%
  select(ts_code)
start_date<-Sys.Date()-years(1)
ts_codes<-ts_name$ts_code

##得出最新的数据
df_all<-dataprices%>%
  left_join(stocks_names)
df_all$list_date<-ymd(df_all$list_date)

colnames(df_all)
########################################################################################
## 最近一个年的次新股表现

cixin<- df_all%>%
  select(ts_code,name,industry,CY_FaZhanQS,CY_JiShuXJ,CY_ChanPinGQ,CY_ChanYeType,market,close,pct_chg,vol,change,list_date,area)%>%
  mutate(Year=year(list_date),Month=month(list_date),Time=as.numeric(today()-list_date))%>%
  arrange(desc(pct_chg))

colnames(cixin)
########################

###########】
##每个月上市的次新股   可以用 树形图来表示   股价用做图形大小  

## 每个月上市的次新股， 每天的涨跌幅，正负可视化，矩形图

## 今年热门行业，热门个股 

##最大收益预期与最小收益预期的判断

## 主流反弹个股，主流反弹板块    反弹强度最强的个股



## 最近一周资金流入重点板块

ziJing<-cixin%>%filter(industry %in% c("元器件","汽车配件","电气设备","化工原料","专用机械","软件服务"))%>%
  arrange(desc(pct_chg))

#############################板块维度的细分

##  科创板涨幅牛股

kechuang <- cixin%>%
  filter(market == "科创板")%>%
  arrange(desc(pct_chg))

##  创业板涨幅牛股

chuangyeban <- cixin%>%
  filter(market == "创业板")%>%
  arrange(desc(pct_chg))

## 北交所
beijiao<- cixin%>%
  filter(market =="北交所")%>%
  arrange(desc(pct_chg))

##  主板涨幅牛股
zhuban<- cixin%>%
  filter(market %in% c("主板","中小板"))%>%
  arrange(desc(pct_chg))

##############################价格维度的细分 

## 高价活跃股
baiyuan <- cixin%>%
  filter(close>20 & pct_chg>4 )%>%
  arrange(desc(pct_chg))

## 低价活跃股
shiyuan <- cixin%>%
  filter(close<20 & pct_chg>4)%>%
  arrange(desc(pct_chg))


##########################

library(janitor)
library(purrr)

count<-cixin%>%
  tabyl(industry)%>%
  adorn_totals("row") %>%
  adorn_pct_formatting()%>%
  arrange(desc(n))%>%
  filter(n>5)%>%
  knitr::kable()

shiyuan%>%
  tabyl(industry,CY_FaZhanQS,market, show_missing_levels = FALSE) %>%
  adorn_totals("row") %>%
  adorn_percentages("all") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns %>%
  adorn_title%>%
  knitr::kable()

##############################




###########################时间维度
cixin_15 <- cixin%>%
  filter(Time>10 & Time <30)%>%
  arrange(desc(pct_chg))

cixin_60 <- cixin%>%
  filter(Time>30 & Time <60)%>%
  arrange(desc(pct_chg))


cixin_2years <- cixin%>%
  filter(Time>60 & Time <900)%>%
  arrange(desc(pct_chg))

colnames(cixin_2years)<-c("代码","公司名称","行业","产业趋势","产业技术","产品类型",
                      "产业类型","板块","收盘价","涨跌幅","交易量","价格变化","上市时间","地区",
                      "上市年份","上市月份","时间")

colnames(cixin_60)<-c("代码","公司名称","行业","产业趋势","产业技术","产品类型",
                      "产业类型","板块","收盘价","涨跌幅","交易量","价格变化","上市时间","地区",
                      "上市年份","上市月份","时间")
colnames(cixin_15)<-c("代码","公司名称","行业","产业趋势","产业技术","产品类型",
                      "产业类型","板块","收盘价","涨跌幅","交易量","价格变化","上市时间","地区",
                      "上市年份","上市月份","时间")

colnames(shiyuan)<-c("代码","公司名称","行业","产业趋势","产业技术","产品类型",
                     "产业类型","板块","收盘价","涨跌幅","交易量","价格变化","上市时间","地区",
                     "上市年份","上市月份","时间")

colnames(baiyuan)<-c("代码","公司名称","行业","产业趋势","产业技术","产品类型",
                     "产业类型","板块","收盘价","涨跌幅","交易量","价格变化","上市时间","地区",
                     "上市年份","上市月份","时间")

colnames(zhuban)<-c("代码","公司名称","行业","产业趋势","产业技术","产品类型",
                    "产业类型","板块","收盘价","涨跌幅","交易量","价格变化","上市时间","地区",
                    "上市年份","上市月份","时间")

colnames(kechuang)<-c("代码","公司名称","行业","产业趋势","产业技术","产品类型",
                      "产业类型","板块","收盘价","涨跌幅","交易量","价格变化","上市时间","地区",
                      "上市年份","上市月份","时间")

colnames(chuangyeban)<-c("代码","公司名称","行业","产业趋势","产业技术","产品类型",
                         "产业类型","板块","收盘价","涨跌幅","交易量","价格变化","上市时间","地区",
                         "上市年份","上市月份","时间")

colnames(ziJing)<-c("代码","公司名称","行业","产业趋势","产业技术","产品类型",
                    "产业类型","板块","收盘价","涨跌幅","交易量","价格变化","上市时间","地区",
                    "上市年份","上市月份","时间")

colnames(beijiao)<-c("代码","公司名称","行业","产业趋势","产业技术","产品类型",
                     "产业类型","板块","收盘价","涨跌幅","交易量","价格变化","上市时间","地区",
                     "上市年份","上市月份","时间")

colnames(cixin)<-c("代码","公司名称","行业","产业趋势","产业技术","产品类型",
                     "产业类型","板块","收盘价","涨跌幅","交易量","价格变化","上市时间","地区",
                     "上市年份","上市月份","时间")

head(cixin)
count<-cixin%>%
  filter('涨跌幅'>5)%>%
  tabyl('行业')%>%
  adorn_totals("row") %>%
  adorn_pct_formatting()%>%
  arrange(desc(n))%>%
  filter(n>5)

library(officer)
library(openxlsx)
# initialize a workbook 
wb<-createWorkbook()

# add a worksheet 

addWorksheet(wb,sheetName="全局总览")
addWorksheet(wb,sheetName="小美女")
addWorksheet(wb,sheetName="老姑娘")
addWorksheet(wb,sheetName="资金流入板块")
addWorksheet(wb,sheetName="上市15天")
addWorksheet(wb,sheetName="上市60天")
addWorksheet(wb,sheetName="近三年上市")
addWorksheet(wb,sheetName="创业板")
addWorksheet(wb,sheetName="科创板")
addWorksheet(wb,sheetName="主板")
addWorksheet(wb,sheetName="北交所")

#add data 
#insertImage(wb, "小美女","//Users//wulixin//Desktop//kehu.png",
#            startRow = 10, startCol = 5, width = 6, height = 5)

#insertImage(wb, "上市15天","//Users//wulixin//Desktop//kehu.png",
#            startRow = 7, startCol = 8, width = 6, height = 5)

writeDataTable(wb,sheet="全局总览",x=count,tableStyle = "TableStyleLight2",withFilter = openxlsx_getOp("withFilter", TRUE))

writeDataTable(wb,sheet="资金流入板块",x=ziJing,tableStyle ="TableStyleMedium9",withFilter = openxlsx_getOp("withFilter", TRUE))
writeDataTable(wb,sheet="上市15天",x=cixin_15,tableStyle ="TableStyleMedium10",withFilter = openxlsx_getOp("withFilter", TRUE))
writeDataTable(wb,sheet="上市60天",x=cixin_60,tableStyle ="TableStyleMedium4",withFilter = openxlsx_getOp("withFilter", TRUE))
writeDataTable(wb,sheet="近三年上市",x=cixin_2years,tableStyle ="TableStyleMedium4",withFilter = openxlsx_getOp("withFilter", TRUE))

writeDataTable(wb,sheet="小美女",x=shiyuan,tableStyle = "TableStyleLight2",withFilter = openxlsx_getOp("withFilter", TRUE))
writeDataTable(wb,sheet="老姑娘",x=baiyuan, tableStyle = "TableStyleLight6",withFilter = openxlsx_getOp("withFilter", TRUE))
writeDataTable(wb,sheet="主板",x=zhuban,tableStyle ="TableStyleMedium6",withFilter = openxlsx_getOp("withFilter", TRUE))
writeDataTable(wb,sheet="创业板",x=chuangyeban,tableStyle ="TableStyleMedium7",withFilter = openxlsx_getOp("withFilter", TRUE))
writeDataTable(wb,sheet="科创板",x=kechuang,tableStyle ="TableStyleMedium2",withFilter = openxlsx_getOp("withFilter", TRUE))
writeDataTable(wb,sheet="北交所",x=beijiao,tableStyle ="TableStyleLight4",withFilter = openxlsx_getOp("withFilter", TRUE))



saveWorkbook(wb,"//Users//wulixin//Desktop//A股多维度数据策略支持分析.xlsx",overwrite=TRUE)
openXL("//Users//wulixin//Desktop//A股多维度数据策略支持分析.xlsx")



################################################################################
##
## 反欺诈算法针对特点板块的活泼性个股进行研究
##
##
################################################################################
industrys<- c("环境治理")
unique(df_all$industry)
cixin_2022<- df_all%>%
  filter(industry %in% industrys)%>%
  filter(list_date>ymd('2022-01-01'))%>%
  select(ts_code,name,industry,CY_FaZhanQS,CY_JiShuXJ,CY_ChanPinGQ,CY_ChanYeType,market,close,pct_chg,vol,change,list_date,area)%>%
  mutate(Year=year(list_date),Month=month(list_date),Time=as.numeric(today()-list_date))%>%
  arrange(desc(pct_chg))

cixin_2021<- df_all%>%
  filter(industry %in% industrys)%>%
  filter(list_date>ymd('2021-01-01') &list_date<ymd('2022-01-01'))%>%
  select(ts_code,name,industry,CY_FaZhanQS,CY_JiShuXJ,CY_ChanPinGQ,CY_ChanYeType,market,close,pct_chg,vol,change,list_date,area)%>%
  mutate(Year=year(list_date),Month=month(list_date),Time=as.numeric(today()-list_date))%>%
  arrange(desc(pct_chg))

cixin_2020<- df_all%>%
  filter(industry %in% industrys)%>%
  filter(list_date>ymd('2020-01-01') &list_date<ymd('2021-01-01'))%>%
  select(ts_code,name,industry,CY_FaZhanQS,CY_JiShuXJ,CY_ChanPinGQ,CY_ChanYeType,market,close,pct_chg,vol,change,list_date,area)%>%
  mutate(Year=year(list_date),Month=month(list_date),Time=as.numeric(today()-list_date))%>%
  arrange(desc(pct_chg))

cixin_other<- df_all%>%
  filter(industry %in% industrys)%>%
  filter(list_date<ymd('2020-01-01'))%>%
  select(ts_code,name,industry,CY_FaZhanQS,CY_JiShuXJ,CY_ChanPinGQ,CY_ChanYeType,market,close,pct_chg,vol,change,list_date,area)%>%
  mutate(Year=year(list_date),Month=month(list_date),Time=as.numeric(today()-list_date))%>%
  arrange(desc(pct_chg))



library(DataExplorer)
library(showtext)
showtext_auto(enable=TRUE)

cixin<-cixin_2022
cixin$change_type<-ifelse(cixin$pct_chg>4,"表现很好",
                          ifelse(cixin$pct_chg>1,"表现一般","表现不佳"))



today_best<-cixin
cixin_2020$change_type<-ifelse(cixin_2020$pct_chg>4,"表现很好",
                          ifelse(cixin_2020$pct_chg>1,"表现一般","表现不佳"))

cixin_2021$change_type<-ifelse(cixin_2021$pct_chg>4,"表现很好",
                          ifelse(cixin_2021$pct_chg>1,"表现一般","表现不佳"))



#%>%
#  filter(CY_FaZhanQS=="朝阳产业" & CY_JiShuXJ=="传统行业")%>%
#  filter(market=="科创板" & CY_ChanYeType=="知识技术密集型")
colnames(today_best)
colnames(today_best)<-c("代码","公司名称","行业","产业趋势","产业技术","产品类型",
                        "产业类型","板块","收盘价","涨跌幅","交易量","价格变化","上市时间","地区",
                        "上市年份","上市月份","时间","表现情况")
colnames(cixin_2021)<-c("代码","公司名称","行业","产业趋势","产业技术","产品类型",
                        "产业类型","板块","收盘价","涨跌幅","交易量","价格变化","上市时间","地区",
                        "上市年份","上市月份","时间","表现情况")
colnames(cixin_2020)<-c("代码","公司名称","行业","产业趋势","产业技术","产品类型",
                        "产业类型","板块","收盘价","涨跌幅","交易量","价格变化","上市时间","地区",
                        "上市年份","上市月份","时间","表现情况")



head(cixin)

stocks_name<-cixin_other%>%filter(pct_chg>6)

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

library(tvthemes)
library(lubridate)
library(forcats) 
library(tidyr)
library(tidyverse)
library(dplyr)
china_stocks %>%
  unnest(stock.prices)%>%
  arrange(desc(Date))%>%
  ggplot(aes(x=desc(Date),y=as.numeric(Close)),color=name)+
  geom_line()+
  facet_wrap(~name,ncol=4,scales = "free_y")+
  labs(x="日期",y="价格",title="个股走势图")+
  theme_hildaDay(ticks = TRUE,
                 legend.position = "top")

##这幅图很牛，之后可以做其它用途
china_stocks %>%
  unnest(stock.prices)%>%
  select(Date,Close,name)%>%
  group_by(name) %>%
  do(p = plot_ly(., x = ~desc(ymd(Date)), y = ~Close,name=~name)%>%add_lines) %>%
  subplot(nrows =5, shareX = TRUE)


library(dplyr)
library(timetk)

anoma_data<-china_stocks %>%
  unnest(stock.prices)%>%
  select(Date,vol,name)%>%
  group_by(name) %>%
  tk_anomaly_diagnostics(Date, vol)%>%
  filter(Date>ymd(today()-3))

head(anoma_data)
anoma_name<-anoma_data%>%filter(anomaly=="Yes")

library(timetk)
china_stocks %>%
  unnest(stock.prices)%>%
  select(Date,Volume,name)%>%
  group_by(name) %>%
  filter(name %in% anoma_name$name)%>%
  plot_anomaly_diagnostics(Date, vol,
                           .message = FALSE,
                           .facet_ncol = 4,
                           .ribbon_alpha = 0.25,
                           .interactive = FALSE)


colnames(anoma_data)<-c("名称","日期","观察次数","周期性","趋势性","提示信号","季节调整","一级水平","二级水平","异常","一级维度","二级维度")

anoma_data
######行业分布
library(proto)
library(ggmap)
library(DT)
library(highcharter)
library(viridisLite)
library(treemap)
library(flexdashboard)
library(RJSONIO)
treemap_data <- china_stocks%>%
  unnest(stock.prices)%>%
  mutate(category = gsub(" ", "-", market),
         subcategory = gsub(" ", "-", industry),
         ratio=mean.log.returns/sd.log.returns,
         Volume=as.numeric(Volume)) %>%
  select(category, subcategory,name,Volume,ratio)%>%
  sample_n(100)

tm <- treemap(treemap_data, index = c("category","subcategory","name"),
              vSize = "Volume", vColor = "ratio",
              type = "value", palette = rev(viridis(6)),title="股票板块分布")


#### 相关系数

library(ggplot2)
library(ggcorrplot)
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
##########clean up and transform data 
for(j in 1:ncol(prices)) prices[, j] = na.locf(prices[, j])       # fill in
prices = prices[, apply(prices, 2, function(x) ! any(is.na(x)))]

log_returns = apply(prices, 2, function(x) diff(log(x)))

X = cor(log_returns)
colnames(X)<-stocks_name$name[i]
corr <- round(cor(log_returns), 1)
# Plot
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="皮尔逊相关系数分析", 
           ggtheme=theme_hildaDay(ticks = TRUE,
                                  legend.position = "left"))



####heatmap 
library(corrplot)
library(gplots)
library(RColorBrewer)
library(heatmaply)
library(d3heatmap)
heatmapdata<-china_stocks%>%
  unnest(stock.prices)%>%
  select(Date,name,Close)%>%
  mutate(Close=as.numeric(Close))%>%
  spread(key=name, value=Close)%>%
  select(-Date)


d3heatmap(heatmapdata,scale = "column", 
          show_grid = TRUE, anim_duration = 500,
          distfun = dist, hclustfun = hclust)


library(heatmaply)
library(dendextend)
ggheatmap(
  heatmapdata,
  scale = "column")

############################################

library(officer)
library(openxlsx)
# initialize a workbook 
wb<-createWorkbook()

# add a worksheet 
addWorksheet(wb,sheetName="2022新上市")

writeDataTable(wb,sheet="2022新上市",x=today_best,tableStyle = "TableStyleLight2")

cixin_other$change_type<-ifelse(cixin_other$pct_chg>4,"表现很好",
                                ifelse(cixin_other$pct_chg>1,"表现一般","表现不佳"))

addWorksheet(wb,sheetName="投资风向")
plot_bar(cixin_other,by='change_type')+
  theme(text = element_text(family='Kai'))
# plot needs to be showing
insertPlot(wb,"投资风向", width = 12, height = 15, fileType = "png", units = "in")


addWorksheet(wb,sheetName="2020以前上市")

colnames(cixin_other)<-c("代码","公司名称","行业","产业趋势","产业技术","产品类型",
                         "产业类型","板块","收盘价","涨跌幅","交易量","价格变化","上市时间","地区",
                         "上市年份","上市月份","时间","表现情况")

writeDataTable(wb,sheet="2020以前上市",x=cixin_other,tableStyle = "TableStyleLight2")



addWorksheet(wb,sheetName="走势图")
china_stocks %>%
  unnest(stock.prices)%>%
  arrange(desc(Date))%>%
  ggplot(aes(x=desc(Date),y=as.numeric(Close)),color=name)+
  geom_line()+
  facet_wrap(~name,ncol=4,scales = "free_y")+
  labs(x="日期",y="价格",title="个股走势图")+
  theme_hildaDay(ticks = TRUE,
                 legend.position = "top")

insertPlot(wb,"走势图", width = 8, height = 8, fileType = "png", units = "in")

addWorksheet(wb,sheetName="相关性分析")
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="皮尔逊相关系数分析", 
           ggtheme=theme_hildaDay(ticks = TRUE,
                                  legend.position = "left"))
insertPlot(wb,"相关性分析", width = 8, height = 8, fileType = "png", units = "in")



addWorksheet(wb,sheetName="聚类算法")
ggheatmap(
  heatmapdata,
  scale = "column")

insertPlot(wb,"聚类算法", width = 8, height = 8, fileType = "png", units = "in")

addWorksheet(wb,sheetName="反欺诈算法")
library(purrr)
library(dplyr)
anoma_data<-china_stocks %>%
  unnest(stock.prices)%>%
  select(Date,Volume,name)%>%
  group_by(name) %>%
  tk_anomaly_diagnostics(Date, Volume)%>%
  filter(Date>ymd(today()-3))
colnames(anoma_data)<-c("名称","日期","观察次数","周期性","趋势性","提示信号","季节调整","一级水平","二级水平","异常","一级维度","二级维度")

writeDataTable(wb,sheet="反欺诈算法",x=anoma_data,tableStyle = "TableStyleLight5")

library(anomalize)

addWorksheet(wb,sheetName="异常监测")
library(timetk)

china_stocks %>%
  unnest(stock.prices)%>%
  select(Date,Volume,name)%>%
  filter(name %in% anoma_namedata$公司)%>%
  group_by(name) %>%
  plot_anomaly_diagnostics(Date, Volume,
                           .message = FALSE,
                           .facet_ncol = 3,
                           .ribbon_alpha = 0.25,
                           .interactive = FALSE)


insertPlot(wb,"异常监测", width = 8, height = 8, fileType = "png", units = "in")


addWorksheet(wb,sheetName="2021上市")

writeDataTable(wb,sheet="2021上市",x=cixin_2021,tableStyle = "TableStyleLight2")

addWorksheet(wb,sheetName="2020上市")

writeDataTable(wb,sheet="2020上市",x=cixin_2020,tableStyle = "TableStyleLight2")




saveWorkbook(wb,"//Users//wulixin//Desktop//投资风向标.xlsx",overwrite=TRUE)
openXL("//Users//wulixin//Desktop//投资风向标.xlsx")



#################  数据标准化处理

library(datawizard)
skewness(rnorm(1000))
kurtosis(rnorm(1000))
hardlyworking %>%
  select(starts_with("xtra_hours")) %>%
  pivot_longer(everything()) %>%
  group_by(name) %>%
  summarise(
    mean = mean(value),
    sd = sd(value),
    median = median(value),
    mad = mad(value)
  )

