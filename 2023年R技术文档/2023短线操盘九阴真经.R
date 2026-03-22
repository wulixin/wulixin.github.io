

####################################################################################
#
#                             2023年短线操盘葵花宝典
#
####################################################################################

#########################################################################
#  六大重要方向  大金融，大消费，高科技，新能源，新科技，新基建(数字中国,智慧城市)；
#  房地产产业链，电子商务产业链，手机产业链，汽车产业链，军工产业链；
#能源产业链，粮食产业链，文化产业链；
#  霸王，诸侯王，君王； 鱼肉，鱼头，鱼尾；
#####################################################


{library(showtext)
  showtext_auto(enable=TRUE)
  font_add('Songti','Songti.ttc')
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
  
  library(highcharter)
  library(purrr)
  library(tidyr)
  library(tidyverse)
  library(dplyr)
  library(janitor)
  library(lares)
  library(DT)
  library(stringi)
  library(stringr)
  
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


dataprices = pro(api_name="daily",trade_date='20230221')
stocks_names<-pro(api_name = 'stock_basic')

#write.csv(stocks_names,"//Users//wulixin//Desktop//stocksfull.csv")

##################################按产业地位分类的产业
#第一：基础产业 产业结构体系中为其它产业的发展提供基本条件并为大多数产业提供服务的产业；
#第二：瓶颈产业  产业结构体系中未得到应有发展而已经严重制约其它产业和国民经济发展的产业
#第三：支柱产业   在产业结构体系的总产出占比较大的产业
#第四：主导产业   在产业结构体系中处于主体地位并起着产业发展的引导和支撑作用
#第五：先行产业：  在产业结构体系中因关系到国民经济发展而必须优先发展的产业

unique(stocks_names$industry)



#  四大重要指标  大金融，大消费，高科技，新能源，大健康，数字经济，新老基建，国产替代；
#  房地产产业链，电子商务产业链，智能手机产业链，智能汽车产业链，军工产业链，交通强国;
#  能源产业链，粮食产业链，文化产业链；


#大健康
DJK<-c("医疗保健","中成药","生物制药","化学制药")
# 大金融
DJR<-c("银行","保险","证券","多元金融")
# 大消费
DXF<-c("红黄酒","啤酒","白酒","食品","黄金","乳制品","服饰","家用电器","软饮料","家居用品")
#高新科技
GKJ<-c("元器件","半导体","机床制造","电气设备")
#新能源(材料) 新能源电池
XNY<-c("矿物制品","铅锌","小金属","铜")
#新基建(东数西算,VR,AR,数字中国，智慧城市)
XLJJ<-c("IT设备","电信运营","通信设备")
#国产替代ChatGPT
GCTD<-c("软件服务","互联网")

#########################六大产业链
# 房地产产业链
FDC<-c("全国地产","水泥","染料涂料","装修装饰","房产服务","建筑工程",
       "其他建材")
# 电子商务产业链
DZSW<-c("园区开发","互联网","广告包装","仓储物流")
# 智能汽车产业链
ZNQC<-c("汽车服务","汽车配件","汽车整车","电器仪表","摩托车")
# 军工高铁产业链
JG<-c("船舶","航空","铝","特种钢")
#特高压电力产业链
TGY<-c("电气设备","火力发电","新型电力","水力发电","铜")
# 实体经济产业
STJJ<-c("旅游服务","商贸代理","超市连锁","酒店餐饮","百货","其他商业",
        "医药商业","综合类","商品城","批发业","旅游景点")



#能源安全 煤炭与石油
NY<-c("石油加工","石油贸易","煤炭开采","石油开采","矿物制品","焦炭加工","钢加工","普钢")
#粮食安全
LS<-c("农业综合","渔业","农药化肥","饲料","种植业")
#民生安全
MS<-c("供气供热","环境保护","林业","水务")
#文化体育强国
WH<-c("出版业","影视音像","文教休闲")
#交通强国
JT<-c("公路","公共交通","水运","港口","机场","空运","路桥","铁路")
#化工强国
HG<-c("陶瓷","塑料","橡胶","纺织","化工原料","日用化工","化纤","造纸","玻璃") 
#制造业强国(工业母机)
ZZY<-c("纺织机械","轻工机械","化工机械","专用机械","机床制造","电器仪表","机械基件",
       "工程机械","运输设备","农用机械")




###########################################################################################
#
#                              政策支持方向！
#
###########################################################################################

XianXing<-c("互联网","机床制造","半导体","软件服务","IT设备","元器件","汽车服务",
            "汽车配件","通信设备","新型电力","汽车整车","电气设备","船舶","航空","生物制药","化学制药","仓储物流","医药商业")

stocks_names$CY_FaZhanQS<-ifelse(stocks_names$industry %in% XianXing,"政策支持行业","行业成熟格局稳定")


############################################################################################
#
#                                  技术先进
#
############################################################################################

GaoXinJiShu<-c("机床制造","半导体","IT设备","元器件","通信设备","新型电力","电气设备","工程机械","专用机械","航空","生物制药","化学制药")
stocks_names$CY_JiShuXJ<-ifelse(stocks_names$industry %in% GaoXinJiShu,"高新技术产业","传统行业产业")
head(stocks_names)

##################################################################################################
#                                     产品供求状况
#
###################################################################################################

ChangXian<-c("黄金","服饰","家居用品","房产服务","全国地产","区域地产","家用电器","电器连锁","林业","水务",
             "摩托车","化纤","橡胶","玻璃","汽车整车","电气设备","建筑工程","其他建材","装修装饰","水泥","染料涂料",
             "化工机械","纺织机械","工程机械","农用机械","轻工机械","机械基件","专用机械","化工原料","电器仪表","机床制造","运输设备")

DuanXian<-c("啤酒", "食品","软饮料","乳制品","白酒","红黄酒","饲料","生物制药","医疗保健","医药商业","中成药","化学制药",
            "日用化工","造纸", "塑料","汽车配件","石油加工","石油贸易","农药化肥","农业综合","渔业","种植业","铝",
            "小金属","铜","钢加工","陶瓷","普钢","焦炭加工","矿物制品","特种钢","供气供热","煤炭开采","铅锌","纺织")

stocks_names$CY_ChanPinGQ<-ifelse(stocks_names$industry %in% ChangXian,"长寿命周期产品",
                                  ifelse(stocks_names$industry %in% DuanXian,"短期消费服务","其它服务"))

################################################################################################
#
#                                         产业类型
#
#       占山为王的霸王，划界而治垄断经营的霸王，居奇货号令全国的君王
#
###################################################################################################

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




####################################################七：产业关联方式分类法：
#向前关联效应，向后关联效应，上游产业链，
# 1 技术关联分类法；2原料关联分类法：3用途关联分类法：4方向关联分类法：5战略关联分类法：
## 这个位置将生成 所属产业链以及所属产业链的地位


################################################### 行业集中度 

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

df_all$ShiZhi<-cut(df_all$amount,breaks=quantile(df_all$amount),
                   labels=c("迷你盘","小盘股","中盘股","大盘股"))  
df_all$Time<-as.numeric(today()-ymd(df_all$list_date))




{## 数据获取
  NYstocks<- stocks_names%>%
    filter(industry %in% NY)%>%
    mutate(
      stock.prices = map(ts_code, 
                         function(.x) get_stock_prices(.x)
      ),
      log.returns  = map(stock.prices, 
                         function(.x) get_log_returns(.x)),
      mean.log.returns = map_dbl(log.returns, ~ mean(.$Log.Returns)),
      sd.log.returns   = map_dbl(log.returns, ~ sd(.$Log.Returns)),
      n.trade.days = map_dbl(stock.prices, nrow)
    )%>%as_tibble()%>%
    unnest(stock.prices)%>%group_by(name)%>%
    mutate(return=(Close-Open)/Open)%>%
    ungroup(name)
  
  
  NYstocks$return_type<-cut(NYstocks$return, breaks=c(-0.1,-0.05,-0.015,0,0.025,0.035,0.08,0.12), 
                             labels =c("大跌","中跌","微跌","小涨","中涨","大涨","游资偏好"))
  df_all$GaiNian<-'能源安全'
  
  NYDT<-NYstocks%>%
    filter(Date>ymd(Sys.Date()-300))%>%
    select(name,return_type)%>%
    tabyl(name, return_type)%>%
    select(name,大涨,游资偏好)%>%
    arrange(desc(游资偏好))%>%
    left_join(df_all)%>%
    select(name,symbol,游资偏好,大涨,close,pct_chg,ShiZhi,GaiNian,
           CY_FaZhanQS,CY_JiShuXJ,CY_ChanPinGQ,CY_ChanYeType,area,market,industry,Time)
  
  colnames(NYDT)<-c("公司名称","代码","游资偏好度","爆发力","收盘价","涨跌幅","市值","大方向","产业趋势",
                     "产业技术","产品类型","产业类型","区域","板块","行业","上市时间") }




{## 数据获取
  LSstocks<- stocks_names%>%
    filter(industry %in% LS)%>%
    mutate(
      stock.prices = map(ts_code, 
                         function(.x) get_stock_prices(.x)
      ),
      log.returns  = map(stock.prices, 
                         function(.x) get_log_returns(.x)),
      mean.log.returns = map_dbl(log.returns, ~ mean(.$Log.Returns)),
      sd.log.returns   = map_dbl(log.returns, ~ sd(.$Log.Returns)),
      n.trade.days = map_dbl(stock.prices, nrow)
    )%>%as_tibble()%>%
    unnest(stock.prices)%>%group_by(name)%>%
    mutate(return=(Close-Open)/Open)%>%
    ungroup(name)
  
  
  LSstocks$return_type<-cut(LSstocks$return, breaks=c(-0.1,-0.05,-0.015,0,0.025,0.035,0.08,0.12), 
                            labels =c("大跌","中跌","微跌","小涨","中涨","大涨","游资偏好"))
  df_all$GaiNian<-'粮食安全'
  
  LSDT<- LSstocks%>%
    filter(Date>ymd(Sys.Date()-300))%>%
    select(name,return_type)%>%
    tabyl(name, return_type)%>%
    select(name,大涨,游资偏好)%>%
    arrange(desc(游资偏好))%>%
    left_join(df_all)%>%
    select(name,symbol,游资偏好,大涨,close,pct_chg,ShiZhi,GaiNian,
           CY_FaZhanQS,CY_JiShuXJ,CY_ChanPinGQ,CY_ChanYeType,area,market,industry,Time)
  
  colnames(LSDT)<-c("公司名称","代码","游资偏好度","爆发力","收盘价","涨跌幅","市值","大方向","产业趋势",
                    "产业技术","产品类型","产业类型","区域","板块","行业","上市时间") }


#library(Hmisc)
#library(funModeling)
#plot_nums(DJRDT)
#plot_cats(DJRDT)


{## 数据获取
  MSstocks<- stocks_names%>%
    filter(industry %in% MS)%>%
    mutate(
      stock.prices = map(ts_code, 
                         function(.x) get_stock_prices(.x)
      ),
      log.returns  = map(stock.prices, 
                         function(.x) get_log_returns(.x)),
      mean.log.returns = map_dbl(log.returns, ~ mean(.$Log.Returns)),
      sd.log.returns   = map_dbl(log.returns, ~ sd(.$Log.Returns)),
      n.trade.days = map_dbl(stock.prices, nrow)
    )%>%as_tibble()%>%
    unnest(stock.prices)%>%group_by(name)%>%
    mutate(return=(Close-Open)/Open)%>%
    ungroup(name)
  
  
  MSstocks$return_type<-cut(MSstocks$return, breaks=c(-0.1,-0.05,-0.015,0,0.025,0.035,0.08,0.12), 
                            labels =c("大跌","中跌","微跌","小涨","中涨","大涨","游资偏好"))
  df_all$GaiNian<-'民生安全'
  
  MSDT<- MSstocks%>%
    filter(Date>ymd(Sys.Date()-300))%>%
    select(name,return_type)%>%
    tabyl(name, return_type)%>%
    select(name,大涨,游资偏好)%>%
    arrange(desc(游资偏好))%>%
    left_join(df_all)%>%
    select(name,symbol,游资偏好,大涨,close,pct_chg,ShiZhi,GaiNian,
           CY_FaZhanQS,CY_JiShuXJ,CY_ChanPinGQ,CY_ChanYeType,area,market,industry,Time)
  
  colnames(MSDT)<-c("公司名称","代码","游资偏好度","爆发力","收盘价","涨跌幅","市值","大方向","产业趋势",
                    "产业技术","产品类型","产业类型","区域","板块","行业","上市时间") }


{## 数据获取
  JTstocks<- stocks_names%>%
    filter(industry %in% JT)%>%
    mutate(
      stock.prices = map(ts_code, 
                         function(.x) get_stock_prices(.x)
      ),
      log.returns  = map(stock.prices, 
                         function(.x) get_log_returns(.x)),
      mean.log.returns = map_dbl(log.returns, ~ mean(.$Log.Returns)),
      sd.log.returns   = map_dbl(log.returns, ~ sd(.$Log.Returns)),
      n.trade.days = map_dbl(stock.prices, nrow)
    )%>%as_tibble()%>%
    unnest(stock.prices)%>%group_by(name)%>%
    mutate(return=(Close-Open)/Open)%>%
    ungroup(name)
  
  
  JTstocks$return_type<-cut(JTstocks$return, breaks=c(-0.1,-0.05,-0.015,0,0.025,0.035,0.08,0.12), 
                            labels =c("大跌","中跌","微跌","小涨","中涨","大涨","游资偏好"))
  df_all$GaiNian<-'交通强国'
  
  JTDT<- JTstocks%>%
    filter(Date>ymd(Sys.Date()-300))%>%
    select(name,return_type)%>%
    tabyl(name, return_type)%>%
    select(name,大涨,游资偏好)%>%
    arrange(desc(游资偏好))%>%
    left_join(df_all)%>%
    select(name,symbol,游资偏好,大涨,close,pct_chg,ShiZhi,GaiNian,
           CY_FaZhanQS,CY_JiShuXJ,CY_ChanPinGQ,CY_ChanYeType,area,market,industry,Time)
  
  colnames(JTDT)<-c("公司名称","代码","游资偏好度","爆发力","收盘价","涨跌幅","市值","大方向","产业趋势",
                    "产业技术","产品类型","产业类型","区域","板块","行业","上市时间") }




{## 数据获取
  WHstocks<- stocks_names%>%
    filter(industry %in% WH)%>%
    mutate(
      stock.prices = map(ts_code, 
                         function(.x) get_stock_prices(.x)
      ),
      log.returns  = map(stock.prices, 
                         function(.x) get_log_returns(.x)),
      mean.log.returns = map_dbl(log.returns, ~ mean(.$Log.Returns)),
      sd.log.returns   = map_dbl(log.returns, ~ sd(.$Log.Returns)),
      n.trade.days = map_dbl(stock.prices, nrow)
    )%>%as_tibble()%>%
    unnest(stock.prices)%>%group_by(name)%>%
    mutate(return=(Close-Open)/Open)%>%
    ungroup(name)
  
  
  WHstocks$return_type<-cut(WHstocks$return, breaks=c(-0.1,-0.05,-0.015,0,0.025,0.035,0.08,0.12), 
                            labels =c("大跌","中跌","微跌","小涨","中涨","大涨","游资偏好"))
  df_all$GaiNian<-'文化强国'
  
  WHDT<- WHstocks%>%
    filter(Date>ymd(Sys.Date()-300))%>%
    select(name,return_type)%>%
    tabyl(name, return_type)%>%
    select(name,大涨,游资偏好)%>%
    arrange(desc(游资偏好))%>%
    left_join(df_all)%>%
    select(name,symbol,游资偏好,大涨,close,pct_chg,ShiZhi,GaiNian,
           CY_FaZhanQS,CY_JiShuXJ,CY_ChanPinGQ,CY_ChanYeType,area,market,industry,Time)
  
  colnames(WHDT)<-c("公司名称","代码","游资偏好度","爆发力","收盘价","涨跌幅","市值","大方向","产业趋势",
                    "产业技术","产品类型","产业类型","区域","板块","行业","上市时间") }



{## 数据获取
  HGstocks<- stocks_names%>%
    filter(industry %in% HG)%>%
    mutate(
      stock.prices = map(ts_code, 
                         function(.x) get_stock_prices(.x)
      ),
      log.returns  = map(stock.prices, 
                         function(.x) get_log_returns(.x)),
      mean.log.returns = map_dbl(log.returns, ~ mean(.$Log.Returns)),
      sd.log.returns   = map_dbl(log.returns, ~ sd(.$Log.Returns)),
      n.trade.days = map_dbl(stock.prices, nrow)
    )%>%as_tibble()%>%
    unnest(stock.prices)%>%group_by(name)%>%
    mutate(return=(Close-Open)/Open)%>%
    ungroup(name)
  
  
  HGstocks$return_type<-cut(HGstocks$return, breaks=c(-0.1,-0.05,-0.015,0,0.025,0.035,0.08,0.12), 
                            labels =c("大跌","中跌","微跌","小涨","中涨","大涨","游资偏好"))
  df_all$GaiNian<-'化工强国'
  
  HGDT<- HGstocks%>%
    filter(Date>ymd(Sys.Date()-300))%>%
    select(name,return_type)%>%
    tabyl(name, return_type)%>%
    select(name,大涨,游资偏好)%>%
    arrange(desc(游资偏好))%>%
    left_join(df_all)%>%
    select(name,symbol,游资偏好,大涨,close,pct_chg,ShiZhi,GaiNian,
           CY_FaZhanQS,CY_JiShuXJ,CY_ChanPinGQ,CY_ChanYeType,area,market,industry,Time)
  
  colnames(HGDT)<-c("公司名称","代码","游资偏好度","爆发力","收盘价","涨跌幅","市值","大方向","产业趋势",
                    "产业技术","产品类型","产业类型","区域","板块","行业","上市时间") }

stocks_names$list_date<-ymd(stocks_names$list_date)

{## 数据获取
  ZZYstocks<- stocks_names%>%
    filter(industry %in% ZZY)%>%
    filter(list_date<=ymd('20221101'))%>%
    mutate(
      stock.prices = map(ts_code, 
                         function(.x) get_stock_prices(.x)
      ),
      log.returns  = map(stock.prices, 
                         function(.x) get_log_returns(.x)),
      mean.log.returns = map_dbl(log.returns, ~ mean(.$Log.Returns)),
      sd.log.returns   = map_dbl(log.returns, ~ sd(.$Log.Returns)),
      n.trade.days = map_dbl(stock.prices, nrow)
    )%>%as_tibble()%>%
    unnest(stock.prices)%>%group_by(name)%>%
    mutate(return=(Close-Open)/Open)%>%
    ungroup(name)
  
  ZZYstocks$return_type<-cut(ZZYstocks$return, breaks=c(-0.1,-0.05,-0.015,0,0.025,0.035,0.08,0.12), 
                            labels =c("大跌","中跌","微跌","小涨","中涨","大涨","游资偏好"))
  df_all$GaiNian<-'制造业强国'
  
  ZZYDT<- ZZYstocks%>%
    filter(Date>ymd(Sys.Date()-300))%>%
    select(name,return_type)%>%
    tabyl(name, return_type)%>%
    select(name,大涨,游资偏好)%>%
    arrange(desc(游资偏好))%>%
    left_join(df_all)%>%
    select(name,symbol,游资偏好,大涨,close,pct_chg,ShiZhi,GaiNian,
           CY_FaZhanQS,CY_JiShuXJ,CY_ChanPinGQ,CY_ChanYeType,area,market,industry,Time)
  
  colnames(ZZYDT)<-c("公司名称","代码","游资偏好度","爆发力","收盘价","涨跌幅","市值","大方向","产业趋势",
                    "产业技术","产品类型","产业类型","区域","板块","行业","上市时间") }



{library(officer)
  library(openxlsx)
  # initialize a workbook 
  wb<-createWorkbook()
  
  #国家安全与强国战略 
  addWorksheet(wb,sheetName="能源安全")
  addWorksheet(wb,sheetName="粮食安全")
  addWorksheet(wb,sheetName="民生安全")
  
  addWorksheet(wb,sheetName="交通强国")
  addWorksheet(wb,sheetName="文化强国")
  addWorksheet(wb,sheetName="化工强国")
  addWorksheet(wb,sheetName="制造业强国")
  
  #insertImage(wb, "小美女","//Users//wulixin//Desktop//kehu.png", startRow = 10, startCol = 5, width = 6, height = 5)
  
  # 国家安全与强国战略
  writeDataTable(wb,sheet="能源安全",x=NYDT,tableStyle ="TableStyleMedium7",withFilter = openxlsx_getOp("withFilter", TRUE))
  writeDataTable(wb,sheet="粮食安全",x=LSDT,tableStyle ="TableStyleMedium2",withFilter = openxlsx_getOp("withFilter", TRUE))
  writeDataTable(wb,sheet="民生安全",x=MSDT,tableStyle ="TableStyleLight4",withFilter = openxlsx_getOp("withFilter", TRUE))
  writeDataTable(wb,sheet="交通强国",x=JTDT,tableStyle = "TableStyleLight2",withFilter = openxlsx_getOp("withFilter", TRUE))
  writeDataTable(wb,sheet="文化强国",x=WHDT,tableStyle ="TableStyleMedium9",withFilter = openxlsx_getOp("withFilter", TRUE))
  writeDataTable(wb,sheet="化工强国",x=HGDT,tableStyle ="TableStyleMedium10",withFilter = openxlsx_getOp("withFilter", TRUE))
  writeDataTable(wb,sheet="制造业强国",x=ZZYDT,tableStyle ="TableStyleMedium4",withFilter = openxlsx_getOp("withFilter", TRUE))
  
  saveWorkbook(wb,"//Users//wulixin//Desktop//短线操盘九阴真经之国家安全与战略.xlsx",overwrite=TRUE)
  openXL("//Users//wulixin//Desktop//短线操盘九阴真经之国家安全与战略.xlsx")}



