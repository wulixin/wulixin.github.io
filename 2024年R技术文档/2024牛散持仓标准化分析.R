
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
library(DT)
library(data.table)

stocks_names<-pro(api_name = 'stock_basic')

head(stocks_names)

latest_week<-Sys.Date()-days(15)
latest_day<-Sys.Date()-days(1)
latest_week<-Sys.Date()-days(15)
latest_month<-Sys.Date()-days(30)
start_date<-Sys.Date()-days(500)

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

######选择牛散特定持仓
stocksnames<-c("新兴铸管","粤传媒","广电电气","中远海发","神州高铁","深康佳A","海南海药","中山公用",
               "大东方","航天动力","益佰制药", "渤海汽车","宝胜股份","冰山冷热","天虹股份","京威股份")


#####平民女妆消费股
stocksnames<-c("两面针","朗姿股份","诺邦股份","水羊股份","福瑞达","华润三九","方盛制药","润本股份","丸美股份","青岛金王",
               "拉芳家化", "双鹭药业","汇洁股份","爱慕股份","贝泰妮","哈三联","百合股份","豫园股份","丽人丽妆","佳云科技",
               "华熙生物","安琪酵母","益盛药业","国药现代","康贝恩","联环药业","嘉亨家化","华仁药业","德展健康","万事利",
               "新瀚新材","上海家化","仁和药业","壹网壹创","青松股份","敷尔佳","锦盛新材","翔港科技","嘉必优","片仔癀",
               "名臣健康","珀莱雅","冠昊生物","大湖股份","常山药业","百洋股份","我武生物","力合科创","南极电商","科思股份")



stocks_name<-stocks_names%>%
  filter(name %in% stocksnames)


#####获取组合数据

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


china_stocksdata<-china_stocks%>%
  unnest(stock.prices)%>%
  mutate(Time=as.numeric(today()-ymd(list_date)))

china_stocksdata$Adjusted

library(TTR)
library(tidyquant)
library(ggcorrplot)
library(corrplot)
library(showtext)
showtext_auto(enable=TRUE)
font_add('Songti','Songti.ttc')
font_families()


Stocks_returns_yearly <- china_stocksdata %>%
  group_by(name) %>%
  tq_transmute(select     = Adjusted, 
               mutate_fun = periodReturn, 
               period     = "yearly", 
               col_rename = "yearly.returns") 
####################
stocks_name<-stocks_names%>%
  filter(area=="吉林")%>%
  filter(industry %in% c("生物制药","中成药","医药商业","化学制药","电气设备","汽车整车","汽车配件","软件服务","专用机械","电器仪表","铝"))
#####################

colnames(Stocks_returns_yearly)<-c("公司名称","Date","yearly.returns")
Stocks_returns_yearly %>%
  ggplot(aes(x = year(Date), y = yearly.returns, fill = 公司名称)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = "牛散持仓：每年回报情况", 
       subtitle = "投资有风险,首战即决战,一战定乾坤——LIXIN WU",
       y = "投资回报", x = "年份", color = "") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  theme_tq() +
  scale_fill_tq()

quantile(Stocks_returns_yearly$yearly.returns)



Yearly_1<-Stocks_returns_yearly%>%
  mutate(Date=as.factor(Date))%>%
  group_by(Date)%>%
  filter(yearly.returns>0)     


unique(Yearly_1$公司名称)

str(Stocks_returns_yearly)

####transmute quantitative data ,tq_transmute 月价格

china_stocksdata %>%
  group_by(name) %>%
  tq_transmute(select = Adjusted, mutate_fun = to.monthly, indexAt = "lastof")

###Working with non-OHLC data
#china_stocksdata %>%
  group_by(name) %>%
  tq_transmute(mutate_fun = to.period,
             period     = "months", 
             col_rename = "月均价")

#########Mutate Quantitative Data, tq_mutate
china_stocksdata %>%  
  group_by(symbol) %>%
    tq_mutate(select     = Close, 
              mutate_fun = MACD, 
              col_rename = c("MACD", "Signal"))%>%
    select(name,MACD,Signal)

#######Mutate rolling regressions with rollapply
fb_returns <- tq_get("META", get  = "stock.prices", from = "2016-01-01", to   = "2016-12-31") %>%
  tq_transmute(adjusted, periodReturn, period = "weekly", col_rename = "fb.returns")


####_xy Variants, tq_mutate_xy and tq_transmute_xy 收盘价与量能的关系
china_stocksdata%>%group_by(symbol) %>%
  tq_mutate_xy(x = close, y = volume, 
               mutate_fun = EVWMA, col_rename = "EVWMA")


Stocks_returns_yearly <- china_stocksdata %>%
  group_by(name) %>%
  tq_transmute(select     = Adjusted, 
               mutate_fun = periodReturn, 
               period     = "yearly", 
               col_rename = "yearly.returns") 


colnames(Stocks_returns_yearly)<-c("公司名称","Date","yearly.returns")
Stocks_returns_yearly %>%
  ggplot(aes(x = year(Date), y = yearly.returns, fill = 公司名称)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = "牛散持仓：每年回报情况", 
       subtitle = "投资有风险,首战即决战,一战定乾坤——LIXIN WU",
       y = "投资回报", x = "年份", color = "") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  theme_tq() +
  scale_fill_tq()

Stocks_returns_yearly

Stocks_returns_yearly %>%
  ggplot(aes(x = Date, y = yearly.returns, fill = 公司名称)) +
  geom_col() +
  geom_hline(yintercept = 0, color = palette_light()[[1]]) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "牛散持仓: 每年回报情况",
       subtitle = "任何投资决策都要以年为单位——LIXIN WU",
       y = "每年回报情况", x = "") + 
  facet_wrap(~ 公司名称, ncol =4, scales = "free_y") +
  theme_tq() + 
  scale_fill_tq()


Stocks_daily_log_returns<-china_stocksdata%>%
  group_by(name) %>%
  tq_transmute(select     = Adjusted, 
               mutate_fun = periodReturn, 
               period     = "daily", 
               type       = "log",
               col_rename = "monthly.returns")

colnames(Stocks_daily_log_returns)<-c("公司名称","Date","monthly.returns")
Stocks_daily_log_returns%>%
  ggplot(aes(x = monthly.returns, fill = 公司名称)) +
  geom_density(alpha = 0.5) +
  labs(title = "牛散尺寸: Log波动分布",
       subtitle="首战即决战,一战定乾坤----LIXIN WU",
       x = "月回报率", y = "密度") +
  theme_tq() +
  scale_fill_tq() + 
  facet_wrap(~ 公司名称, ncol = 4)

###############走势图
Stocks_daily <- china_stocksdata %>%
  group_by(name)

stocks_name<-stocks_names%>%
  filter(area=="吉林")%>%
  filter(industry %in% c("生物制药","中成药","医药商业","化学制药","电气设备","汽车整车","汽车配件","软件服务","专用机械","电器仪表","铝"))
###

  #filter(name %in% c("拉芳家化","哈三联","水羊股份","万事利","贝泰妮","片仔癀","华熙生物","冠昊生物","润本股份"))%>%
  #filter(name %in% c('华仁药业','两面针',"德展健康","青岛金王","佳云科技","青松股份","大湖股份","百洋股份","仁和药业"))%>%
Stocks_daily %>%  
  filter(industry %in% c("电气设备","汽车整车","汽车配件","软件服务","专用机械","电器仪表","铝"))%>%
  ggplot(aes(x = desc(Date), y = Adjusted, color = name)) +
  geom_line(size = 1) +
  labs(title = "新能源智能汽车产业：走势图",
       x = "", y = "收盘价", color = "") +
  facet_wrap(~ name, ncol = 4, scales = "free_y") +
  #scale_y_continuous(labels = scales::dollar) +
  theme_tq() + 
  scale_color_tq()

#########跟茅台酒走势参考

# Asset Returns
Stocks_returns_monthly <- china_stocksdata %>%
  group_by(name) %>%
  tq_transmute(select     = Adjusted, 
               mutate_fun = periodReturn,
               period     = "monthly")

# Baseline Returns 贵州茅台作为参考 A股定海神针

#baseline_returns_monthly <- 
  pro(api_name = 'daily', ts_code='002407.SZ',start_date=start_date)%>%
  mutate(open=as.numeric(as.character(open)),
         high=as.numeric(as.character(high)),
         close=as.numeric(as.character(close)),
         low=as.numeric(as.character(low)),
         vol=as.numeric(as.character(vol)),
         Adjusted=as.numeric(as.character(pre_close)),
         Date=ymd(trade_date))%>%
  tq_transmute(select     = Adjusted, 
               mutate_fun = periodReturn,
               period     = "monthly")
#baseline_returns_monthly$name<-"贵州茅台"

GZMT <- stocks_names %>%filter(name=="贵州茅台")%>%
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

####两种数据获取方式，数据格式

GZMTdata<-GZMT%>%
  unnest(stock.prices)%>%
  mutate(Time=as.numeric(today()-ymd(list_date)))

baseline_returns_monthly <- GZMTdata%>%
  tq_transmute(select     = Adjusted, 
               mutate_fun = periodReturn,
               period     = "monthly")

# Asset Returns
Stocks_returns_monthly <- china_stocksdata %>%
  group_by(name) %>%
  tq_transmute(select     = Adjusted, 
               mutate_fun = periodReturn,
               period     = "monthly")

Stocks_returns_monthly

baseline_returns_monthly

returns_joined <- left_join(Stocks_returns_monthly, 
                            baseline_returns_monthly,
                            by = "Date")

#####缺失值处理
returns_joined <- returns_joined %>% 
  mutate(monthly.returns.x= ifelse(is.na(monthly.returns.x), median(monthly.returns.x, na.rm = TRUE),monthly.returns.x),
         monthly.returns.y= ifelse(is.na(monthly.returns.y), median(monthly.returns.y, na.rm = TRUE),monthly.returns.y))



####成功做出了替代方案Rolling Correlations of Returns
Stocks_rolling_corr <-returns_joined%>%group_by(name)%>%
  mutate(rolling.corr.6=runCor(monthly.returns.x,monthly.returns.y,n=6))


Stocks_rolling_corr <- as.data.frame(returns_joined) %>%
  tq_transmute_xy(x= monthly.returns.x, 
                  y= monthly.returns.y,
                  mutate_fun=runCor,
                  n= 6,
                  col_rename = "rolling.corr.6")
##获取沪深指数
df = pro(api="index_daily",ts_code='399300.SZ', start_date=start_date)


library(lares)


Stocks_rolling_corr %>%
  ggplot(aes(x = Date, y = rolling.corr.6, color = name)) +
  geom_hline(yintercept = 0, color = palette_light()[[1]]) +
  geom_line(size = 1) +
  labs(title = "牛散组合: 与A股定海神针茅台走势的相关关系",
       subtitle="A股定海神针：贵州茅台",
       x = "", y = "相关系数", color = "") +
  facet_wrap(~ name, ncol = 4) +
  theme_tq() + 
  scale_color_tq()



########Example 4: Use TTR MACD to Visualize Moving Average Convergence Divergence

Stocks_macd <- china_stocksdata %>%
  group_by(name) %>%
  tq_mutate(select     = Close, 
            mutate_fun = MACD, 
            nFast      = 12, 
            nSlow      = 26, 
            nSig       = 9, 
            maType     = SMA) %>%
  mutate(diff = macd - signal) %>%
  select(-(Open:Volume))

Stocks_macd %>%
  filter(Date >= as_date("2023-08-01")) %>%
  ggplot(aes(x = Date)) + 
  geom_hline(yintercept = 0, color = palette_light()[[1]]) +
  geom_line(aes(y = macd, col = name)) +
  geom_line(aes(y = signal), color = "blue", linetype = 2) +
  geom_bar(aes(y = diff), stat = "identity", color = palette_light()[[1]]) +
  facet_wrap(~ name, ncol = 4, scale = "free_y") +
  labs(title = "牛散组合: MACD指标Moving Average Convergence Divergence",
       y = "MACD", x = "", color = "") +
  theme_tq() +
  scale_color_tq()


#Example 5: Use xts apply.quarterly to Get the Max and Min Price for Each Quarter


Stocks_max_by_qtr <- china_stocksdata %>%
  group_by(name) %>%
  tq_transmute(select     = Adjusted, 
               mutate_fun = apply.quarterly, 
               FUN        = max, 
               col_rename = "max.close") %>%
  mutate(year.qtr = paste0(year(Date), "-Q", quarter(Date))) %>%
  select(-Date)
Stocks_max_by_qtr


Stocks_min_by_qtr <- china_stocksdata %>%
  group_by(name) %>%
  tq_transmute(select     = Adjusted, 
               mutate_fun = apply.quarterly, 
               FUN        = min, 
               col_rename = "min.close") %>%
  mutate(year.qtr = paste0(year(Date), "-Q", quarter(Date))) %>%
  select(-Date)

Stocks_by_qtr <- left_join(Stocks_max_by_qtr, Stocks_min_by_qtr,
                         by = c("name"   = "name",
                                "year.qtr" = "year.qtr"))
Stocks_by_qtr


Stocks_by_qtr %>%
  ggplot(aes(x = year.qtr, color = name)) +
  geom_segment(aes(xend = year.qtr, y = min.close, yend = max.close),
               size = 1) +
  geom_point(aes(y = max.close), size = 2) +
  geom_point(aes(y = min.close), size = 2) +
  facet_wrap(~ name, ncol = 4, scale = "free_y") +
  labs(title = "牛散组合: 季度最高价与最低价",
       y = "股价", color = "") +
  theme_tq() +
  scale_color_tq() +
  scale_y_continuous(labels = scales::dollar) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        axis.title.x = element_blank())

###Use Return.clean
china_stocksdata %>%
  group_by(name) %>%
  tq_transmute(adjusted, periodReturn, period = "daily") %>%
  tq_transmute(daily.returns, Return.clean, alpha = 0.05) %>%
  tq_transmute(daily.returns, Return.excess, Rf = 0.03 / 252)


GZMTdata%>%
  ggplot(aes(x = Date, y = Adjusted)) +
  geom_line(color = palette_light()[[1]]) + 
  scale_y_continuous() +
  labs(title = "贵州茅台股票走势", 
       subtitle = "LIXIN WU ", 
       y = "收盘价", x = "") + 
  theme_tq()


GZMTdata%>%
  ggplot(aes(x = Date, y = Adjusted)) +
  geom_line(color = palette_light()[[1]]) + 
  scale_y_log10() +
  labs(title = "贵州茅台股票LOG走势", 
       subtitle = "LIXIN WU ", 
       y = "收盘价", x = "") + 
  theme_tq()

GZMTdata%>%
  ggplot(aes(x = Date, y = Adjusted)) +
  geom_line(color = palette_light()[[1]]) + 
  geom_smooth(method = "lm") +
  labs(title = "贵州茅台股票走势", 
       subtitle = "LIXIN WU Applying Linear Trendline ", 
       y = "收盘价", x = "") + 
  theme_tq()

start <- end - weeks(24)

GZMTdata %>%
  filter(date >= start - days(50)) %>%
  ggplot(aes(x = Date, y = Volume)) +
  geom_segment(aes(xend = Date, yend = 0, color = Volume)) + 
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "贵州茅台交易量图", 
       subtitle = "每日成交量", 
       y = "交易量", x = "") +
  theme_tq() +
  theme(legend.position = "none") 



