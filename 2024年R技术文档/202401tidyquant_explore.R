

###################################################################################
#
#
#           月线，年线级别的大投资主线
#
####################################################################################
library(tidyquant)
library(tidyr)
library(tidyselect)
library(tidyverse)
library(purrr)
library(dplyr)
library(dbplyr)
library(dtplyr)

##################根据年线为板块设计最近连续两三年跑赢指数的股票，正回归的票
FANG_annual_returns <- FANG %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "yearly", 
               type       = "arithmetic")
FANG_annual_returns

######可视化每年的跑赢指数情况

FANG_annual_returns %>%
  ggplot(aes(x = date, y = yearly.returns, fill = symbol)) +
  geom_col() +
  geom_hline(yintercept = 0, color = palette_light()[[1]]) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "FANG: Annual Returns",
       subtitle = "Get annual returns quickly with tq_transmute!",
       y = "Annual Returns", x = "") + 
  facet_wrap(~ symbol, ncol = 2, scales = "free_y") +
  theme_tq() + 
  scale_fill_tq()

###### 周线级别连续跑赢指数的股票

FANG_daily_log_returns <- FANG %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "daily", 
               type       = "log",
               col_rename = "monthly.returns")

FANG_daily_log_returns %>%
  ggplot(aes(x = monthly.returns, fill = symbol)) +
  geom_density(alpha = 0.5) +
  labs(title = "FANG: Charting the Daily Log Returns",
       x = "Monthly Returns", y = "Density") +
  theme_tq() +
  scale_fill_tq() + 
  facet_wrap(~ symbol, ncol = 2)

#########  月线级别的行情与机会

FANG %>%
  group_by(symbol) %>%
  tq_transmute(select     = open:volume, 
               mutate_fun = to.period, 
               period     = "months")

########  走势图可视化

FANG_daily <- FANG %>%
  group_by(symbol)

FANG_daily %>%
  ggplot(aes(x = date, y = adjusted, color = symbol)) +
  geom_line(size = 1) +
  labs(title = "Daily Stock Prices",
       x = "", y = "Adjusted Prices", color = "") +
  facet_wrap(~ symbol, ncol = 2, scales = "free_y") +
  scale_y_continuous(labels = scales::dollar) +
  theme_tq() + 
  scale_color_tq()

#######With Monthly Periodicity Aggregation

FANG_monthly <- FANG %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = to.period, 
               period     = "months")

FANG_monthly %>%
  ggplot(aes(x = date, y = adjusted, color = symbol)) +
  geom_line(size = 1) +
  labs(title = "Monthly Stock Prices",
       x = "", y = "Adjusted Prices", color = "") +
  facet_wrap(~ symbol, ncol = 2, scales = "free_y") +
  scale_y_continuous(labels = scales::dollar) +
  theme_tq() + 
  scale_color_tq()

library(TTR)

# Asset Returns
FANG_returns_monthly <- FANG %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn,
               period     = "monthly")

# Baseline Returns     这个位置可以设置为某个指数
baseline_returns_monthly <- "XLK" %>%
  tq_get(get  = "stock.prices",
         from = "2013-01-01", 
         to   = "2016-12-31") %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn,
               period     = "monthly")

returns_joined <- left_join(FANG_returns_monthly, 
                            baseline_returns_monthly,
                            by = "date")
returns_joined

###################与某个股票的相关系数，与特定指数的相关系数

FANG_rolling_corr <- returns_joined %>%
  tq_transmute_xy(x          = monthly.returns.x, 
                  y          = monthly.returns.y,
                  mutate_fun = runCor,
                  n          = 6,
                  col_rename = "rolling.corr.6")


FANG_rolling_corr %>%
  ggplot(aes(x = date, y = rolling.corr.6, color = symbol)) +
  geom_hline(yintercept = 0, color = palette_light()[[1]]) +
  geom_line(size = 1) +
  labs(title = "FANG: Six Month Rolling Correlation to XLK",
       x = "", y = "Correlation", color = "") +
  facet_wrap(~ symbol, ncol = 2) +
  theme_tq() + 
  scale_color_tq()



####Example 4: Use TTR MACD to Visualize Moving Average Convergence Divergence


FANG_macd <- FANG %>%
  group_by(symbol) %>%
  tq_mutate(select     = close, 
            mutate_fun = MACD, 
            nFast      = 12, 
            nSlow      = 26, 
            nSig       = 9, 
            maType     = SMA) %>%
  mutate(diff = macd - signal) %>%
  select(-(open:volume))

FANG_macd


FANG_macd %>%
  filter(date >= as_date("2016-10-01")) %>%
  ggplot(aes(x = date)) + 
  geom_hline(yintercept = 0, color = palette_light()[[1]]) +
  geom_line(aes(y = macd, col = symbol)) +
  geom_line(aes(y = signal), color = "blue", linetype = 2) +
  geom_bar(aes(y = diff), stat = "identity", color = palette_light()[[1]]) +
  facet_wrap(~ symbol, ncol = 2, scale = "free_y") +
  labs(title = "FANG: Moving Average Convergence Divergence",
       y = "MACD", x = "", color = "") +
  theme_tq() +
  scale_color_tq()

#######Example 5: Use xts apply.quarterly to Get the Max and Min Price for Each Quarter
############每个季度的最高股价与最低股价


FANG_max_by_qtr <- FANG %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = apply.quarterly, 
               FUN        = max, 
               col_rename = "max.close") %>%
  mutate(year.qtr = paste0(year(date), "-Q", quarter(date))) %>%
  select(-date)
FANG_max_by_qtr

FANG_min_by_qtr <- FANG %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = apply.quarterly, 
               FUN        = min, 
               col_rename = "min.close") %>%
  mutate(year.qtr = paste0(year(date), "-Q", quarter(date))) %>%
  select(-date)

FANG_by_qtr <- left_join(FANG_max_by_qtr, FANG_min_by_qtr,
                         by = c("symbol"   = "symbol",
                                "year.qtr" = "year.qtr"))
FANG_by_qtr

###############数据可视化

FANG_by_qtr %>%
  ggplot(aes(x = year.qtr, color = symbol)) +
  geom_segment(aes(xend = year.qtr, y = min.close, yend = max.close),
               size = 1) +
  geom_point(aes(y = max.close), size = 2) +
  geom_point(aes(y = min.close), size = 2) +
  facet_wrap(~ symbol, ncol = 2, scale = "free_y") +
  labs(title = "FANG: Min/Max Price By Quarter",
       y = "Stock Price", color = "") +
  theme_tq() +
  scale_color_tq() +
  scale_y_continuous(labels = scales::dollar) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.x = element_blank())

#############Example 6: Use zoo rollapply to visualize a rolling regression

###Annual returns 

FANG_returns_yearly %>%
  ggplot(aes(x = year(date), y = yearly.returns, fill = symbol)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = "FANG: Annual Returns", 
       subtitle = "Mutating at scale is quick and easy!",
       y = "Returns", x = "", color = "") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  theme_tq() +
  scale_fill_tq()

##############可以筛选出年线return大于0的股票 

AAPL <- tq_get("AAPL", from = "2007-01-01", to = "2016-12-31")
AAPL

get_annual_returns <- function(stock.returns) {
  stock.returns %>%
    tq_transmute(select     = adjusted, 
                 mutate_fun = periodReturn, 
                 type       = "log", 
                 period     = "yearly")
}

AAPL_annual_log_returns <- get_annual_returns(AAPL)
AAPL_annual_log_returns

#### 年线级别的趋势

AAPL_annual_log_returns %>%
  ggplot(aes(x = year(date), y = yearly.returns)) + 
  geom_hline(yintercept = 0, color = palette_light()[[1]]) +
  geom_point(size = 2, color = palette_light()[[3]]) +
  geom_line(size = 1, color = palette_light()[[3]]) + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "AAPL: Visualizing Trends in Annual Returns",
       x = "", y = "Annual Returns", color = "") +
  theme_tq()

library(broom)
tidy(mod)

get_model <- function(stock_data) {
  annual_returns <- get_annual_returns(stock_data)
  mod <- lm(yearly.returns ~ year(date), data = annual_returns)
  tidy(mod)
}

get_model(AAPL)

############################Scale to Many Stocks

set.seed(10)
stocks_tbl <- tq_index("SP500") %>%
  sample_n(5) 
stocks_tbl

stocks_model_stats <- stocks_tbl %>%
  select(symbol, company) %>%
  tq_get(from = "2007-01-01", to = "2016-12-31") %>%
  
  # Nest 
  group_by(symbol, company) %>%
  nest() %>%
  
  # Apply the get_model() function to the new "nested" data column
  mutate(model = map(data, get_model)) %>%
  
  # Unnest and collect slope
  unnest(model) %>%
  filter(term == "year(date)") %>%
  arrange(desc(estimate)) %>%
  select(-term)

stocks_model_stats

#####获取组合数据

c("AAPL", "GOOG", "BAD APPLE") %>%
  tq_get(get = "stock.prices", complete_cases = FALSE)




FANG %>%
  pivot_table(
    .columns = symbol,
    .values  = ~ SUM(is.na(adjusted))
  ) %>%
  kable()

FANG %>%
  pivot_table(
    .rows    = c(symbol, ~ QUARTER(date)),
    .columns = ~ YEAR(date),
    .values  = ~ (LAST(adjusted) - FIRST(adjusted)) / FIRST(adjusted)
  ) %>%
  kable()

FANG %>%
  pivot_table(
    .rows    = symbol,
    .columns = ~ YEAR(date),
    .values  = ~ PCT_CHANGE_FIRSTLAST(adjusted)
  ) %>% 
  kable()

lookup_table <- tibble(
  stock   = c("FB", "AMZN", "NFLX", "GOOG"),
  company = c("Facebook", "Amazon", "Netflix", "Google")
)

lookup_table %>% kable()

##############最新获取美股数据的方式
tiingo_api_key('7b057f93b4a1fe08834b0c68f48890f43858b8bb')

tq_get(c("AAPL", "GOOG"),
       get    = "tiingo.iex",
       from   = "2023-01-01",
       to     = "2024-01-12",
       resample_frequency = "5min")

FANG %>%
  group_by(symbol) %>%
  tq_mutate_xy(x = close, y = volume, 
               mutate_fun = EVWMA, col_rename = "EVWMA")

wti_prices <- tq_get("DCOILWTICO", get = "economic.data") 

wti_prices %>%    
  tq_transmute(mutate_fun = to.period,
               period     = "months", 
               col_rename = "WTI Price")


library(PortfolioAnalytics)
library(PerformanceAnalytics)
library(quantmod)
library(purrr)

latest_week<-Sys.Date()-days(15)
latest_day<-Sys.Date()-days(1)
latest_week<-Sys.Date()-days(15)
latest_month<-Sys.Date()-days(30)
start_date<-Sys.Date()-days(550)

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



periodReturn(QQQQ,period='yearly',subset='2003::')  # returns years 2003 to present
periodReturn(QQQQ,period='yearly',subset='2003')  # returns year 2003



periodReturn(x,
             period='monthly',
             subset=NULL,
             type='arithmetic',
             leading=TRUE,
             ...)

dailyReturn(x, subset=NULL, type='arithmetic',
            leading=TRUE, ...)
weeklyReturn(x, subset=NULL, type='arithmetic',
             leading=TRUE, ...)
monthlyReturn(x, subset=NULL, type='arithmetic',
              leading=TRUE, ...)
quarterlyReturn(x, subset=NULL, type='arithmetic',
                leading=TRUE, ...)
annualReturn(x, subset=NULL, type='arithmetic',
             leading=TRUE, ...)
yearlyReturn(x, subset=NULL, type='arithmetic',
             leading=TRUE, ...)
allReturns(x, subset=NULL, type='arithmetic',
           leading=TRUE)


