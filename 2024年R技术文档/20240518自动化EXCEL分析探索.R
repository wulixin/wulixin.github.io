

library(TSstudio)
library(modeltime)
library(modelStudio)
library(ModelMetrics)

library(DataExplorer)
head(Coffee_Prices)
ts_plot(Coffee_Prices,
        type = "multiple")

library(xlcharts)
library(openxlsx)
library(readxl)
library(writexl)
# R TIPS ----
# TIP 005: Automate Excel Workbooks with R ----
#
# 👉 For Weekly R-Tips, Sign Up Here: https://learn.business-science.io/r-tips-newsletter

# 1.0 LIBRARIES ----

library(openxlsx)
library(tidyquant)
library(tidyverse)
library(timetk)
library(Quandl)
Quandl.api_key("5tLBvJCNq1-k9UmG5x22")

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
start_date<-Sys.Date()-days(365)

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
###R语言获取数据的办法
stock_data_tbl <- c("000001.SZ","000008.SZ", "000999.SZ") %>%
      pro(api_name = 'daily',start_date= "2023-01-01")


stock_data_tbl1<-stock_data_tbl%>%inner_join(stocks_names)

head(stock_data_tbl1)

library(lubridate)
stock_pivot_table <- stock_data_tbl1 %>%
  pivot_table(
    .rows    = ~ YEAR(ymd(trade_date)),
    .columns = ~ name,
    .values  = ~ PCT_CHANGE_FIRSTLAST(close)) 


stock_plot <- stock_data_tbl1 %>%
  group_by(name) %>%
  plot_time_series(trade_date,close, .facet_ncol = 1, .interactive = FALSE)

# 3.0 CREATE WORKBOOK ----

# * Initialize a workbook ----
wb <- createWorkbook()

# * Add a Worksheet ----
addWorksheet(wb, sheetName = "stock_analysis")

# * Add Plot ----

print(stock_plot)

wb %>% insertPlot(sheet = "stock_analysis", startCol = "G", startRow = 3)

# * Add Data ----

writeDataTable(wb, sheet = "stock_analysis", x = stock_pivot_table)

# * Save Workbook ----
saveWorkbook(wb, "//Users//wulixin//Desktop//stock_analysis.xlsx", overwrite = TRUE)

# * Open the Workbook ----
openXL("//Users//wulixin//Desktop//stock_analysis.xlsx")

# LEARNING MORE ----

# FREE MASTERCLASS
# - 10 SECRETS TO BECOMING A DATA SCIENTIST
#   https://learn.business-science.io/free-rtrack-masterclass
