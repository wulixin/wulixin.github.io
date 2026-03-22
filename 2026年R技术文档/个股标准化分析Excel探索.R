

today<-ymd(Sys.Date())
pro <-pro_api(token ='fe8102bf83f5f83f6608aa46fa5e985c534c227786236a1192e5fd55')

###获取数据
dat<-pro(api_name = 'daily', ts_code='000593.SZ',start_date= today-years(2))


#####
stock<-dat%>%
  mutate(open=as.numeric(as.character(open)),
         high=as.numeric(as.character(high)),
         close=as.numeric(as.character(close)),
         low=as.numeric(as.character(low)),
         vol=as.numeric(as.character(vol)),
         pre_close=as.numeric(as.character(pre_close)))
colnames(stock)<-c("ts_code","date","open","high","low","close","pre_close","change","pct_change","volume","amount")


##数据探索

library(DataExplorer)
plot_intro(stock)

##缺失值
plot_missing(stock)
## 如果缺失的处理方法
#inal_data <- drop_columns(final_data, "speed")

##变量的分布
#@@离散型变量分布
##plot_bar(stock)
##plot_bar(final_data, with = "arr_delay")
##plot_bar(final_data, by = "origin") 这个等到标签体系建立后可以用


## 离散型变量处理 

plot_histogram(stock)
#final_data <- update_columns(final_data, "flight", as.factor)

##QQplot 

qq_data <- stock[, c("pct_change", "volume", "amount")]

#log_qq_data <- update_columns(qq_data, 0:2, function(x) log(x + 1))

plot_qq(qq_data, sampled_rows = 1000L)


library(recipes)
## 相关系数  一个股票的数据用这个没有意思 

plot_correlation(na.omit(stock), maxcat = 5L)


# 主成分分析

pca_df <- na.omit(stock[-1,])
plot_prcomp(pca_df, variance_cap = 0.9, nrow = 2L, ncol = 2L)



## 连续变量分布
# 一个连续变量与其它连续变量之间的关系
# Reduce data size for demo purpose
arr_delay_df <- stock[, c("amount", "pct_change",  "volume")]

## Call boxplot function
plot_boxplot(arr_delay_df,by="pct_change")

###把时间周期缩短到最近半年如果都是大于O那么是一直上涨的

plot_scatterplot(arr_delay_df, by = "pct_change", sampled_rows = 1000L)

library(TSstudio)
stock%>%
  mutate(date=ymd(date))%>%
  select(date,close)%>%
  ts_plot(title="个股走势图",Xtitle="时间",Ytitle="价格",Xgrid=TRUE,Ygrid=TRUE,
          slider=TRUE,color="pink",width=2,line.mode="lines+markers")

stock_ts<-stock%>%
  mutate(date=ymd(date))


###画出多个股票走势

ts_plot(Coffee_Prices,
        type = "multiple")

dataprices = pro(api_name="daily",trade_date='20220527')


wb <- createWorkbook()


prices <- read.csv(url(csv.url), as.is = TRUE)
prices$Date <- as.Date(prices$Date)
close <- prices$Close
prices$logReturns = c(0, log(close[2:length(close)]/close[1:(length(close) - 1)]))

## Create plot of price series and add to worksheet
ggplot(data = prices, aes(as.Date(Date), as.numeric(Close))) + geom_line(colour = "royalblue2") +
  labs(x = "Date", y = "Price", title = ticker) + geom_area(fill = "royalblue1",
                                                            alpha = 0.3) + coord_cartesian(ylim = c(min(prices$Close) - 1.5, max(prices$Close) +
                                                                                                      1.5))

## Add worksheet and write plot to sheet
addWorksheet(wb, sheetName = "CBA")
insertPlot(wb, sheet = 1, xy = c("J", 3))

## Histogram of log returns
ggplot(data = prices, aes(x = logReturns)) + geom_bar(binwidth = 0.0025) + labs(title = "Histogram of log returns")

## currency
class(prices$Close) <- "currency"  ## styles as currency in workbook

## write historical data and histogram of returns
writeDataTable(wb, sheet = "CBA", x = prices)
insertPlot(wb, sheet = 1, startRow = 25, startCol = "J")


## Add conditional formatting to show where logReturn > 0.01 using default
## style
conditionalFormat(wb, sheet = 1, cols = 1:ncol(prices), rows = 2:(nrow(prices) +
                                                                    1), rule = "$H2 > 0.01")

## style log return col as a percentage
logRetStyle <- createStyle(numFmt = "percentage")

addStyle(wb, 1, style = logRetStyle, rows = 2:(nrow(prices) + 1), cols = "H", gridExpand = TRUE)

setColWidths(wb, sheet = 1, cols = c("A", "F", "G", "H"), widths = 15)

## save workbook to working directory
saveWorkbook(wb, "stockPrice.xlsx", overwrite = TRUE)
openXL("stockPrice.xlsx")


