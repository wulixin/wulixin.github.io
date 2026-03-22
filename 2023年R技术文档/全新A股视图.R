

{library(officedown)
library(officer)
block_toc()
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
library(xts) }


##日期数据设置,比较最近五天的复盘数据

today<-ymd(Sys.Date())
today<-str_remove(str_remove(Sys.Date(), "-"),"-")
yesterday<-str_remove(str_remove(Sys.Date()-1, "-"),"-")
lasterday<-str_remove(str_remove(Sys.Date()-2, "-"),"-")
llday<-str_remove(str_remove(Sys.Date()-3, "-"),"-")
lllday<-str_remove(str_remove(Sys.Date()-4, "-"),"-")
latest_month<-str_remove(str_remove(Sys.Date()-150, "-"),"-")
start_date<-Sys.Date()-years(1)


## 股价获取数据修改  
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
#
stocks_names<-pro(api_name = 'stock_basic')
write_csv(stocks_names,'//Users//wulixin//Desktop//230618stocksnames.csv')
#stocks_names<-pro(api_name='stock_basic',fields='ts_code,symbol,name,area,industry,list_date')
#stocks_names<-read_csv('//Users//wulixin//Desktop//stocksnames.csv')


### 每日表现数据获取
dataprices1 = pro(api_name="daily",trade_date='20230616')

dataprices2 = pro(api_name="daily",trade_date='20230615')

dataprices3 = pro(api_name="daily",trade_date='20230614')

dataprices4 = pro(api_name="daily",trade_date='20230613')

dataprices5 = pro(api_name="daily",trade_date='20230612')



## 六大主要方向,七大主要产业链,
library(officer)
library(readxl)
JYZJGJAQ <- read_xls("//Users//wulixin//Desktop//短线操盘九阴真经之国家安全与战略.xls",
                       sheet="能源安全")
JYZJLSAQ <- read_xls("//Users//wulixin//Desktop//短线操盘九阴真经之国家安全与战略.xls",
                     sheet="粮食安全")
JYZJMSAQ <- read_xls("//Users//wulixin//Desktop//短线操盘九阴真经之国家安全与战略.xls",
                     sheet="民生安全")
JYZJJTQG <- read_xls("//Users//wulixin//Desktop//短线操盘九阴真经之国家安全与战略.xls",
                     sheet="交通强国")
JYZJWHQG <- read_xls("//Users//wulixin//Desktop//短线操盘九阴真经之国家安全与战略.xls",
                     sheet="文化强国")
JYZJHGQG <- read_xls("//Users//wulixin//Desktop//短线操盘九阴真经之国家安全与战略.xls",
                     sheet="化工强国")
JYZJZZYQG <- read_xls("//Users//wulixin//Desktop//短线操盘九阴真经之国家安全与战略.xls",
                     sheet="制造业强国")

JYZJ<-rbind(JYZJGJAQ,JYZJHGQG,JYZJJTQG,JYZJLSAQ,JYZJMSAQ,JYZJWHQG,JYZJZZYQG)

write.csv(JYZJ,"//Users//wulixin//Desktop//JYZJ.csv")

JYZJ%>%cross_join(dataprices1%>%select(ts_code,pct_chg),suffix=c('代码','ts_code'))%>%
  filter(pct_chg>1)%>%
  select(大方向)%>%
  group_by(大方向)%>%
  count()%>%arrange(desc(n))


#######################葵花宝典系列

KHBDDJK <- read_xls("//Users//wulixin//Desktop//短线操盘葵花宝典.xls",
                     sheet="大健康")
KHBDDXF <- read_xls("//Users//wulixin//Desktop//短线操盘葵花宝典.xls",
                     sheet="大消费")
KHBDDJR <- read_xls("//Users//wulixin//Desktop//短线操盘葵花宝典.xls",
                     sheet="大金融")
KHBDXNY <- read_xls("//Users//wulixin//Desktop//短线操盘葵花宝典.xls",
                     sheet="新能源")
KHBDXJJ <- read_xls("//Users//wulixin//Desktop//短线操盘葵花宝典.xls",
                     sheet="新基建")
KHBDXKJ <- read_xls("//Users//wulixin//Desktop//短线操盘葵花宝典.xls",
                     sheet="新科技")
KHBDGCTD <- read_xls("//Users//wulixin//Desktop//短线操盘葵花宝典.xls",
                      sheet="国产替代")

KHBD<-rbind(KHBDGCTD,KHBDXKJ,KHBDXJJ,KHBDXNY,KHBDDJR,KHBDDXF,KHBDDJK)

write.csv(KHBD,'//Users//wulixin//Desktop//KHBD.csv')

head(KHBD)
#######这个位置是概念，需要修改位大方向！
KHBD%>%cross_join(dataprices1%>%select(ts_code,pct_chg),suffix=c('代码','ts_code'))%>%
  filter(pct_chg>1)%>%
  select(概念)%>%
  group_by(概念)%>%
  count()%>%arrange(desc(n))


############################### 六大主要产业链


#######################葵花宝典系列六大产业链

PXJPDZSW <- read_xls("//Users//wulixin//Desktop//短线操盘辟邪剑谱之六大产业链.xls",
                    sheet="电子商务产业链")
PXJPZNQC <- read_xls("//Users//wulixin//Desktop//短线操盘辟邪剑谱之六大产业链.xls",
                    sheet="智能汽车产业链")
PXJPTGY <- read_xls("//Users//wulixin//Desktop//短线操盘辟邪剑谱之六大产业链.xls",
                    sheet="特高压产业链")
PXJPJG <- read_xls("//Users//wulixin//Desktop//短线操盘辟邪剑谱之六大产业链.xls",
                    sheet="军工产业链")
PXJPSTJJ<- read_xls("//Users//wulixin//Desktop//短线操盘辟邪剑谱之六大产业链.xls",sheet="实体经济产业")

PXJP<-rbind(PXJPSTJJ,PXJPJG,PXJPTGY,PXJPZNQC,PXJPDZSW)

write.csv(PXJP,'//Users//wulixin//Desktop//PXJP.csv')

#######这个位置是概念，需要修改位大方向！
PXJP%>%cross_join(dataprices1%>%select(ts_code,pct_chg),suffix=c('代码','ts_code'))%>%
  filter(pct_chg>1)%>%
  select(大方向)%>%
  group_by(大方向)%>%
  count()%>%arrange(desc(n))

#############################################

### 做五个页面

### 做出不同燃气的统计数据,方便找出每个行业爆发力最强的几只股票

### 板块分布全局图

```{r}
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
  hc_size(height = 600,width = 750)
```




### 爆发力游资偏好度

选择最近半年，最近一个月爆发过的个股，再次爆发的概率，火山再次点燃的概率会比较高！

```{r}

KHBDDT1<-KHBD%>%cross_join(dataprices1,suffix=c('代码','ts_code'))%>%
  filter(pct_chg>1)%>%
  select(公司名称,代码,收盘价,爆发力,游资偏好,产业趋势,大方向,pct_chg)

datatable(KHBDDT1,filter = 'top', options = list(
  pageLength = 15, autoWidth = TRUE))|>
  formatStyle('爆发力',color = styleInterval(c(5,10), c('green', 'blue', 'red')),
              fontWeight = styleInterval(12.5, c('normal', 'bold')))

```





### 社交关系网络图

物以类聚，人以群分,股票跟人一样！

```{r}
library(lubridate)
#dataprices = pro(api_name="daily",trade_date='20220826')
df_all<-dataprices%>%left_join(stocks_names)%>%filter(pct_chg > 6 & pct_chg < 21)

#df_all<-dataprices%>%left_join(stocks_names)%>%
#  filter(industry %in% c("供气供热"))

library(stringr)
stocks_names$list_date<-ymd(stocks_names$list_date)


#"小金属","铜",
#stocks_name<-df_all%>%filter(industry %in% c("医药商业","医疗保健","化学制药","生物制药"))%>%
#  filter(ymd(list_date) < ymd("20231001"))

stocks_name<-df_all%>%filter(ymd(list_date) < ymd("20230501"))

#"农药化肥","通信设备","化学制药","元器件","纺织","服饰"
#"元器件","电子元件","半导体"
#"医药商业","医疗保健","化学制药","生物制药",
#"焦炭加工","煤炭开采"
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

colnames(prices) = stocks_name$name[i]

##########clean up and transform data 
for(j in 1:ncol(prices)) prices[, j] = na.locf(prices[, j])       # fill in

prices = prices[, apply(prices, 2, function(x) ! any(is.na(x)))]

log_returns = apply(prices, 2, function(x) diff(log(x)))

head(log_returns)


X = cor(log_returns)
colnames(X)<-stocks_name$name[i]


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
        width=750,height=600,main="天网作战地图之社交关系网",
        bg = "black")
```




### 多头策略-注册制新股

```{r}

colnames(Top201)<-c("热度","名称","行业","代码","板块","交易时间","上市日期","火山类型","行情类型")

datatable(Top201,filter = 'top', options = list(
  pageLength = 10, autoWidth = TRUE),caption = htmltools::tags$caption(
    style = 'caption-side: Top; text-align: center;',
    '次新股热度排名', htmltools::em('"近期多头策略个股热度排行"')
  ))
```

### 智猪博弈之高风险股

```{r}
dataprices1 <- pro(api_name="daily",trade_date=yesterday1)%>%filter(pct_chg>2)
dataprices2 <- pro(api_name="daily",trade_date=yesterday2)%>%filter(pct_chg>2)
dataprices3 <- pro(api_name="daily",trade_date=yesterday3)%>%filter(pct_chg>2)

Top30<-dataprices%>%
  filter(pct_chg>5)%>%
  filter(ts_code %in% dataprices1$ts_code)%>%
  filter(ts_code %in% dataprices2$ts_code |ts_code %in% dataprices3$ts_code)%>%
  left_join(stocks_names)%>%
  filter(amount>80000 & close< 20)%>%
  select(pct_chg,name,industry,ts_code,market,trade_date,list_date)%>%
  mutate(list_date=ymd(list_date),trade_date=ymd(trade_date))%>%
  arrange(desc(pct_chg))

Top30$HS_type<-ifelse(Top30$pct_chg>5,"强烈买入信号","一般买入")

TopIndustry<-cxcount3%>%top_n(3)%>%select(industry)

Top30$SD_type<-ifelse(Top30$industry %in% TopIndustry$industry,"浪尖","风口")

Top301<-Top30%>%mutate(pct_chg=pct_chg*50)

colnames(Top301)<-c("评分","名称","行业","代码","板块","交易时间","上市日期","买入信号","形态")

datatable(Top301,filter = 'top', options = list(
  pageLength = 5, autoWidth = TRUE),caption = htmltools::tags$caption(
    style = 'caption-side: Top; text-align: center;',
    '智猪博弈', htmltools::em('"高度博弈,小部队追击"')
  ))
```






library(dplyr)
library(tidyverse)
library(tidyr)


ts_name<-stocks_names%>%select(ts_code)

ts_codes<-ts_name$ts_code

df_all<-dataprices%>%left_join(stocks_names)
df_all$list_date<-ymd(df_all$list_date)


library(jpeg)

#img <- image_read("/Users/wulixin/Desktop/Wechat.jpeg")
#img
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


###############
dataprices$pct_type<-cut(dataprices$pct_chg,breaks=c(-20,-4,0,3,8,20),labels=c("大跌","小跌","小涨","中涨","大涨"))

library(janitor)
library(gt)
t1<-dataprices%>%tabyl(pct_type)
library(dplyr)
dznum<-dataprices%>%tabyl(pct_type)%>%
  filter(pct_type %in% c("小涨","中涨","大涨"))%>%
  select(n)%>%
  sum()

subtitle1<-ifelse(dznum>2500,"投资策略：近期大盘形势很好,放手操作||智能持仓提醒：提高仓位，80%~100%仓位！",
                  ifelse(dznum>2000,"投资策略：大盘进入震荡,指数存在回调需求,投资风格切换,谨慎操作||智能持仓提醒：提高仓位，50%~60%仓位！",
                         ifelse(dznum>1000,"投资策略：大盘形势转差,降低频繁操作,上方存在压力,空头处于强势！ ||智能持仓提醒：控制仓位，50%仓位！",                                     ifelse(dznum>500,"投资策略：投资情绪低迷,赚钱效应差||智能持仓提醒：控制仓位，20%~50%仓位！","投资策略：极端行情,大盘超跌,个股超跌,注意做好空仓！伺机逐步低吸！"))))

qstable<-t1 %>%
  adorn_totals("row") %>%
  adorn_pct_formatting()

##这个位置容易出错
#
colnames(qstable)<-c("涨跌类型","数量","百分比","@百分比")

#colnames(qstable)<-c("涨跌类型","数量","百分比")


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
    footnote = "首战即决战，一战定乾坤，干大战定天下。——LIXIN WU ||指数连续上涨4，5个交易日关注回调做好减仓！赚钱之后做好减仓！",
    locations = cells_column_labels(
      columns = '数量' ) )




# 近期大资金偏好 

## 热门资金流入板块/盘子大小

* 羊群战略,观察羊群走势,通过头羊走势判断羊群走势,将金融行为学应用在A股投资中,结合动量效应与反转效应把握市场潜在机会！


```{r echo=FALSE,warning=FALSE,message=FALSE}
df_all$ShiZhi<-cut(df_all$amount,breaks=quantile(df_all$amount),
                   labels=c("迷你盘","小盘股","中盘股","大盘股"))
### 统计数据    可以使用的第一个大图   

cxcount3<-as.data.frame(df_all%>%filter(pct_chg>3)%>%count(industry)%>%arrange(desc(n)))
cxcount3$industry<-as.factor(cxcount3$industry)

cxcount3$n<-as.numeric(cxcount3$n)

cxcount3<-cxcount3%>%filter(n>6)

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
```

# 热门个股热度排行

## 股吧文本挖掘热门股票

```{r echo=FALSE,warning=FALSE,message=FALSE}
yesterday1<-str_remove(str_remove(Sys.Date()-1, "-"),"-")
yesterday2<-str_remove(str_remove(Sys.Date()-2, "-"),"-")
yesterday3<-str_remove(str_remove(Sys.Date()-3, "-"),"-")
yesterday4<-str_remove(str_remove(Sys.Date()-6, "-"),"-")

dataprices = pro(api_name="daily",trade_date=today)%>%filter(pct_chg>1)%>%left_join(stocks_names)
dataprices1 <- pro(api_name="daily",trade_date=yesterday1)%>%filter(pct_chg>1)%>%left_join(stocks_names)
dataprices2 <- pro(api_name="daily",trade_date=yesterday2)%>%filter(pct_chg>1)%>%left_join(stocks_names)
dataprices3 <- pro(api_name="daily",trade_date=yesterday3)%>%filter(pct_chg>1)%>%left_join(stocks_names)


text<-list(dataprices$name,dataprices1$name,dataprices2$name,dataprices3$name)
library(tmcn)
TDM<-createTDM(text, language = "cn")

demoFreq<-createWordFreq(TDM, onlyCN = TRUE, nosymbol = TRUE, stopwords = NULL,
                         useStopDic = FALSE)
demoFreq<-demoFreq%>%filter(freq>2)
library("d3wordcloud")
d3wordcloud(demoFreq$word, demoFreq$freq, size.scale = "linear")

```


## 动量理论与羊群效应

* 动量效应是由Jegadeesh和Titman（1993）提出的，是指股票的收益率有延续原来的运动方向的趋势，即过去一段时间收益率较高的股票在未来获得的收益率仍会高于过去收益率较低的股票。

* 大盘跌破3000点之后，市场的反转效应投资策略收益更为明显，过去一直表现很差的股票在反转效应期间赚钱效应会更好！

* 智能手机时代让信息不对称性减弱,信息的快速传递,让很多投资者听风就是雨,加重了跟风效应,羊群效应,加上抖音自媒体的宣传,打板战法,前期知名度很高，热度很高的个股,具有很好的群众基础和套牢盘,同时就像火山口一样,很容易再次点燃,吸引到更多跟风资金,同时真假难辨的消息很容易使得股票短期翻倍,比如贵州习酒上市,贵广网络,贵绳股份,两个公司相互辟谣,但最终还是翻倍！
因此事件驱动下热门个股因为知名度,热度,散户群众基础大涨概率更高！

```{r echo=FALSE,warning=FALSE,message=FALSE}

dataprices1 <- pro(api_name="daily",trade_date=yesterday1)%>%filter(pct_chg>1)
dataprices2 <- pro(api_name="daily",trade_date=yesterday2)%>%filter(pct_chg>1)
dataprices3 <- pro(api_name="daily",trade_date=yesterday3)%>%filter(pct_chg>1)

Top20<-dataprices%>%
  filter(pct_chg>1)%>%
  filter(ts_code %in% dataprices1$ts_code)%>%
  filter(ts_code %in% dataprices2$ts_code |ts_code %in% dataprices3$ts_code)%>%
  left_join(stocks_names)%>%
  filter(amount>80000)%>%
  select(pct_chg,name,industry,ts_code,market,trade_date,list_date)%>%
  mutate(list_date=ymd(list_date),trade_date=ymd(trade_date))%>%
  arrange(desc(pct_chg))

Top20$HS_type<-ifelse(Top20$pct_chg>3,"火山大爆发","火山爆发中")

TopIndustry<-cxcount3%>%top_n(6)%>%select(industry)

Top20$SD_type<-ifelse(Top20$industry %in% TopIndustry$industry,"大资金介入","个股分化")

Top201<-Top20%>%filter(ymd(list_date)>ymd('20200101'))%>%mutate(pct_chg=pct_chg*50)

Top202<-Top20%>%filter(ymd(list_date)<ymd('20200101'))%>%mutate(pct_chg=pct_chg*50)

colnames(Top201)<-c("热度","名称","行业","代码","板块","交易时间","上市日期","火山类型","行情类型")

datatable(Top201,filter = 'top', options = list(
  pageLength = 10, autoWidth = TRUE),caption = htmltools::tags$caption(
    style = 'caption-side: Top; text-align: center;',
    '次新股热度排名', htmltools::em('"近期多头策略个股热度排行"')
  ))
```


## 多头策略热门个股

```{r echo=FALSE,warning=FALSE,message=FALSE}

colnames(Top202)<-c("热度","名称","行业","代码","板块","交易时间","上市日期","火山类型","行情类型")

datatable(Top202,filter = 'top', options = list(
  pageLength = 10, autoWidth = TRUE),caption = htmltools::tags$caption(
    style = 'caption-side: Top; text-align: center;',
    '多头策略热门个股排名', htmltools::em('"近期多头策略个股热度排行"')
  ))
```


# 强烈买入信号决战牛股

## 纵横资本 A组合

适合投资群体1~5W资金客户,喜欢参与博弈,火山口🌋,火山一旦爆发,很难快速熄灭下来,因此存在高波动,高博弈的特点！
高频交易客户,高风险偏好,公司股价中等价格股票,中小市值偏好,牛散博弈！

```{r echo=FALSE,warning=FALSE,message=FALSE}
dataprices1 <- pro(api_name="daily",trade_date=yesterday1)%>%filter(pct_chg>2)
dataprices2 <- pro(api_name="daily",trade_date=yesterday2)%>%filter(pct_chg>2)
dataprices3 <- pro(api_name="daily",trade_date=yesterday3)%>%filter(pct_chg>2)

Top30<-dataprices%>%
  filter(pct_chg>5)%>%
  filter(ts_code %in% dataprices1$ts_code)%>%
  filter(ts_code %in% dataprices2$ts_code |ts_code %in% dataprices3$ts_code)%>%
  left_join(stocks_names)%>%
  filter(amount>80000 & close< 20)%>%
  select(pct_chg,name,industry,ts_code,market,trade_date,list_date)%>%
  mutate(list_date=ymd(list_date),trade_date=ymd(trade_date))%>%
  arrange(desc(pct_chg))

Top30$HS_type<-ifelse(Top30$pct_chg>5,"强烈买入信号","一般买入")

TopIndustry<-cxcount3%>%top_n(3)%>%select(industry)

Top30$SD_type<-ifelse(Top30$industry %in% TopIndustry$industry,"浪尖","风口")

Top301<-Top30%>%mutate(pct_chg=pct_chg*50)

colnames(Top301)<-c("评分","名称","行业","代码","板块","交易时间","上市日期","买入信号","形态")

datatable(Top301,filter = 'top', options = list(
  pageLength = 5, autoWidth = TRUE),caption = htmltools::tags$caption(
    style = 'caption-side: Top; text-align: center;',
    '智猪博弈', htmltools::em('"高度博弈,小部队追击"')
  ))
```

### 作战全局走势图

```{r echo=FALSE,warning=FALSE,message=FALSE}
stocks_name<-stocks_names%>%
  filter(name %in% Top30$name)

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

china_stocks %>%
  unnest(stock.prices)%>%
  select(Date,Close,name)%>%
  group_by(name) %>%
  do(p = plot_ly(., x = ~desc(ymd(Date)), y = ~Close,name=~name)%>%
       add_lines) %>%
  subplot(nrows =2, shareX = TRUE)%>%
  layout(title = "天网作战系统之全局走势图")

```  

## 纵横资本 B组合

良好的沟通是成功的一半,有格局,有魄力,有胆识的客户欢迎联系！

```{r echo=FALSE,warning=FALSE,message=FALSE}
dataprices1 <- pro(api_name="daily",trade_date=yesterday1)%>%filter(pct_chg>0 & pct_chg< 2 )
dataprices2 <- pro(api_name="daily",trade_date=yesterday2)%>%filter(pct_chg>0 & pct_chg< 2)
dataprices3 <- pro(api_name="daily",trade_date=yesterday3)%>%filter(pct_chg>0 & pct_chg< 2)

Top40<-dataprices%>%
  filter(pct_chg>1 & pct_chg<5 )%>%
  filter(ts_code %in% dataprices1$ts_code)%>%
  filter(ts_code %in% dataprices2$ts_code |ts_code %in% dataprices3$ts_code)%>%
  left_join(stocks_names)%>%
  filter(amount>80000 & close< 20)%>%
  select(pct_chg,name,industry,ts_code,market,trade_date,list_date)%>%
  mutate(list_date=ymd(list_date),trade_date=ymd(trade_date))%>%
  arrange(desc(pct_chg))

Top40$HS_type<-ifelse(Top40$pct_chg < 3,"一般买入","买入")

TopIndustry<-cxcount3%>%top_n(6)%>%select(industry)

Top40$SD_type<-ifelse(Top40$industry %in% TopIndustry$industry,"浪尖","风口")

Top401<-Top40%>%mutate(pct_chg=pct_chg*50)

colnames(Top401)<-c("评分","名称","行业","代码","板块","交易时间","上市日期","买入信号","形态")

datatable(Top401,filter = 'top', options = list(
  pageLength = 5, autoWidth = TRUE),caption = htmltools::tags$caption(
    style = 'caption-side: Top; text-align: center;',
    '智猪博弈', htmltools::em('"高度博弈,小部队追击"')
  ))
```


## 纵横资本 C组合

良好的沟通是成功的一半,有格局,有魄力,有胆识的客户欢迎联系！

```{r echo=FALSE,warning=FALSE,message=FALSE}
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

dataprices1 <- pro(api_name="daily",trade_date=yesterday1)%>%filter(pct_chg>-1 & pct_chg< 1 )
dataprices2 <- pro(api_name="daily",trade_date=yesterday2)%>%filter(pct_chg>-1 & pct_chg< 1)
dataprices3 <- pro(api_name="daily",trade_date=yesterday3)%>%filter(pct_chg>-1 & pct_chg< 1)

Top50<-dataprices%>%
  filter(pct_chg>1 & pct_chg<3)%>%
  filter(ts_code %in% dataprices1$ts_code)%>%
  filter(ts_code %in% dataprices2$ts_code |ts_code %in% dataprices3$ts_code)%>%
  left_join(stocks_names)%>%
  filter(amount>80000 & close< 20)%>%
  select(pct_chg,name,industry,ts_code,market,trade_date,list_date)%>%
  mutate(list_date=ymd(list_date),trade_date=ymd(trade_date))%>%
  arrange(desc(pct_chg))

Top50$HS_type<-ifelse(Top50$pct_chg>5,"买入","关注")

TopIndustry<-cxcount3%>%top_n(10)%>%select(industry)

Top50$SD_type<-ifelse(Top50$industry %in% TopIndustry$industry,"火山异动","蓄势待发")

Top501<-Top50%>%mutate(pct_chg=pct_chg*50)

colnames(Top501)<-c("评分","名称","行业","代码","板块","交易时间","上市日期","买入信号","形态")

datatable(Top501,filter = 'top', options = list(
  pageLength = 5, autoWidth = TRUE),caption = htmltools::tags$caption(
    style = 'caption-side: Top; text-align: center;',
    '智猪博弈', htmltools::em('"高度博弈,小部队追击"')
  ))

stocks_name<-stocks_names%>%
  filter(name %in% Top50$name)

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

china_stocks %>%
  unnest(stock.prices)%>%
  select(Date,Close,name)%>%
  group_by(name) %>%
  do(p = plot_ly(., x = ~desc(ymd(Date)), y = ~Close,name=~name)%>%
       add_lines) %>%
  subplot(nrows =5, shareX = TRUE)%>%
  layout(title = "天网作战系统之全局走势图")

```


良好的沟通是成功的一半,有格局,有魄力,有胆识的客户欢迎联系！

# 价格维度—高价VS低价

在2020年的上涨行情中,高价股持续上涨不断新高,在这一波行情中高价股投资者赚的盆满钵满,随着时间到了2021年,持有煤炭,钢铁,造纸,有色,玻璃等传统行业上涨的过程中,低价股不断的翻倍.


## ST低价垃圾黄金区

```{r echo=FALSE,warning=FALSE,message=FALSE}

library(dplyr)
library(tidyverse)
df_allST<-df_all%>%
  #filter(industry %in% c("汽车整车","软件服务"))%>%
  filter(name %in% df_all[str_which(df_all$name,'ST'),13] & pct_chg>4)%>%
  arrange(desc(pct_chg))%>%
  select(name,ts_code,pct_chg,list_date,close,industry,area,market,vol)

colnames(df_allST)<-c("名称","代码","涨跌幅","上市时间","收盘价","行业","地区","板块","交易量")


datatable(df_allST,filter = 'top', options = list(
  pageLength = 5, autoWidth = TRUE))|>
  formatStyle('收盘价',color = styleInterval(c(100,600), c('green', 'blue', 'red')),
              fontWeight = styleInterval(12.5, c('normal', 'bold'))) |>
  formatStyle('涨跌幅',color = styleInterval(c(0, 5), c('green', 'blue', 'red')),
              fontWeight = styleInterval(5, c('normal', 'bold'))) |>
  formatStyle( 
    '交易量',color = styleInterval(c(5000, 100000), c('green', 'blue', 'red')),
    backgroundColor = styleInterval(10000, c('gray', 'yellow'))) 

```  


## 低价活跃个股

```{r echo=FALSE,warning=FALSE,message=FALSE}
shiyuan <- df_all%>%
  filter(close<10 & pct_chg>5)%>%
  select(ts_code,name,industry,close,pct_chg,vol,change)%>%
  arrange(desc(pct_chg))


colnames(shiyuan)<-c("代码","公司名称","行业","收盘价","涨跌幅","交易量","价格变化")

library(janitor)
library(purrr)

shiyuan%>%
  tabyl("行业")%>%
  adorn_totals("row") %>%
  adorn_pct_formatting()%>%
  arrange(desc(n))%>%
  filter(n>5)%>%
  knitr::kable()

datatable(shiyuan,filter = 'top', options = list(
  pageLength = 35, autoWidth = TRUE))|>
  formatStyle('收盘价',color = styleInterval(c(100,600), c('green', 'blue', 'red')),
              fontWeight = styleInterval(12.5, c('normal', 'bold'))) |>
  formatStyle('涨跌幅',color = styleInterval(c(0, 5), c('green', 'blue', 'red')),
              fontWeight = styleInterval(5, c('normal', 'bold'))) |>
  formatStyle( 
    '交易量',color = styleInterval(c(5000, 100000), c('green', 'blue', 'red')),
    backgroundColor = styleInterval(10000, c('gray', 'yellow'))) 

```


## 高价活跃个股

```{r echo=FALSE,warning=FALSE,message=FALSE}
baiyuan <- df_all%>%
  filter(close>100 & pct_chg> 4)%>%
  select(ts_code,name,industry,close,pct_chg,vol,change)%>%
  arrange(desc(pct_chg))


colnames(baiyuan)<-c("代码","公司名称","行业","收盘价","涨跌幅","交易量","价格变化")

baiyuan%>%
  tabyl("行业")%>%
  adorn_totals("row") %>%
  adorn_pct_formatting()%>%
  arrange(desc(n))%>%
  filter(n>5)%>%
  knitr::kable()

datatable(baiyuan,filter = 'top', options = list(
  pageLength = 25, autoWidth = TRUE))|>
  formatStyle('收盘价',color = styleInterval(c(100,600), c('green', 'blue', 'red')),
              fontWeight = styleInterval(12.5, c('normal', 'bold'))) |>
  formatStyle('涨跌幅',color = styleInterval(c(0, 5), c('green', 'blue', 'red')),
              fontWeight = styleInterval(5, c('normal', 'bold'))) |>
  formatStyle( 
    '交易量',color = styleInterval(c(5000, 100000), c('green', 'blue', 'red')),
    backgroundColor = styleInterval(10000, c('gray', 'yellow'))) 

```


# 上市时间长短维度 


## 次新股VS旧股

在IPO的进程中,次新股代表新型经济特征的公司,尤其是服务业,科技创新行业的公司不断的上市,这对于过去的投资者来说因为IPO数量不断的增多,对于市场上公司的认知存在很大的挑战！

```{r echo=FALSE,warning=FALSE,message=FALSE}
cixin<- df_all%>%
  filter(list_date>ymd(latest_month) & pct_chg>3)%>%
  select(ts_code,name,industry,close,pct_chg,vol,change)%>%
  arrange(desc(pct_chg))

colnames(cixin)<-c("代码","公司名称","行业","收盘价","涨跌幅","交易量","价格变化")

cixin%>%
  tabyl("行业")%>%
  adorn_totals("row") %>%
  adorn_pct_formatting()%>%
  arrange(desc(n))%>%
  filter(n>5)%>%
  knitr::kable()

datatable(cixin,filter = 'top', options = list(
  pageLength = 25, autoWidth = TRUE))|>
  formatStyle('收盘价',color = styleInterval(c(100,600), c('green', 'blue', 'red')),
              fontWeight = styleInterval(12.5, c('normal', 'bold'))) |>
  formatStyle('涨跌幅',color = styleInterval(c(0, 5), c('green', 'blue', 'red')),
              fontWeight = styleInterval(5, c('normal', 'bold'))) |>
  formatStyle( 
    '交易量',color = styleInterval(c(5000, 100000), c('green', 'blue', 'red')),
    backgroundColor = styleInterval(10000, c('gray', 'yellow'))) 

```



## 热门老股

过去呈现出大盘牛的特征,而现在呈现出的特点指数牛,板块牛的行情,一个板块一旦坐在了风口浪尖,那么这个板块的股票将出现持续性较强的拉升周期!
  
  ```{r echo=FALSE,warning=FALSE,message=FALSE}
ziJing<-df_all%>%filter(industry %in% c("生物制药","白酒","小金属","元器件","电子元件","半导体") & pct_chg>3)%>%
  select(ts_code,name,industry,close,pct_chg,vol,change)%>%
  arrange(desc(pct_chg))

colnames(ziJing)<-c("代码","公司名称","行业","收盘价","涨跌幅","交易量","价格变化")

ziJing%>%
  tabyl("行业")%>%
  adorn_totals("row") %>%
  adorn_pct_formatting()%>%
  arrange(desc(n))%>%
  filter(n>5)%>%
  knitr::kable()


datatable(ziJing,filter = 'top', options = list(
  pageLength = 25, autoWidth = TRUE))|>
  formatStyle('收盘价',color = styleInterval(c(100,600), c('green', 'blue', 'red')),
              fontWeight = styleInterval(12.5, c('normal', 'bold'))) |>
  formatStyle('涨跌幅',color = styleInterval(c(0, 5), c('green', 'blue', 'red')),
              fontWeight = styleInterval(5, c('normal', 'bold'))) |>
  formatStyle( 
    '交易量',color = styleInterval(c(5000, 100000), c('green', 'blue', 'red')),
    backgroundColor = styleInterval(10000, c('gray', 'yellow'))) 

```



# 涨跌幅板块维度

科创板创业板能够上涨20%，主板却只有10%，对于板块的认知,科创板个股上市以来出现了持续的夭折,并且不断的新低,但正如博弈论中所说,这种均衡状态是不会持续太久的,如果100%的人都不赚钱,那么总有一天会打破这种均衡！

## 北交所

```{r echo=FALSE,warning=FALSE,message=FALSE}

beijiao<- df_all%>%
  filter(market %in% c("北交所") & pct_chg>1)%>%
  select(ts_code,name,industry,close,pct_chg,vol,change)%>%
  arrange(desc(pct_chg))


colnames(beijiao)<-c("代码","名称","行业","收盘价","涨跌幅","交易量","价格变化")

datatable(beijiao,filter = 'top', options = list(
  pageLength = 15, autoWidth = TRUE))|>
  formatStyle('收盘价',color = styleInterval(c(100,600), c('green', 'blue', 'red')),
              fontWeight = styleInterval(12.5, c('normal', 'bold'))) |>
  formatStyle('涨跌幅',color = styleInterval(c(0, 5), c('green', 'blue', 'red')),
              fontWeight = styleInterval(5, c('normal', 'bold'))) |>
  formatStyle( 
    '交易量',color = styleInterval(c(5000, 100000), c('green', 'blue', 'red')),
    backgroundColor = styleInterval(10000, c('gray', 'yellow')))

```


##  科创板个股

```{r echo=FALSE,warning=FALSE,message=FALSE}
kechuang <- df_all%>%
  filter(market == "科创板" & pct_chg>5)%>%
  select(ts_code,name,industry,close,pct_chg,vol,change)%>%
  arrange(desc(pct_chg))

colnames(kechuang)<-c("代码","公司名称","行业","收盘价","涨跌幅","交易量","价格变化")

kechuang%>%
  tabyl("行业")%>%
  adorn_totals("row") %>%
  adorn_pct_formatting()%>%
  arrange(desc(n))%>%
  filter(n>5)%>%
  knitr::kable()

datatable(kechuang,filter = 'top', options = list(
  pageLength = 15, autoWidth = TRUE))|>
  formatStyle('收盘价',color = styleInterval(c(100,600), c('green', 'blue', 'red')),
              fontWeight = styleInterval(12.5, c('normal', 'bold'))) |>
  formatStyle('涨跌幅',color = styleInterval(c(0, 5), c('green', 'blue', 'red')),
              fontWeight = styleInterval(5, c('normal', 'bold'))) |>
  formatStyle( 
    '交易量',color = styleInterval(c(5000, 100000), c('green', 'blue', 'red')),
    backgroundColor = styleInterval(10000, c('gray', 'yellow'))) 

```


##  创业板个股

注册制改革之后，创业板大量公司上市,必然会分解改革的阵痛；

```{r echo=FALSE,warning=FALSE,message=FALSE}
chuangyeban <- df_all%>%
  filter(market == "创业板" & pct_chg>5)%>%
  select(ts_code,name,industry,close,pct_chg,vol,change)%>%
  arrange(desc(pct_chg))

colnames(chuangyeban)<-c("代码","公司名称","行业","收盘价","涨跌幅","交易量","价格变化")

chuangyeban%>%
  tabyl("行业")%>%
  adorn_totals("row") %>%
  adorn_pct_formatting()%>%
  arrange(desc(n))%>%
  filter(n>5)%>%
  knitr::kable()


datatable(chuangyeban,filter = 'top', options = list(
  pageLength = 25, autoWidth = TRUE))|>
  formatStyle('收盘价',color = styleInterval(c(100,600), c('green', 'blue', 'red')),
              fontWeight = styleInterval(12.5, c('normal', 'bold'))) |>
  formatStyle('涨跌幅',color = styleInterval(c(0, 5), c('green', 'blue', 'red')),
              fontWeight = styleInterval(5, c('normal', 'bold'))) |>
  formatStyle( 
    '交易量',color = styleInterval(c(5000, 100000), c('green', 'blue', 'red')),
    backgroundColor = styleInterval(10000, c('gray', 'yellow'))) 


```


## 主板

```{r echo=FALSE,warning=FALSE,message=FALSE}
zhuban<- df_all%>%
  filter(market %in% c("主板","中小板") & pct_chg>5)%>%
  select(ts_code,name,industry,close,pct_chg,vol,change)%>%
  arrange(desc(pct_chg))
colnames(zhuban)<-c("代码","公司名称","行业","收盘价","涨跌幅","交易量","价格变化")

zhuban%>%
  tabyl("行业")%>%
  adorn_totals("row") %>%
  adorn_pct_formatting()%>%
  arrange(desc(n))%>%
  filter(n>5)%>%
  knitr::kable()

datatable(zhuban,filter = 'top', options = list(
  pageLength = 45, autoWidth = TRUE))|>
  formatStyle('收盘价',color = styleInterval(c(100,600), c('green', 'blue', 'red')),
              fontWeight = styleInterval(12.5, c('normal', 'bold'))) |>
  formatStyle('涨跌幅',color = styleInterval(c(0, 5), c('green', 'blue', 'red')),
              fontWeight = styleInterval(5, c('normal', 'bold'))) |>
  formatStyle( 
    '交易量',color = styleInterval(c(5000, 100000), c('green', 'blue', 'red')),
    backgroundColor = styleInterval(10000, c('gray', 'yellow'))) 

```


执业编号：S0770621060006 投资有风险，投资需要谨慎

温馨提示投资有风险,投资需要谨慎!
  
  更多内容点击 [关注小编](http://wulixin.github.io//QuandlFinance//home.html) 

footnotes^[内心的强大才是真的强大]