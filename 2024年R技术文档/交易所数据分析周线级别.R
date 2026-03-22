################################################
#
#               本周趋势股
#
###############################################


library(lubridate)
library(openxlsx)
library(readxl)
library(readr)
library(dplyr)
library(dbplyr)
library(dtplyr)
library(tidyr)
library(tidyverse)

dataETF<-read_excel('//Users//wulixin//Desktop//基金行情.xlsx',sheet="基金行情")
str(dataETF)
dataETF$交易日期<-ymd(dataETF$交易日期)
dataETF$`涨跌幅（%）`<-as.numeric(dataETF$`涨跌幅（%）`)
dataETF$`成交金额(万元)`<-as.numeric(dataETF$`成交金额(万元)`)
dataETF$证券简称<-as.factor(dataETF$证券简称)

quantile(dataETF$`涨跌幅（%）`)
min(dataETF$`涨跌幅（%）`)
dataETF$pct_ch<-dataETF$`涨跌幅（%）`/100
dataETF$type<-ifelse(dataETF$pct_ch>0.04 ,"本周有大资金介入",
                       ifelse(dataETF$pct_ch>0.02 ,"温和看涨",
                         ifelse(dataETF$pct_ch>-0.01 ,"不看跌",
                            ifelse(dataETF$pct_ch>-0.03 ,"温和看跌","本周有大资金流出"))))

dataETF$周<-week(dataETF$交易日期)
#####本周有大资金偏好的
todaydataETF<-dataETF%>%
  filter(type=="本周有大资金介入")%>%
  select("证券简称","证券代码","周")

todaydataETF<-dataETF%>%
  filter(type=="本周有大资金流出")%>%
  select("证券简称","证券代码","周")

dataETF$pct_ch
todaydataETF<-dataETF%>%
  filter(pct_ch>0)%>%
  select("证券简称","证券代码","周")


colnames(todaydataETF)<-c("名称","代码","周")
todaydataETF$类型<-"本周热度飙升板"


#[1] "交易日期"       "证券代码"       "证券简称"       "前收"           "开盘"           "最高"          
#[7] "最低"           "今收"           "涨跌幅（%）"    "成交量（万份）" "成交金额(万元)" "每百份基金净值"
#[13] "pct_ch"         "type"    

colnames(dataETF)
#######周热度板数据可视化
head(todaydataETF)
datatable(todaydataETF)
todaydataETF$名称<-as.character(todaydataETF$名称)

TJ<-as.data.frame(table(todaydataETF$名称,todaydataETF$类型))
#TJ%>%filter(TJ$Freq>2)

########词云图可视化

library(d3wordcloud)
d3wordcloud(TJ$Var1, TJ$Freq, colors = c("#000000", "#0000FF", "#FF0000"))

#d3wordcloud(TJ$Var1, TJ$Freq, colors = substr(viridis::viridis(10, 1), 0 , 7))
#d3wordcloud(TJ$Var1, TJ$Freq, rotate.min = -180, rotate.max = 180)



#################################股票数据分析


dataStocks<-read_excel('//Users//wulixin//Desktop//股票行情M3W2.xlsx',sheet="股票行情")
str(dataStocks)

dataStocks$交易日期<-ymd(dataStocks$交易日期)
dataStocks$`涨跌幅（%）`<-as.numeric(dataStocks$`涨跌幅（%）`)
dataStocks$`成交金额(万元)`<-as.numeric(dataStocks$`成交金额(万元)`)
dataStocks$证券简称<-as.factor(dataStocks$证券简称)

quantile(dataStocks$`涨跌幅（%）`)
min(dataStocks$`涨跌幅（%）`)
dataStocks$pct_ch<-dataStocks$`涨跌幅（%）`/100
dataStocks$type<-ifelse(dataStocks$pct_ch>0.04 ,"本周有大资金介入",
                     ifelse(dataStocks$pct_ch>0.02 ,"温和看涨",
                            ifelse(dataStocks$pct_ch>-0.01 ,"不看跌",
                                   ifelse(dataStocks$pct_ch>-0.03 ,"温和看跌","本周有大资金流出"))))

dataStocks$周<-week(dataStocks$交易日期)
#####本周有大资金偏好的
todaydataStocks<-dataStocks%>%
  filter(type=="本周有大资金介入")%>%
  select("证券简称","证券代码","周")

todaydataStocks<-dataStocks%>%
  filter(type=="本周有大资金流出")%>%
  select("证券简称","证券代码","周")


colnames(todaydataStocks)<-c("名称","代码","周")
todaydataStocks$类型<-"本周热度飙升板"

colnames(dataStocks)
##################周热度板数据可视化
head(todaydataStocks)
datatable(todaydataStocks)
todaydataStocks$名称<-as.character(todaydataStocks$名称)

TJ<-as.data.frame(table(todaydataStocks$名称,todaydataStocks$类型))
TJ%>%filter(TJ$Freq>2)

########词云图可视化

library(d3wordcloud)
d3wordcloud(TJ$Var1, TJ$Freq, colors = c("#000000", "#0000FF","#FF0000"))



#################################指数数据分析


dataZS<-read_excel('//Users//wulixin//Desktop//指数行情M3W2.xlsx',sheet="指数行情")

dataZS$交易日期<-ymd(dataZS$交易日期)
dataZS$`涨跌幅（%）`<-as.numeric(dataZS$`涨跌幅（%）`)
dataZS$`成交金额(万元)`<-as.numeric(dataZS$`成交金额(万元)`)
dataZS$指数简称<-as.factor(dataZS$指数简称)

quantile(dataZS$`涨跌幅（%）`)
min(dataZS$`涨跌幅（%）`)
dataZS$pct_ch<-dataZS$`涨跌幅（%）`/100
dataZS$type<-ifelse(dataZS$pct_ch>0.04 ,"本周有大资金介入",
                        ifelse(dataZS$pct_ch>0.02 ,"温和看涨",
                               ifelse(dataZS$pct_ch>-0.01 ,"不看跌",
                                      ifelse(dataZS$pct_ch>-0.03 ,"温和看跌","本周有大资金流出"))))

dataZS$周<-week(dataZS$交易日期)
#####本周有大资金偏好的
todaydataZS<-dataZS%>%
  filter(type=="本周有大资金介入")%>%
  select("指数简称","指数代码","周")
datatable(todaydataZS,caption = '多头炮指数')

todaydataZS<-dataZS%>%
  filter(type=="本周有大资金流出")%>%
  select("指数简称","指数代码","周")


colnames(todaydataZS)<-c("名称","代码","周")
todaydataZS$类型<-"本周热度飙升板"

colnames(dataZS)
#######周热度板数据可视化
head(todaydataZS)
datatable(todaydataZS)
todaydataZS$名称<-as.character(todaydataZS$名称)

TJ<-as.data.frame(table(todaydataZS$名称,todaydataZS$类型))
#TJ%>%filter(TJ$Freq>2)

########词云图可视化

library(d3wordcloud)
d3wordcloud(TJ$Var1, TJ$Freq, colors = c("#000000", "#0000FF","#FF0000"))

#############这个数据适合用表格的形式表示出来



######################################每日板块信息

todayHq<-read_excel('//Users//wulixin//Desktop//每日板块信息.xlsx',sheet="指数表现")


library(DT)
datatable(todayHq,caption = '指数表现:重点关注成交量变化,累计涨幅',
          filter = 'top',extensions = 'Buttons', options = list(pageLength = 8, 
                  autoWidth = TRUE,dom = 'Bfrtip',buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))|>
  formatStyle('日涨跌幅（%）',color = styleInterval(c(-0.02,0), c('green', 'blue', 'red')),
              fontWeight = styleInterval(12.5, c('normal', 'bold')))|>
  formatStyle('成交额较昨日增减（%）',color = styleInterval(c(-5,2), c('green', 'blue', 'red')),
              fontWeight = styleInterval(12.5, c('normal', 'bold')))|>
  formatStyle('今年以来涨跌幅（%）',color = styleInterval(c(-1,2), c('green', 'blue', 'red')),
              fontWeight = styleInterval(12.5, c('normal', 'bold')))|>
  formatStyle('收盘',  color = 'red', backgroundColor = 'pink', fontWeight = 'bold')|>
  formatStyle('指数名称',  color = 'red', backgroundColor = 'orange', fontWeight = 'bold')|>
  formatStyle('指数代码',  color = 'black', backgroundColor = 'lightblue', fontWeight = 'bold')|>
  formatStyle('今年以来涨跌',background = styleColorBar(todayHq$今年以来涨跌, 'steelblue'),backgroundSize = '1 800',backgroundRepeat = 'no-repeat',backgroundPosition = 'center')



####################################指数估值

todayGZ<-read_excel('//Users//wulixin//Desktop//每日板块信息.xlsx',sheet="指数估值")

colnames(todayGZ)
str(todayGZ)

library(DT)
datatable(todayGZ,caption = '指数估值:股息率与市净率',
          filter = 'top',extensions = 'Buttons', options = list(pageLength = 8, 
                                                                autoWidth = TRUE,dom = 'Bfrtip',buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))|>
  formatStyle('市静率',color = styleInterval(c(0,1), c('green', 'blue', 'red')),
              fontWeight = styleInterval(12.5, c('normal', 'bold')))|>
  formatStyle("股息率",color = styleInterval(c(1,4), c('green', 'blue', 'red')),
              fontWeight = styleInterval(12.5, c('normal', 'bold')))|>
  formatStyle('指数名称）',  color = 'red', backgroundColor = 'pink', fontWeight = 'bold')|>
  formatStyle('静态市盈率',  color = 'red', backgroundColor = 'orange', fontWeight = 'bold')|>
  formatStyle('滚动市盈率',  color = 'black', backgroundColor = 'lightblue', fontWeight = 'bold')


######################################### 每日板块信息

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


#######图形一：全局操盘视图  行业————时间维度

library(RColorBrewer)
library(viridis)
library(viridisLite)
library(dplyr)
library(highcharter)
todayBK<-read_excel('//Users//wulixin//Desktop//每日板块信息.xlsx',sheet="每日板块信息")

colnames(todayBK)
str(todayBK)

todayBK$`日涨跌幅（%）`<-as.numeric(todayBK$`日涨跌幅（%）`)
treemap_data1 <- todayBK%>%
  mutate(category = gsub(" ", "-",指数名称),
         subcategory = gsub(" ", "-",板块名称))%>%
  select(category, subcategory,`日涨跌幅（%）`)

pkmn_min <- treemap_data1 %>% 
  mutate(category = stringr::str_to_title(category)) %>% 
  mutate(subcategory= ifelse(is.na(subcategory), category, paste(category, "+", subcategory))) %>%
  mutate(val = 1)

cols <- pkmn_min %>% 
  count(category,subcategory, sort = TRUE) %>% 
  pull(subcategory) %>% 
  unique()
hchart(
  data_to_hierarchical(treemap_data1,c("category","subcategory","日涨跌幅（%）"), size='日涨跌幅（%）', 
                       colors =rev(viridis(8))),type = "treemap",allowDrillToNode = TRUE,
  tooltip = list(valueDecimals = FALSE),levels = lvl_opts) %>% 
  hc_chart(
    style = list(fontFamily = "Gloria Hallelujah")) %>% 
  hc_title(text = "每日最强与最弱板块信息",
           style = list(fontFamily = "Gloria Hallelujah",fontSize = "30px", color = "red", fontWeight = "bold")) %>% 
  hc_size(height = 750,width = 1350) 


##########################指数贡献
todayZSGX<-read_excel('//Users//wulixin//Desktop//每日板块信息.xlsx',sheet="指数贡献")

colnames(todayZSGX)
str(todayZSGX)

todayZSGX$贡献点数<-as.numeric(todayZSGX$贡献点数)

treemap_data2 <- todayZSGX%>%
  mutate(category = gsub(" ", "-",贡献指数名称),
         subcategory = gsub(" ", "-",股票名称))%>%
  select(category, subcategory,贡献点数)

pkmn_min <- treemap_data2 %>% 
  mutate(category = stringr::str_to_title(category)) %>% 
  mutate(subcategory= ifelse(is.na(subcategory), category, paste(category, "+", subcategory))) %>%
  mutate(val = 1)

cols <- pkmn_min %>% 
  count(category,subcategory, sort = TRUE) %>% 
  pull(subcategory) %>% 
  unique()
hchart(
  data_to_hierarchical(treemap_data2,c("category","subcategory","贡献点数"), size='贡献点数', 
                       colors =rev(viridis(8))),type = "treemap",allowDrillToNode = TRUE,
  tooltip = list(valueDecimals = FALSE),levels = lvl_opts) %>% 
  hc_chart(
    style = list(fontFamily = "Gloria Hallelujah")) %>% 
  hc_title(text = "权重指数前十大贡献股",
           style = list(fontFamily = "Gloria Hallelujah",fontSize = "30px", color = "red", fontWeight = "bold")) %>% 
  hc_size(height = 750,width = 650) 

colnames(todayZSGX)

todayZSGX$收盘价<-as.numeric(todayZSGX$收盘价)
todayZSGX$`日涨跌幅(%)`<-as.numeric(todayZSGX$`日涨跌幅(%)`)

datatable(todayZSGX,caption = '关注指数高贡献个股',filter = 'top',extensions = 'Buttons', options = list(pageLength = 100, 
                autoWidth = TRUE,dom = 'Bfrtip',buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))|>
  formatStyle('股票名称',  color = 'red', backgroundColor = 'pink', fontWeight = 'bold')|>
  formatStyle('贡献指数名称',  color = 'red', backgroundColor = 'orange', fontWeight = 'bold')|>
  formatStyle('收盘价',  color = 'black', backgroundColor = 'lightyellow', fontWeight = 'bold')|>
  formatStyle('日涨跌幅(%)',color = styleInterval(c(-0.02,1), c('green', 'blue', 'red')))|>
  formatStyle('贡献点数',color = styleInterval(c(0,5), c('green', 'blue', 'red')))


##########################中证股息率与市净率

Stocks_ZZ<-read_excel('//Users//wulixin//Desktop//中证股息率与市净率.xls',sheet="个股数据")
Stocks_ZZ$个股静态市盈率<-as.numeric(Stocks_ZZ$个股股息率)
Stocks_ZZ$个股滚动市盈率<-as.numeric(Stocks_ZZ$个股股息率)
Stocks_ZZ$个股市净率<-as.numeric(Stocks_ZZ$个股股息率)
Stocks_ZZ$个股股息率<-as.numeric(Stocks_ZZ$个股股息率)

str(Stocks_ZZ)
colnames(Stocks_ZZ)
Stocks_ZZF<-Stocks_ZZ%>%select("证券代码","证券名称","一级行业名称","四级行业名称","个股静态市盈率","个股滚动市盈率","个股市净率","个股股息率")            
datatable(Stocks_ZZF,caption = '指数表现:重点关注成交量变化,累计涨幅',
          filter = 'top',extensions = 'Buttons', options = list(pageLength = 100, 
                                                                autoWidth = TRUE,dom = 'Bfrtip',buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))|>
  formatStyle('一级行业名称',  color = 'red', backgroundColor = 'pink', fontWeight = 'bold')|>
  formatStyle('四级行业名称',  color = 'red', backgroundColor = 'orange', fontWeight = 'bold')|>
  formatStyle('证券名称',  color = 'black', backgroundColor = 'lightblue', fontWeight = 'bold')


"#FFC0CB" "#FFFF00" "#00FF00"
#d3wordcloud(TJ$Var1, TJ$Freq, colors = substr(viridis::viridis(10, 1), 0 , 7))
#d3wordcloud(TJ$Var1, TJ$Freq, rotate.min = -180, rotate.max = 180)




##########################
###########涨幅居前的概念
##########跌幅居前的概念
##########成交量居前的
#########超级看多
#########温和看多
########不看涨
library(stringi)
library(stringr)
library(sentimentr)
library(quanteda)
library(SnowballC)
library(tm)
library(tmcn)
library(openNLP)
library(NLP4R)
library(ICTCLAS)
library(RWordSeg)
library(tmcn)
library(jiebaR)
text<-list(todaydataETF$名称)

TDM<-createTDM(text, language = "cn")
TDM<-createDTM(text, language = c("zh", "en"), tokenize = NULL, removePunctuation = TRUE, 
          removeNumbers = TRUE, removeStopwords = TRUE)
demoFreq<-createWordFreq(TDM, onlyCN = TRUE, nosymbol = TRUE, stopwords = NULL,
                         useStopDic = FALSE)
demoFreq<-demoFreq%>%filter(freq>2)


library("d3wordcloud")
d3wordcloud(demoFreq$word, demoFreq$freq, size.scale = "linear")

d3wordcloud(demoFreq$word, demoFreq$freq, colors = c("#000000", "#0000FF", "#FF0000"))


d3wordcloud(demoFreq$word, demoFreq$freq, rotate.min = -180, rotate.max = 180)

d3wordcloud(demoFreq$word, demoFreq$freq, , spiral = "archimedean")

library(wordcloud2)

wordcloud2(demoFreq, color = "random-light", backgroundColor = "grey")

wordcloud2(demoFreq, minRotation = -pi/6, maxRotation = -pi/6, minSize = 10,
           rotateRatio = 1)

library(jiebaR)
library(jiebaRD)
library(DT)
library(data.table)

library(plotly)
library(lattice)
BZ_ETF<-dataETF%>%
  filter(type=="本周有大资金介入")
library(showtext)
showtext_auto(enable=TRUE)
font_add('Songti','Songti.ttc')
font_families()

xyplot(`涨跌幅（%）` ~ 证券简称 | 交易日期,data=BZ_ETF,layout = c(5,1),par.settings = list(superpose.symbol = list(pch = 16, col = "red")),
       scales = list(x = list(relation = "free"), y = list(relation = "free")),
       auto.key = list(title = "Group", points = TRUE, columns = 2),
       main = "本周爆发概念数量图",
       xlab = "时间",
       ylab = "涨幅")


"#101010","#FDF7F7" "#ED79F9" "#FFC0CB" "#FFFF00" "#00FF00" "#00FF00" "#0000FF"

