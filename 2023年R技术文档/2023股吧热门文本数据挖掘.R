
##########################################################################
#
#                  股吧热门数据可视化
#
#
###########################################################################
library(Tushare)
library(lares)
library(jiebaR)
library(jiebaRD)
library(wordcloud2)
library(text2vec)
library(NLP)
library(textshape)
library(dtt)
library(tmcn)
library(tm)
library(syuzhet)
library(stringi)
library(stringr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(dbplyr)
library(purrr)
library(lubridate)
pro <-pro_api(token ='fe8102bf83f5f83f6608aa46fa5e985c534c227786236a1192e5fd55')
#stocks_names<-pro(api_name = 'stock_basic')
today<-str_remove(str_remove(Sys.Date(), "-"),"-")

stocks_names<-pro(api_name = 'stock_basic',fields='ts_code,symbol,name,area,industry,list_date')
dataprices = pro(api_name="daily",trade_date=today)
ts_name<-stocks_names%>%select(ts_code)
ts_codes<-ts_name$ts_code
df_all<-dataprices%>%left_join(stocks_names)
df_all$list_date<-ymd(df_all$list_date)


yesterday1<-str_remove(str_remove(Sys.Date()-1, "-"),"-")
yesterday2<-str_remove(str_remove(Sys.Date()-2, "-"),"-")
yesterday3<-str_remove(str_remove(Sys.Date()-3, "-"),"-")

yesterday4<-str_remove(str_remove(Sys.Date()-6, "-"),"-")
dataprices = pro(api_name="daily",trade_date=today)%>%filter(pct_chg>1)%>%left_join(stocks_names)
dataprices1 <- pro(api_name="daily",trade_date=yesterday1)%>%filter(pct_chg>1)%>%left_join(stocks_names)
dataprices2 <- pro(api_name="daily",trade_date=yesterday2)%>%filter(pct_chg>1)%>%left_join(stocks_names)
dataprices3 <- pro(api_name="daily",trade_date=yesterday3)%>%filter(pct_chg>1)%>%left_join(stocks_names)


text<-print(list(dataprices$name,dataprices1$name,dataprices2$name,dataprices3$name))
library(tmcn)
TDM<-createTDM(text, language = c("zh", "en"), tokenize = NULL, removePunctuation = TRUE, 
          removeNumbers = TRUE, removeStopwords = TRUE)

demoFreq<-createWordFreq(TDM, onlyCN = TRUE, nosymbol = TRUE, stopwords = NULL,
               useStopDic = FALSE)
demoFreq<-demoFreq%>%filter(freq>2)
library("d3wordcloud")
d3wordcloud(demoFreq$word, demoFreq$freq, size.scale = "linear")
d3wordcloud(demoFreq$word, demoFreq$freq ,size.scale = "log",
            rotate.min = 15, rotate.max = 30,rangesizefont = c(10, 20))




library(showtext)
showtext_auto(enable = TRUE)
font_add('Songti', 'Songti.ttc')

library(wordcloud2)

#color = ifelse(demoFreq[, 2] > 4, 'red', 'skyblue'))
wordcloud2(demoFreq)


wordcloud(demoFreq$word,demoFreq$freq,scale=c(4,.5),min.freq=3,max.words=Inf,
          random.order=TRUE, random.color=FALSE, rot.per=.1,
          colors=ifelse(demoFreq$freq>3, 'red', 'skyblue'),use.r.layout=FALSE,
          fixed.asp=TRUE)





d3wordcloud(demoFreq$word, demoFreq$freq)

d3wordcloud(demoFreq$word, demoFreq$freq, 
            colors = c("#FF0000", "#00FF00", "#0000FF"))

library(viridis)
library(viridisLite)

d3wordcloud(demoFreq$word, demoFreq$freq, 
            font = "Erica One", padding = 5)

d3wordcloud(demoFreq$word, demoFreq$freq,spiral = "archimedean")

d3wordcloud(demoFreq$word, demoFreq$freq,spiral = "rectangular")

d3wordcloud(demoFreq$word, demoFreq$freq, size.scale = "linear")

d3wordcloud(demoFreq$word, demoFreq$freq ,size.scale = "log",
            rotate.min = 15, rotate.max = 30,rangesizefont = c(10, 20))



