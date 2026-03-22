

################################################################################
##
## 反欺诈算法针对特点板块的活泼性个股进行研究
##
##
################################################################################

industrys<- c("银行","证券","期货","保险")
unique(df_all$industry)

cixin_2023<- df_all%>%
  filter(industry %in% industrys)%>%
  filter(list_date>ymd('2023-01-01'))%>%
  select(ts_code,name,industry,CY_FaZhanQS,CY_JiShuXJ,CY_ChanPinGQ,CY_ChanYeType,market,close,pct_chg,vol,change,list_date,area)%>%
  mutate(Year=year(list_date),Month=month(list_date),Time=as.numeric(today()-list_date))%>%
  arrange(desc(pct_chg))

cixin_2022<- df_all%>%
  filter(industry %in% industrys)%>%
  filter(list_date>ymd('2022-01-01') &list_date<ymd('2023-01-01'))%>%
  select(ts_code,name,industry,CY_FaZhanQS,CY_JiShuXJ,CY_ChanPinGQ,CY_ChanYeType,market,close,pct_chg,vol,change,list_date,area)%>%
  mutate(Year=year(list_date),Month=month(list_date),Time=as.numeric(today()-list_date))%>%
  arrange(desc(pct_chg))

cixin_2021<- df_all%>%
  filter(industry %in% industrys)%>%
  filter(list_date>ymd('2021-01-01') &list_date<ymd('2022-01-01'))%>%
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

cixin<-cixin_2023
cixin$change_type<-ifelse(cixin$pct_chg>4,"表现很好",
                          ifelse(cixin$pct_chg>1,"表现一般","表现不佳"))



today_best<-cixin
cixin_2021$change_type<-ifelse(cixin_2021$pct_chg>4,"表现很好",
                               ifelse(cixin_2021$pct_chg>1,"表现一般","表现不佳"))

cixin_2022$change_type<-ifelse(cixin_2022$pct_chg>4,"表现很好",
                               ifelse(cixin_2022$pct_chg>1,"表现一般","表现不佳"))



#%>%
#  filter(CY_FaZhanQS=="朝阳产业" & CY_JiShuXJ=="传统行业")%>%
#  filter(market=="科创板" & CY_ChanYeType=="知识技术密集型")
colnames(today_best)
colnames(today_best)<-c("代码","公司名称","行业","产业趋势","产业技术","产品类型",
                        "产业类型","板块","收盘价","涨跌幅","交易量","价格变化","上市时间","地区",
                        "上市年份","上市月份","时间","表现情况")
colnames(cixin_2022)<-c("代码","公司名称","行业","产业趋势","产业技术","产品类型",
                        "产业类型","板块","收盘价","涨跌幅","交易量","价格变化","上市时间","地区",
                        "上市年份","上市月份","时间","表现情况")
colnames(cixin_2021)<-c("代码","公司名称","行业","产业趋势","产业技术","产品类型",
                        "产业类型","板块","收盘价","涨跌幅","交易量","价格变化","上市时间","地区",
                        "上市年份","上市月份","时间","表现情况")



head(cixin)


stocks_name<-df_all%>%filter(industry %in% industrys)

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

library(highcharter)
library(purrr)
library(tidyr)
library(tidyverse)
library(dplyr)
china_stocksdata<-china_stocks%>%
  unnest(stock.prices)%>%
  mutate(Time=as.numeric(today()-ymd(list_date)))

############每个行业龙头个股

NEWD<-china_stocksdata%>%group_by(name)%>%
  mutate(return=(Close-Open)/Open)%>%
  ungroup(name)

NEWD$return_type<-cut(NEWD$return, breaks=c(-0.1,-0.05,-0.015,0,0.03,0.06,0.08,0.12), 
                      labels =c("大跌","中跌","微跌","小涨","中涨","大涨","游资偏好"))


library(janitor)
library(lares)
library(DT)
library(stringi)
library(stringr)
COUNTC<-NEWD%>%
  filter(Date>ymd(Sys.Date()-300))%>%
  select(name,return_type)%>%left_join(df_all%>%select(name,ts_code))


dfc<-COUNTC %>%
  tabyl(name, return_type)%>%
  select(name,大涨,游资偏好)%>%
  arrange(desc(游资偏好))

colnames(dfc)<-c("公司名称","爆发力","游资偏好")

datatable(dfc)

dp<-china_stocks %>%
  unnest(stock.prices)%>%
  arrange(desc(Date))%>%
  ggplot(aes(x=desc(Date),y=as.numeric(Close)),color=name)+
  geom_line()+
  facet_wrap(~name,ncol=4,scales = "free_y")+
  labs(x="日期",y="价格",title="全局纵览走势图")+
  theme_avatar(title.font = "Slayer",
             text.font = "Slayer",
             title.size = 14) 

ggplotly(dp)


##这幅图很牛，之后可以做其它用途
china_stocks %>%
  unnest(stock.prices)%>%
  select(Date,Close,name)%>%
  group_by(name) %>%
  do(p = plot_ly(., x = ~desc(ymd(Date)), y = ~Close,name=~name)%>%add_lines) %>%
  subplot(nrows =5, shareX = TRUE)


########################################反欺诈算法区域
library(dplyr)
library(timetk)

anoma_data<-china_stocks %>%
  unnest(stock.prices)%>%
  select(Date,vol,name)%>%
  group_by(name) %>%
  tk_anomaly_diagnostics(Date, vol)%>%
  filter(Date>ymd(today()-3))

datatable(anoma_data)
anoma_name<-anoma_data%>%filter(anomaly=="Yes")

anoma_data%>%filter(anomaly=="Yes")%>%select(name,anomaly,Date)%>%knitr::kable()

head(df_all)
anoma_data%>%
  filter(anomaly=="Yes")%>%
  select(name,anomaly,Date)%>%
  left_join(df_all)%>%
  select(name,anomaly,Date,close,pct_chg)%>%
  arrange(desc(pct_chg))%>%
  knitr::kable()

anoma_namedata<-anoma_data%>%
  filter(anomaly=="Yes")%>%
  select(name,anomaly,Date)%>%
  left_join(df_all)%>%
  select(name,anomaly,Date,close,pct_chg)%>%
  arrange(desc(pct_chg))


##################################   针对某个板块异常特定的走势图

china_stocks %>%
  unnest(stock.prices)%>%
  select(Date,Close,name)%>%
  group_by(name) %>%
  do(p = plot_ly(., x = ~desc(ymd(Date)), y = ~Close,name=~name)%>%
       add_lines) %>%
  subplot(nrows =3, shareX = TRUE)%>%
  layout(title = "天网作战系统之全局走势图")


#################################异常数据表格

library(DT)
colnames(anoma_namedata)<-c('公司','是否异常','时间','收盘价','涨跌幅')
datatable(anoma_namedata,caption = htmltools::tags$caption(
  style = 'caption-side: top; text-align: center;',
  '反欺诈表: ', htmltools::em('主力异常行为数据')),extensions = 'Buttons', 
  options = list(dom = 'Bfrtip',pageLength = 20,
                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),initComplete = JS(
                   "function(settings, json) {",
                   "$(this.api().table().header()).css({'background-color': 'lightpink', 'color': 'brown'});",
                   "$(this.api().table().body()).css({'background-color': 'gray', 'color': 'red'});",
                   "}"),searchHighlight = TRUE, filter = 'top'
  ))%>%
  formatStyle('涨跌幅',
              color = styleInterval(4, c('red', 'brown')),
              backgroundColor = styleInterval(2, c('yellow', 'lightblue')))%>%
  formatStyle(
    '是否异常',
    transform = 'rotateX(15deg) rotateY(1deg) rotateZ(1deg)',
    backgroundColor = styleEqual(
      unique(anoma_namedata$是否异常), c('lightgreen')
    ) )


#'background-color': '#000', 'color': '#fff'
plot_ly(anoma_namedata) %>% 
  add_table()%>%
  layout(title='反欺诈算法数据')

library(dplyr)
library(purrr)

library(timetk)

china_data<-china_stocks %>%
  unnest(stock.prices)
#####################################OK的
library(showtext)
showtext_auto(enable=TRUE)

a_plot1<-china_stocks %>%
  unnest(stock.prices)%>%
  select(Date,Volume,name)%>%
  filter(name %in% anoma_namedata$公司)%>%
  group_by(name) %>%
  plot_anomaly_diagnostics(Date, Volume,
                           .message = FALSE,
                           .facet_ncol = 3,
                           .ribbon_alpha = 0.25,
                           .interactive = FALSE)

ggplotly(a_plot1)%>%layout(title = "天网作战地图之反欺诈算法")

colnames(anoma_data)<-c("名称","日期","观察次数","周期性","趋势性","提示信号","季节调整","一级水平","二级水平","异常","一级维度","二级维度")

datatable(anoma_data)

######行业分布
library(proto)
library(ggmap)
library(DT)
library(highcharter)
library(viridisLite)
library(treemap)
library(flexdashboard)
library(RJSONIO)
library(showtext)
font_add("Heiti TC Light", regular = "STHeiti Light.ttc")
showtext_auto()

treemap_data <- china_stocks%>%
  unnest(stock.prices)%>%
  mutate(category = gsub(" ", "-", market),
         subcategory = gsub(" ", "-", industry),
         ratio=mean.log.returns/sd.log.returns,
         Volume=as.numeric(Volume)) %>%
  select(category, subcategory,name,Volume,ratio)%>%
  sample_n(100)


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

pkmn_min <- treemap_data %>% 
  select(category,subcategory,name) %>%
  mutate(category = stringr::str_to_title(category)) %>% 
  mutate(subcategory= ifelse(is.na(subcategory), category, paste(category, "上市时间区间", subcategory))) %>%
  mutate(val = 1)

cols <- pkmn_min %>% 
  count(category,subcategory, sort = TRUE) %>% 
  pull(subcategory) %>% 
  unique()

#######图形一：全局操盘视图  

library(dplyr)

hchart(
  data_to_hierarchical(treemap_data,c("category","subcategory","name"), size=Volume, 
                       colors =rev(viridis(8))),type = "treemap",allowDrillToNode = TRUE,
  tooltip = list(valueDecimals = FALSE),levels = lvl_opts) %>% 
  hc_chart(
    style = list(fontFamily = "Gloria Hallelujah")) %>% 
  hc_title(text = "全局作战操盘地图可视化",
           style = list(fontFamily = "Gloria Hallelujah",fontSize = "30px", color = "red", fontWeight = "bold")) %>% 
  hc_size(height = 750,width = 1350)


#### 相关系数

library(ggplot2)
library(ggcorrplot)
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


#####################Network 数据可视化

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
        width=1350,height=850,main="天网作战地图之社交关系网",
        bg = "black")


graphjs(g, vertex.size=0.25, vertex.shape=colnames(X), edge.alpha=0.5,
        width=1350,height=850,main="天网作战地图之社交关系网",
        bg = "white")

graphjs(g, vertex.size=0.25, vertex.shape=colnames(X), edge.alpha=0.5,
        width=1350,height=850,main="天网作战地图之社交关系网",
        bg = "pink")



#######################heatmap  热图研究  
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
addWorksheet(wb,sheetName="2023新上市")

writeDataTable(wb,sheet="2023新上市",x=today_best,tableStyle = "TableStyleLight2")

cixin_other$change_type<-ifelse(cixin_other$pct_chg>4,"表现很好",
                                ifelse(cixin_other$pct_chg>1,"表现一般","表现不佳"))

addWorksheet(wb,sheetName="投资风向")
plot_bar(cixin_other,by='change_type')+
  theme(text = element_text(family='Kai'))
# plot needs to be showing
insertPlot(wb,"投资风向", width = 12, height = 15, fileType = "png", units = "in")


addWorksheet(wb,sheetName="2021以前上市")

colnames(cixin_other)<-c("代码","公司名称","行业","产业趋势","产业技术","产品类型",
                         "产业类型","板块","收盘价","涨跌幅","交易量","价格变化","上市时间","地区",
                         "上市年份","上市月份","时间","表现情况")

writeDataTable(wb,sheet="2021以前上市",x=cixin_other,tableStyle = "TableStyleLight2")



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

addWorksheet(wb,sheetName="2022上市")

writeDataTable(wb,sheet="2022上市",x=cixin_2022,tableStyle = "TableStyleLight2")




saveWorkbook(wb,"//Users//wulixin//Desktop//投资风向标.xlsx",overwrite=TRUE)
openXL("//Users//wulixin//Desktop//投资风向标.xlsx")

