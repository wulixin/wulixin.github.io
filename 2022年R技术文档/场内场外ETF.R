
library(billboarder)
library(wordcloud2)
library(jiebaR)
library(tmcn)
library(gplots)
library(RColorBrewer)
library(heatmaply)
library(d3heatmap)
library(quantmod)
library(shiny)
library(shinyWidgets)
library(bs4Dash)
library(plotly)
library(shiny)
library(fontawesome)
library(shinyWidgets)
library(bs4Dash)
library(DT)
library(shinydashboard)
library(leaflet)
library(dygraphs)
library(plotly)
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
library(xts)
library(lubridate)


#({
today<-ymd(Sys.Date())
pro <-pro_api(token ='fe8102bf83f5f83f6608aa46fa5e985c534c227786236a1192e5fd55')
##  场外基金数据
Ofunds_names<-pro(api_name = 'fund_basic',market="O")

get_fundsdata<-function(ts_code){
  data<-pro(api_name='fund_nav',ts_code=ts_code,start_date='20211228',end_date='20220112')%>%
    select(ts_code,ann_date,nav_date,unit_nav,accum_nav)
}


##get basket stocks 
get_fund_prices <- function(ticker, return_format = "tibble", ...) {
  # Get stock prices
  fund_prices <- get_fundsdata(ts_code= ticker, ...)
  Date_new<-fund_prices$nav_date
  # Return in xts format if tibble is not specified
  if (return_format == "tibble") {
    fund_prices <- fund_prices%>%
      as_tibble() %>%
      mutate(Date=ymd(ann_date))
  } else {
    fund_prices <- fund_prices
  }
  fund_prices
}

Ofunds_name<-Ofunds_names%>%
  select(ts_code,name,management,fund_type,found_date,issue_amount,invest_type)%>%
  filter(invest_type %in% c("股票型"))

Ofunds_name$found_date<-ymd(Ofunds_name$found_date)

ts_codes<-Ofunds_name$ts_code

Odf<-Ofunds_name%>%
  mutate(prices = Map(function(n)
  {
    #print(n)
    tryCatch(get_fund_prices(n), error = function(e) NA)
  }, ts_codes))%>%
  unnest()%>%
  select(ts_code,name,nav_date,unit_nav,accum_nav,management,fund_type,issue_amount,invest_type)


colnames(Odf)<-c("代码","名称","净值日期","净值","累计净值","管理机构","基金类型","规模","投资类型")

##
stocks_names<-pro(api_name = 'stock_basic',fields='ts_code,symbol,name,area,industry,list_date')
#个股数据
STOCKprices = pro(api_name="daily",trade_date='20220112')
df_all<-STOCKprices%>%left_join(stocks_names)
df_all$list_date<-ymd(df_all$list_date)
df_all$industry<-as.factor(df_all$industry)

#场内基金数据  
###1408
funds_names<-pro(api_name = 'fund_basic',market="E")
funds_names$found_date<-ymd(funds_names$found_date)
df_ETF=pro(api_name='fund_daily',trade_date='20220112')
#获取可转债基础信息列表
bonds_names = pro(api_name="cb_basic")
df_bonds = pro(api_name='cb_daily',trade_date='20220112')
longhu<-pro(api_name='top_inst', trade_date='20220112')

#场外基金数据


ETFdf<-funds_names%>%left_join(df_ETF)%>%
  arrange(desc(pct_chg))%>%
  select(ts_code,name,invest_type,close,pct_chg,vol,amount,benchmark,found_date)
colnames(ETFdf)<-c("代码","名称","投资类型","收盘价","涨跌幅","交易量","规模","衡量尺度","上市日期")

#获取可转债基础信息列表
BONDdf<-df_bonds%>%left_join(bonds_names)%>%
  select(ts_code,bond_short_name,stk_short_name,pct_chg,maturity_date,vol,close)%>%
  arrange(desc(pct_chg))

colnames(BONDdf)<-c("代码","转债名称","股票名称","涨跌幅","到期日期","交易量","收盘价")

##获取股票数据
STOCKdf<-df_all%>%
  filter(close<50 & pct_chg>2)%>%
  select(ts_code,name,industry,close,pct_chg,vol,change)%>%
  arrange(desc(pct_chg))
colnames(STOCKdf)<-c("代码","公司名称","行业","收盘价","涨跌幅","交易量","价格变化")

latest_month<-Sys.Date()-days(350)
cixindf<- df_all%>%
  filter(list_date>ymd(latest_month))%>%
  select(ts_code,name,industry,close,pct_chg,vol,change)%>%
  arrange(desc(pct_chg))

colnames(cixindf)<-c("代码","公司名称","行业","收盘价","涨跌幅","交易量","价格变化")
#龙虎板
DFLH<-longhu%>%
  left_join(df_all,by=c("ts_code"))%>%
  select(ts_code,name,industry,close,pct_chg)%>%
  group_by('name')

JIGOUlonghu<-data.frame(table(DFLH$name,DFLH$pct_chg,DFLH$industry))%>%
  filter(Freq>2)%>%
  arrange(desc(Freq))
##
#获取某一日所有股票
##获取全部可交易股票基础信息
stocks_hknames<-pro(api_name = 'hk_basic')

stocks_hknames<-stocks_hknames%>%
  select(ts_code,name,cn_spell,market,list_date,trade_unit)

stockhk = pro(api_name="hk_daily",trade_date='20220112')
#获取单一股票行情
hkts_name<-stocks_hknames%>%
  select(ts_code)
start_date<-Sys.Date()-years(1)
hkts_codes<-hkts_name$ts_code
datapriceshk<-hkts_codes%>%
  pro(api_name = 'hk_daily',start_date='20210106', end_date='20220112')

hkdf<-datapriceshk%>%
  left_join(stocks_hknames)%>% 
  select(name,close,pct_chg,vol,change)%>%
  arrange(desc(pct_chg))


colnames(hkdf)<-c("公司名称","收盘价","涨跌幅","交易量","价格变化")

########################

ui1 <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "关注我",titleWidth = 150,
                  dropdownMenuOutput("messageMenu")),
  dashboardSidebar(
    width = 180,
    sidebarMenu(
      menuItem("涨幅排行榜", tabName = "RESTOCKS", icon = icon("th")),
      menuItem("机构参与牛股",tabName=" JIGOU",icon=icon("th")),
      menuItem("次新黄金坑掘金",tabName="CIXIN",icon=icon("th")),
      menuItem("T+0潜力可转债",tabName="BONDS",icon=icon("th")),
      menuItem("场内ETF基金", tabName = "ETF", icon = icon("th")),
      menuItem("场外股票基金", tabName = "OETF", icon = icon("th")),
      menuItem("港股涨幅排名", tabName = "hk", icon = icon("th"))
    )),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      tabItem(tabName="RESTOCKS",
              fluidRow(column(10, DTOutput('tbl1')))),
      
      tabItem(tabName="JIGOU",
              fluidRow(column(10, DTOutput('tbl2')))),
      tabItem(tabName="CIXIN",
              fluidRow(column(10, DTOutput('tbl3')))),
      
      tabItem(tabName="hk",
              fluidRow(column(10, DTOutput('tbl6')))),
      tabItem(tabName="OETF",
              fluidRow(column(10, DTOutput('tbl7')))),
      
      tabItem(tabName="BONDS",
              fluidRow(column(10, DTOutput('tbl4')))),
      
      tabItem(tabName="ETF",
              fluidRow(column(10, DTOutput('tbl5')))) ))
  
)
# ))




server <- function(input, output,session) {
  
  output$tbl1<-renderDT({
    datatable(STOCKdf,filter = 'top', options = list(pageLength = 25, autoWidth = TRUE))|>
      formatStyle('收盘价',color = styleInterval(c(100,600), c('green', 'blue', 'red')),
                  fontWeight = styleInterval(12.5, c('normal', 'bold'))) |>
      formatStyle('涨跌幅',color = styleInterval(c(0, 5), c('green', 'blue', 'red')),
                  fontWeight = styleInterval(5, c('normal', 'bold'))) |>
      formatStyle( 
        '交易量',color = styleInterval(c(5000, 100000), c('green', 'blue', 'red')),
        backgroundColor = styleInterval(10000, c('gray', 'yellow'))) 
  })
  
  output$tbl2<-renderDT({
    datatable(JIGOUlonghu,filter = 'top')
  })
  
  output$tbl3<-renderDT({
    datatable(cixindf,filter = 'top', options = list(pageLength = 25, autoWidth = TRUE))|>
      formatStyle('收盘价',color = styleInterval(c(100,600), c('green', 'blue', 'red')),
                  fontWeight = styleInterval(12.5, c('normal', 'bold'))) |>
      formatStyle('涨跌幅',color = styleInterval(c(0, 5), c('green', 'blue', 'red')),
                  fontWeight = styleInterval(5, c('normal', 'bold'))) |>
      formatStyle( 
        '交易量',color = styleInterval(c(5000, 100000), c('green', 'blue', 'red')),
        backgroundColor = styleInterval(10000, c('gray', 'yellow'))) 
  })
  
  output$tbl4<-renderDT({
    datatable(BONDdf,filter = 'top', options = list(pageLength = 25, autoWidth = TRUE))
  })
  
  output$tbl5<-renderDT({
    datatable(ETFdf,filter = 'top', options = list(pageLength = 25, autoWidth = TRUE))|>
      formatStyle('收盘价',color = styleInterval(c(100,600), c('green', 'blue', 'red')),
                  fontWeight = styleInterval(12.5, c('normal', 'bold'))) |>
      formatStyle('涨跌幅',color = styleInterval(c(0, 5), c('green', 'blue', 'red')),
                  fontWeight = styleInterval(5, c('normal', 'bold'))) |>
      formatStyle( 
        '交易量',color = styleInterval(c(5000, 100000), c('green', 'blue', 'red')),
        backgroundColor = styleInterval(10000, c('gray', 'yellow'))) 
  })
  
  output$tbl6<-renderDT({
    datatable(hkdf,filter = 'top', options = list(pageLength = 25, autoWidth = TRUE))|>
      formatStyle('收盘价',color = styleInterval(c(100,600), c('green', 'blue', 'red')),
                  fontWeight = styleInterval(12.5, c('normal', 'bold'))) |>
      formatStyle('涨跌幅',color = styleInterval(c(0, 5), c('green', 'blue', 'red')),
                  fontWeight = styleInterval(5, c('normal', 'bold'))) |>
      formatStyle( 
        '交易量',color = styleInterval(c(5000, 100000), c('green', 'blue', 'red')),
        backgroundColor = styleInterval(10000, c('gray', 'yellow'))) 
  })
  
  output$tbl7<-renderDT({
    datatable(Odf,filter = 'top', options = list(pageLength = 25, autoWidth = TRUE))
  })
  
  output$messageMenu <- renderMenu({
    messageData<-"WELCOME WU LI XIN WORLD!"
    msgs <- apply(messageData, 1, function(row) {
      messageItem(from = row[["from"]], message = row[["message"]])
    })
    dropdownMenu(type = "messages", .list = msgs)
  }) }

shinyApp(ui1, server)


