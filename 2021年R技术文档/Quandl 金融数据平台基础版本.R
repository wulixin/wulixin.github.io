

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

({
  today<-ymd(Sys.Date())
  pro <-pro_api(token ='fe8102bf83f5f83f6608aa46fa5e985c534c227786236a1192e5fd55')
  #stock_code<-'603053.SH'
  #stock_prices<-pro(api_name = 'daily', ts_code=stock_code,start_date= today-years(6))
  stocks_names<-pro(api_name = 'stock_basic',fields='ts_code,symbol,name,area,industry,list_date')
  #个股数据
  STOCKprices = pro(api_name="daily",trade_date='20211228')
  df_all<-STOCKprices%>%left_join(stocks_names)
  df_all$list_date<-ymd(df_all$list_date)
  #场内基金数据  
  ###1408
  funds_names<-pro(api_name = 'fund_basic',market="E")
  funds_names$found_date<-ymd(funds_names$found_date)
  df_ETF=pro(api_name='fund_daily',trade_date='20211228')
  #获取可转债基础信息列表
  bonds_names = pro(api_name="cb_basic")
  df_bonds = pro(api_name='cb_daily',trade_date='20211228')
  
  longhu<-pro(api_name='top_inst', trade_date='20211228')
  
  ETFdf<-funds_names%>%left_join(df_ETF)%>%
    arrange(desc(pct_chg))%>%
    select(ts_code,name,invest_type,close,pct_chg,vol,amount,benchmark,found_date)
  colnames(ETFdf)<-c("代码","名称","投资类型","收盘价","涨跌幅","交易量","规模","衡量尺度","上市日期")
  
  #获取可转债基础信息列表
  BONDdf<-df_bonds%>%left_join(bonds_names)%>%
    select(ts_code,bond_short_name,stk_short_name,pct_chg,maturity_date,vol,close)%>%
    arrange(desc(pct_chg))
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
  DF<-longhu%>%
    left_join(df_all,by=c("ts_code"))%>%
    select(ts_code,name,industry,close,pct_chg)%>%
    group_by('name')
  
  remen<-data.frame(table(DF$name,DF$pct_chg,DF$industry))%>%
    filter(Freq>2)%>%
    arrange(desc(Freq))
  
  ##hk
  stocks_hknames<-pro(api_name = 'hk_basic')
  
  stocks_hknames<-stocks_hknames%>%
    select(ts_code,name,cn_spell,market,list_date,trade_unit)
  ts_name<-stocks_hknames%>%
    select(ts_code)
  ts_codes<-ts_name$ts_code
  datahkprices<-ts_codes%>%
    pro(api_name = 'hk_daily',start_date='20210101', end_date='20211129')
  hkdf<-datahkprices%>%
    left_join(stocks_hknames)
  
  
ui<-dashboardPage(
    skin = "red",
    dashboardHeader(title = "Quandl金融数据平台",titleWidth = 150,
                    dropdownMenuOutput("messageMenu")),
    dashboardSidebar(
      width = 180,
      sidebarMenu(
        menuItem("涨幅排行榜", tabName = "RESTOCKS", icon = icon("th")),
        menuItem("机构参与牛股",tabName=" JIGOU",icon=icon("th")),
        menuItem("次新黄金坑掘金",tabName="CIXIN",icon=icon("th")),
        menuItem("T+0潜力可转债",tabName="BONDS",icon=icon("th")),
        menuItem("场内基金排名", tabName = "ETF", icon = icon("th")),
        menuItem("港股价值投资", tabName = "hk", icon = icon("th"))
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
        
        tabItem(tabName="BONDS",
                fluidRow(column(10, DTOutput('tbl4')))),
        
        tabItem(tabName="ETF",
                fluidRow(column(10, DTOutput('tbl5'))))，
        
        tabItem(tabName="hk",
                fluidRow(column(10, DTOutput('tbl6')))) )) )
     





  server1<- function(input, output,session) {
  
   output$tbl1<-renderDT({
      datatable(STOCKdf,filter = 'top', options = list(pageLength = 25, autoWidth = TRUE))
    })
  
  output$tbl2<-renderDT({
    datatable(remen,filter = 'top', options = list(pageLength = 25, autoWidth = TRUE)) 
    })
  
  output$tbl3<-renderDT({
    datatable(cixindf,filter = 'top', options = list(pageLength = 25, autoWidth = TRUE))
    })
  
  output$tbl4<-renderDT({
    datatable(BONDdf,filter = 'top', options = list(pageLength = 25, autoWidth = TRUE)) 
    })
  
  output$tbl5<-renderDT({
    datatable(ETFdf,filter = 'top', options = list(pageLength = 25, autoWidth = TRUE))
    })
  
  output$tbl6<-renderDT({
    datatable(hkdf,filter = 'top', options = list(pageLength = 25, autoWidth = TRUE))
    })
  
  output$messageMenu <- renderMenu({
    messageData<-"WELCOME WU LI XIN WORLD!"
    msgs <- apply(messageData, 1, function(row) {
      messageItem(from = row[["from"]], message = row[["message"]])
    })
    dropdownMenu(type = "messages", .list = msgs)
    }) 
  }
})

library(shiny)
shinyApp(ui, server1)


