library(plotly)
library(shiny)
library(shinythemes)
library(dplyr)
library(tidyr)
library(data.table)
library(lubridate)
library(dplyr)
library(ggrepel)
ui <- fluidPage(
  # Set theme
  theme = shinytheme("superhero"),
  # Some help text
  h2("探索事件触发引下连锁反应的数据可视化"),
  # Vertical space
  tags$hr(),
  
  # Window length selector
  selectInput("window", label = "Select Window Length", choices = c(10, 20, 30, 60, 90), selected = 10),
  
  # Plotly Chart Area
  fluidRow(
    column(6, plotlyOutput(outputId = "timeseries", height = "600px")),
    column(6, plotlyOutput(outputId = "correlation", height = "600px"))),
  fluidRow(verbatimTextOutput("click")),
  
  tags$hr(),
  tags$blockquote("随着时间的变化相关相关关系也会发生变化")
)


server <- function(input, output){
    
    # Read data
    #stockdata <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/stockdata.csv")
    stockdata<-fread("D:\\2021\\STOCKS.csv")
    colnames(stockdata)<-c("V1","Date","FuJianShuiNI","JiDongShuiNi","MaoHuaShiHua","SFSN")
    # Create dates
    stockdata$Date <- ymd(stockdata$Date)
    stockdata<-stockdata%>%select("Date","FuJianShuiNI","JiDongShuiNi","MaoHuaShiHua","SFSN")
    # Reshape
    ds <- reshape2::melt(stockdata, id = "Date")
    ds <- filter(ds, variable != "SFSN")
    ds$value<-as.numeric(ds$value)
    # Set some colors
    
    # Plot time series chart 
    output$timeseries <- renderPlotly({
      p <- plot_ly(source = "source") %>% 
        add_lines(data = ds, x = ~Date, y = ~value, color = ~variable, mode = "lines", line = list(width = 3))
      
      # Add SP500
      p <- p %>%
        add_lines(data = stockdata, x = ~Date, y = ~SFSN, mode = "lines", yaxis = "y2", name = "上峰水泥", opacity = 0.3,
                  line = list(width = 5)) %>% 
        layout(title = "今日领涨水泥板块",
               xaxis = list(title = "Dates", gridcolor = "#bfbfbf", domain = c(0, 0.98)),
               yaxis = list(title = "Stock Price", gridcolor = "#bfbfbf"), 
               plot_bgcolor = "burlywood",
               paper_bgcolor = element_rect(fill = 'khaki'), 
               yaxis2 = list(title = "今日领涨板块", side = "right", overlaying = "y"))
      p
    })
    
    # Coupled hover event
    output$correlation <- renderPlotly({
      
      # Read in hover data
      eventdata <- event_data("plotly_hover", source = "source")
      validate(need(!is.null(eventdata),"Hover over the time series chart to populate this heatmap"))
      
      # Get point number
      datapoint <- as.numeric(eventdata$pointNumber)[1]
      
      # Get window length
      window <- as.numeric(input$window)
      
      # Show correlation heatmap
      rng <- (datapoint - window):(datapoint + window)
      cormat <- round(cor(stockdata[rng,2:5]),2)
      
      plot_ly(x = rownames(cormat), y = colnames(cormat), z = cormat, type = "heatmap", 
              colors = c("tomato2","springgreen3"))%>% 
        layout(title = "Correlation heatmap",
               xaxis = list(title = ""),
               yaxis = list(title = ""))
      
    })
    
  }


shinyApp(ui, server)
