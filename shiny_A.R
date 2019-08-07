

library(devtools)
library(shiny)
library(fullPage)

#demo("fullPage", package = "fullPage")
#demo("pagePiling", package = "fullPage")
#demo("multiPage", package = "fullPage")



ui <- fullPage(
  fullSection(
    menu = "first",
    center = TRUE,
    h1("Callbacks")
  ),
  fullSection(
    menu = "second",
    center = TRUE,
    h3("Slice"),
    verbatimTextOutput("slide")
  )
)

server <- function(input, output){
  
  output$slide <- renderPrint({
    input$slide_origin # returns menu
  })
  
}

shinyApp(ui, server)