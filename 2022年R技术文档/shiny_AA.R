
library(shiny)
library(shinybulma)

shinyApp(
  ui = bulmaPage(
    bulmaHero(
      fullheight = TRUE,
      color = "primary",
      bulmaHeroBody(
        bulmaContainer(
          bulmaTitle("Shiny meets Bulma!"),
          bulmaSubtitle("A neat framework for your Shiny apps.")
        )
      )
    ),
    bulmaSection(
      bulmaContainer(
        bulmaTileAncestor(
          bulmaTileParent(
            vertical = TRUE,
            bulmaTileChild(
              bulmaTitle("Tile 1"),
              p("Put some data here"),
              color = "link"
            ),
            bulmaTileChild(
              bulmaTitle("Tile 2"),
              plotOutput("chart"),
              color = "danger"
            )
          ),
          bulmaTileParent(
            vertical = TRUE,
            bulmaTileChild(
              bulmaTitle("Tile 3"),
              p("Put some data here"),
              color = "warning"
            ),
            bulmaTileChild(
              bulmaTitle("Tile 3"),
              ("Put some data here"),
              color = "info"
            )
          )
        )
      )
    )
  ),
  server = function(input, output) {
    output$chart <- renderPlot({
      plot(x = runif(20, 5, 10), y = runif(20, 10, 12))
    })
  }
)