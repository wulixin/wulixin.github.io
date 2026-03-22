

library(echarts4r)
library(ECharts2Shiny)

cor(mtcars) |>
  e_charts() |>
  e_correlations(
    order = "hclust",
    visual_map = FALSE
  ) |>
  e_visual_map(
    min = -1,
    max = 1
  )

X |>
  e_charts() |>
  e_correlations(
    order = "hclust",
    visual_map = TRUE
  ) |>
  e_visual_map(
    min = -1,
    max = 1
  )

# Changing number of digits for correlation coeffcient
# --------------------------------
a<-ggcorrplot(X,
              hc.order = TRUE, type = "lower",
              outline.color = "white",
              ggtheme = ggplot2::theme_gray,
              colors = c("#6D9EC1", "white", "#E46726"))

ggplotly(a)
