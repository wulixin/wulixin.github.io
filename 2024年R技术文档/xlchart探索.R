

library(tidymodels)

library(xlcharts)

treeData <- data.frame(
  "Type" = c("Maple", "Oak", "Pine"), 
  "LeafColor" = c("Red", "Green", "Green"), 
  "Height" = c(549, 783, 1204)
)

write_xlsx(x = treeData, path = "//Users//wulixin//Desktop//treeData.xlsx")


library(xlcharts)

bar <- data.frame(
  "Number" = c(2,3,4,5,6,7),
  "Batch 1" = c(10,40,50,20,10,50),
  "Batch 2" = c(30,60,70,10,40,30)
)

write_xlsx(bar, path = "//Users//wulixin//Desktop//bar.xlsx")


wb <- load_workbook(filename = "//Users//wulixin//Desktop//bar.xlsx")
ws <- active(wb)

chart1 <- BarChart(
  type = "col",
  style = 10,
  title = "Bar Chart",
  shape = 4
) |>
  y_axis(title = "Test number") |>
  x_axis(title = "Sample length (mm)")

data <- Reference(ws, min_col=2, min_row=1, max_row=7, max_col=3)
cats <- Reference(ws, min_col=1, min_row=2, max_row=7)

chart1 |>
  add_data(data = data, titles_from_data = TRUE) |>
  set_categories(cats)

####定义这个图标的位置

ws |> add_chart(chart1, "A10")


chart2 <- BarChart(
  type = "bar",
  style = 11,
  title = "Horizontal Bar Chart",
  shape = 4) |>
  y_axis(title = 'Test number') |>
  x_axis(title = 'Sample length (mm)')

chart2 |>
  add_data(data = data, titles_from_data = TRUE) |>
  set_categories(cats)

ws |> add_chart(chart2, "G10")


chart3 <- BarChart(
  type = "col",
  style = 12,
  title = "Stacked Chart",
  shape = 4,
  grouping = "stacked",
  overlap = 100
) |>
  y_axis(title = 'Test number') |>
  x_axis(title = 'Sample length (mm)')

chart3 |>
  add_data(data = data, titles_from_data = TRUE) |>
  set_categories(cats)

ws |> add_chart(chart3, "A27")


chart4 <- BarChart(
  type = "bar",
  style = 13,
  title = "Percent Stacked Chart",
  shape = 4,
  grouping = "percentStacked",
  overlap = 100
) |>
  y_axis(title = 'Test number') |>
  x_axis(title = 'Sample length (mm)')

chart4 |>
  add_data(data = data, titles_from_data = TRUE) |>
  set_categories(cats)

ws |> add_chart(chart4, "G27")

save_workbook(wb, "//Users//wulixin//Desktop//bar1.xlsx")

#####
##指定reticulate应该使用的conda环境  这个位置非常棒
reticulate::use_condaenv("r-reticulate-arm64")
library(xlcharts)

bar3d <- data.frame(
  "Fruits" = c("Apples", "Oranges", "Pears"),
  "2013" = c(5, 6, 8),
  "2014" = c(4, 2, 3)
)

write_xlsx(bar3d, path = "//Users//wulixin//Desktop//bar3d.xlsx")

wb <- load_workbook(filename = "//Users//wulixin//Desktop//bar3d.xlsx")
ws <- active(wb)

data <- Reference(ws, min_col=2, min_row=1, max_col=3, max_row=4)
titles <- Reference(ws, min_col=1, min_row=2, max_row=4)

chart <- BarChart3D(
  title = "3D Bar Chart"
) |>
  add_data(data = data, titles_from_data = TRUE) |>
  set_categories(labels = titles)

ws |> add_chart(chart, "E5")

save_workbook(wb, "//Users//wulixin//Desktop//bar3d.xlsx")





