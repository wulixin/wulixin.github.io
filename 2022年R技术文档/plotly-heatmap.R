

library(plotly)
fig <- plot_ly(z = volcano, type = "heatmap")

fig

m <- matrix(rnorm(9), nrow = 3, ncol = 3)
fig <- plot_ly(
  x = c("a", "b", "c"), y = c("d", "e", "f"),
  z = m, type = "heatmap"
)

fig


fig <- plot_ly(z = volcano, colors = "Greys", type = "heatmap")

fig


fig <- plot_ly(z = volcano, colors = colorRamp(c("red", "green")), type = "heatmap")

fig


vals <- unique(scales::rescale(c(volcano)))
o <- order(vals, decreasing = FALSE)
cols <- scales::col_numeric("Blues", domain = NULL)(vals)
colz <- setNames(data.frame(vals[o], cols[o]), NULL)
fig <- plot_ly(z = volcano, colorscale = colz, type = "heatmap")

fig
