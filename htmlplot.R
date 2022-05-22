

library(Rnvd3)
dat <- reshape2::melt(
  apply(HairEyeColor, c(1, 2), sum), value.name = "Count"
)
head(dat)

# style axis titles with CSS ####
library(htmltools)
CSS <- HTML(
  ".nvd3 .nv-axis.nv-x text.nv-axislabel,
   .nvd3 .nv-axis.nv-y text.nv-axislabel {
     font-size: 2rem;
     fill: red;
  }"
)
widget <- multiBarChart(
  dat, Count ~ Eye, "Hair", palette = "turbo"
)
prependContent(
  widget,
  tags$style(CSS)
)


ibrary(Rnvd3)
dat <- aggregate(breaks ~ wool + tension, data = warpbreaks, mean)
levels(dat[["tension"]]) <- c("Low", "Medium", "High")

hMultiBarChart(
  dat, breaks ~ wool, "tension", yAxisShowMaxMin = TRUE,
  yAxisTitle = "Mean of breaks", yAxisTickFormat = ".01f"
)

# the axis titles are small, let's make them bigger
library(htmltools)
CSS <- HTML(
  ".nvd3 .nv-axis.nv-x text.nv-axislabel,
   .nvd3 .nv-axis.nv-y text.nv-axislabel {
     font-size: 1rem;
  }"
)
prependContent(
  hMultiBarChart(
    dat, breaks ~ wool, "tension", yAxisShowMaxMin = TRUE,
    yAxisTitle = "Mean of breaks", yAxisTickFormat = ".01f"
  ),
  tags$style(CSS)
)



library(Rnvd3)
# in this example we use the tooltip formatters for styling only; we could
# achieve the same result with the help of CSS
dat <- reshape2::melt(
  apply(HairEyeColor, c(1, 2), sum), value.name = "Count"
)
multiBarChart(
  dat, Count ~ Eye, "Hair",
  tooltipFormatters = list(
    value = JS(
      "function(x){",
      "  return '<span style=\"color:red;\">' + x + '</span>';",
      "}"
    ),
    header = JS(
      "function(x){",
      "  return '<span style=\"color:green;\">' + x + '</span>';",
      "}"
    ),
    key = JS(
      "function(x){",
      "  return '<i style=\"color:blue;\">' + x + '</i>';",
      "}"
    )
  )
)

# style axis titles with CSS ####
library(htmltools)

CSS <- HTML(
  ".nvd3 .nv-axis.nv-x text.nv-axislabel,
   .nvd3 .nv-axis.nv-y text.nv-axislabel {
     font-size: 2rem;
     fill: red;
  }"
)

widget <- multiBarChart(
  dat, Count ~ Eye, "Hair", palette = "turbo"
)
prependContent(
  widget,
  tags$style(CSS)
)
