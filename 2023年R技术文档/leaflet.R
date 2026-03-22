

tidyverse - Meta R package for data analysis
dplyr - Data wrangling and manipulation
ggplot2 - Data visualization
tidyr - Data wrangling and manipulation
timetk - Time series analysis
readr - Data import
tidymodels - Machine learning
leaflet - Interactive maps and geospatial analysis
shiny - Interactive web apps


library(ggplot2)
library(leaflet)
library(leaflet.providers)

#install.packages("pak")
pak::pak("tidyverse/ggplot2")


library(leaflet)
library(leafletCN)
##解决国内不能使用的问题
#devtools::install_github("lchiffon/leafletCN")
#devtools::install_github("cran/rgeos") 
#terra
#sf

m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=111.779, lat=37.141, popup="孝义市")
m  # Print the map




###点图
sizes <- expand.grid(size = (0:3) * 2, stroke = (0:3) * 2)
ggplot(sizes, aes(size, stroke, size = size, stroke = stroke)) + 
  geom_abline(slope = -1, intercept = 6, colour = "white", linewidth = 6) + 
  geom_point(shape = 21, fill = "red") +
  scale_size_identity()



leaflet() %>% addTiles() %>% setView(-93.65, 42.0285, zoom = 4) %>%
  addWMSTiles(
    "http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi",
    layers = "nexrad-n0r-900913",
    options = WMSTileOptions(format = "image/png", transparent = TRUE),
    attribution = "Weather data © 2012 IEM Nexrad"
  )

m %>% addProviderTiles(providers$MtbMap) %>%
  addProviderTiles(providers$Stadia.StamenTonerLines,
                   options = providerTileOptions(opacity = 0.35)) %>%
  addProviderTiles(providers$Stadia.StamenTonerLabels)




mapping <- function(data, layout, params) {
  if (is.null(data) || nrow(data) == 0) {
    return(cbind(data, PANEL = integer(0)))
  }
  rbind(
    cbind(data, PANEL = 1L),
    cbind(data, PANEL = 2L)
  )
}


layout <- function(data, params) {
  data.frame(PANEL = c(1L, 2L), SCALE_X = 1L, SCALE_Y = 1L)
}


