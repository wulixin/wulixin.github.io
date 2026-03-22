

df = pro(api_name="cb_basic")

library(leaflet)
library(maps)
library(sp)

m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=130, lat=26, popup="The birthplace of R")
m  # Print the map


library(maps)
mapStates = map("state", fill = TRUE, plot = FALSE)
leaflet(data = mapStates) %>% addTiles() %>%
  addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)



m = leaflet() %>% addTiles()
df = data.frame(
  lat = rnorm(100),
  lng = rnorm(100),
  size = runif(100, 5, 20),
  color = sample(colors(), 100)
)
m = leaflet(df) %>% addTiles()
m %>% addCircleMarkers(radius = ~size, color = ~color, fill = FALSE)
m %>% addCircleMarkers(radius = runif(100, 4, 10), color = c('red'))



library(leaflet)

m <- leaflet() %>% addTiles()

m %>%
  # Central Park
  fitBounds(-73.9, 40.75, -73.95,40.8) %>%
  addMeasure()


m %>%
  # Berlin, Germany
  fitBounds(13.76134, 52.675499, 13.0884, 52.33812) %>%
  addMeasure(
    position = "bottomleft",
    primaryLengthUnit = "meters",
    primaryAreaUnit = "sqmeters",
    activeColor = "#3D535D",
    completedColor = "#7D4479")



library(leaflet)

m <- leaflet() %>% addTiles() %>% setView(0,0,2)

m %>% addGraticule()

m %>% addGraticule(interval = 40, style = list(color = "#FF0000", weight = 1))



library(leaflet)
l <- leaflet() %>% setView(0,0,3)

l %>%
  addProviderTiles(providers$Esri.WorldStreetMap) %>%
  addMiniMap()


l %>%
  addProviderTiles(providers$Esri.WorldStreetMap) %>%
  addMiniMap(
    tiles = providers$Esri.WorldStreetMap,
    toggleDisplay = TRUE)


library(leaflet)
library(htmltools)
library(htmlwidgets)

leaflet() %>% addTiles() %>%
  addEasyButton(easyButton(
    icon="fa-globe", title="Zoom to Level 1",
    onClick=JS("function(btn, map){ map.setZoom(1); }"))) %>%
  addEasyButton(easyButton(
    icon="fa-crosshairs", title="Locate Me",
    onClick=JS("function(btn, map){ map.locate({setView: true}); }")))




leaflet() %>% addTiles() %>%
  addMarkers(data=quakes,
             clusterOptions = markerClusterOptions(),
             clusterId = "quakesCluster")
  


l <- leaflet() %>% setView(0,0,3)

esri <- grep("^Esri", providers, value = TRUE)

for (provider in esri) {
  l <- l %>% addProviderTiles(provider, group = provider)
}

l %>%
  addLayersControl(baseGroups = names(esri),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addMiniMap(tiles = esri[[1]], toggleDisplay = TRUE,
             position = "bottomleft") %>%
  htmlwidgets::onRender("
    function(el, x) {
      var myMap = this;
      myMap.on('baselayerchange',
        function (e) {
          myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
        })
    }")




## 查看每个分公司的规模/资产规模/客户数量/客户资产

cities <- read.csv(textConnection("
City,Lat,Long,Pop
Boston,42.3601,-71.0589,645966
Hartford,41.7627,-72.6743,125017
New York City,40.7127,-74.0059,8406000
Philadelphia,39.9500,-75.1667,1553000
Pittsburgh,40.4397,-79.9764,305841
Providence,41.8236,-71.4222,177994
"))

leaflet(cities) %>% addTiles() %>%
  addCircles(lng = ~Long, lat = ~Lat, weight = 1,
             radius = ~sqrt(Pop) * 30, popup = ~City
  )


## 区间

leaflet() %>% addTiles() %>%
  addRectangles(
    lng1=-118.456554, lat1=34.078039,
    lng2=-118.436383, lat2=34.062717,
    fillColor = "transparent"
  )



# Or use the geojsonio equivalent:
# # From http://eric.clst.org/Stuff/USGeoJSON and
# https://en.wikipedia.org/wiki/List_of_United_States_counties_and_county_equivalents

#nycounties <- rgdal::readOGR("https://rstudio.github.io/leaflet/json/nycounties.geojson")

library(sf)

nycounties <- geojsonio::geojson_read("https://rstudio.github.io/leaflet/json/nycounties.geojson", what = "sp") 

pal <- colorNumeric("viridis", NULL)

leaflet(nycounties) %>%
  addTiles() %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
              fillColor = ~pal(log10(pop)),
              label = ~paste0(county, ": ", formatC(pop, big.mark = ","))) %>%
  addLegend(pal = pal, values = ~log10(pop), opacity = 1.0,
            labFormat = labelFormat(transform = function(x) round(10^x)))



#####################
leaflet() %>%
  addTiles() %>%
  addMarkers(data = coffee_shops, group = "Food & Drink") %>%
  addMarkers(data = restaurants, group = "Food & Drink") %>%
  addMarkers(data = restrooms, group = "Restrooms")


outline <- quakes[chull(quakes$long, quakes$lat),]

map <- leaflet(quakes) %>%
  # Base groups
  addTiles(group = "OSM (default)") %>%
  addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  # Overlay groups
  addCircles(~long, ~lat, ~10^mag/5, stroke = F, group = "Quakes") %>%
  addPolygons(data = outline, lng = ~long, lat = ~lat,
              fill = F, weight = 2, color = "#FFFFCC", group = "Outline") %>%
  # Layers control
  addLayersControl(
    baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
    overlayGroups = c("Quakes", "Outline"),
    options = layersControlOptions(collapsed = FALSE)
  )
map

map %>% hideGroup("Outline")

##############################
quakes <- quakes %>%
  dplyr::mutate(mag.level = cut(mag,c(3,4,5,6),
                                labels = c('>3 & <=4', '>4 & <=5', '>5 & <=6')))

quakes.df <- split(quakes, quakes$mag.level)

l <- leaflet() %>% addTiles()

names(quakes.df) %>%
  purrr::walk( function(df) {
    l <<- l %>%
      addMarkers(data=quakes.df[[df]],
                 lng=~long, lat=~lat,
                 label=~as.character(mag),
                 popup=~as.character(mag),
                 group = df,
                 clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),
                 labelOptions = labelOptions(noHide = F,
                                             direction = 'auto'))
  })

l %>%
  addLayersControl(
    overlayGroups = names(quakes.df),
    options = layersControlOptions(collapsed = FALSE)
  )
