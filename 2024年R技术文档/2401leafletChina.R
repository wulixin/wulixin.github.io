



leafletGeo = function(mapName,
                      dat = NULL,
                      namevar = NULL,
                      valuevar = NULL){
  countries <- readGeoLocal(mapName)
  countries$popup = countries$name
  # if(.Platform$OS.type == "windows"){
  #   countries$popup = encodingSolution(countries$popup)
  # }
  
  if(is.null(dat)){
    return(
      countries
    )
  }else{
    if(class(dat) != 'data.frame'){
      stop("dat should be a data.frame")
    }
    if(is.null(namevar)){
      name = dat[, 1] %>% toLabel()
    }else{
      name = evalFormula(namevar,dat)
    }
    name = as.character(name) %>% toLabel()
    
    if(is.null(valuevar)){
      value = dat[, 2]
    }else{
      value = evalFormula(valuevar,dat)
    }
    countries <- readGeoLocal(mapName)
    countries$label = toLabel(countries$name)
    index = sapply(countries$label,function(x) which(name==x)[1])
    countries$value = value[index]
    countries$popup = countries$name
    return(
      countries
    )
  }
}



amap = function(map,
                attribution = '&copy; <a href="http://amap.com">amap.com</a >',
                ...){
  leaflet::addTiles(map,
                    'http://webrd02.is.autonavi.com/appmaptile?lang=zh_cn&size=1&scale=1&style=8&x={x}&y={y}&z={z}',
                    leaflet::tileOptions(tileSize=256,  minZoom=3,maxZoom=17),
                    attribution = attribution,
                    ...)
}


bmap = function(map,
                attribution = '&copy; <a href="http://map.baidu.com">baidu.map.com</a >',
                ...){
  leaflet::addTiles(map,
                    'http://webrd02.is.autonavi.com/appmaptile?lang=zh_cn&size=1&scale=1&style=8&x={x}&y={y}&z={z}',
                    leaflet::tileOptions(tileSize=256,  minZoom=3,maxZoom=17),
                    attribution = attribution,
                    ...)
}


## regionNames 

regionNames = function(mapName=NULL){
  # city = 'china'
  if(is.null(mapName)){
    print(leafletcn.map.names$name)
    cat("\nThese are valid mapName~\n")
    return("NULL")
  }
  
  ## read from local files
  countries <- readGeoLocal(mapName)
  
  ## convert Encoding in Windows
  if(.Platform$OS.type == "windows"){
    encodingSolution(countries$name)
  }
  
  countries$name
}



## read geo shap 

read.geoShape = function(txt){
  
  raw = jsonlite::fromJSON(txt)
  # Polygons part
  
  ployList = lapply(raw$features$geometry$coordinates, function(x){
    if(class(x) == "array"){
      a = as.vector(x)
      dim(a) = c(length(a)/2,2)
      # if(length(dim(x))==3){
      #   Sr = (sp::Polygon(x[1,,])
      # }else{
      #   Sr = sp::Polygon(x)
      # }
      Sr = sp::Polygon(a)
      Sp = sp::Polygons(list(Sr), "namei")
      return(Sp)
    }else{
      if(any(sapply(x, class) == 'list')){
        whilei = 0
        while(any(sapply(x, class) == 'list')){
          whilei = whilei+1
          if(whilei==10)
            break
          index = which(sapply(x, class)=='list')[1]
          x = append(x[-index],x[[index]])
        }
      }
      
      Sr = lapply(x,function(y){
        
        a = as.vector(y)
        dim(a) = c(length(a)/2,2)
        return(sp::Polygon(a))
        # if(length(dim(y))==3){
        #   return(sp::Polygon(y[1,,]))
        # }else{
        #   return(sp::Polygon(y))
        # }
      })
      Sp = sp::Polygons(Sr, "namei")
      return(Sp)
    }
  })
  
  for(i in 1:length(ployList)){
    ployList[[i]]@ID = as.character(i)
  }
  ployPart =  sp::SpatialPolygons(ployList, 1:length(ployList))
  
  # dat part
  datPart = raw$features$properties
  if(any(sapply(datPart, class)=='list')){
    index = which(sapply(datPart, class)=='list')
    outlist = lapply(index, function(x){
      out = do.call(rbind,datPart[,x])
      colnames(out) = paste0(names(datPart)[x], 1:dim(out)[2])
      return(out)
    })
    datPart = cbind(datPart, do.call(cbind, outlist))
    datPart = datPart[, -index]
  }
  rownames(datPart) = row.names(ployPart)
  
  
  ex_1.7 = sp::SpatialPolygonsDataFrame(ployPart,
                                        datPart)
  return(ex_1.7)
}

###demo function 

demomap = function(mapName){
  # if(.Platform$OS.type == "windows"){
  #   locate = Sys.getlocale("LC_CTYPE")
  #   Sys.setlocale("LC_CTYPE","eng")
  # }
  
  countries <- readGeoLocal(mapName)
  countries$popup = countries$name
  # countries$color = rainbow(length(countries$name))
  ## Encoding
  # Sys.setlocale("LC_CTYPE","eng")
  # if(.Platform$OS.type == "windows"){
  #   countries$popup = encodingSolution(countries$popup)
  # }
  
  map <- leaflet::leaflet(countries)
  output = map %>% leaflet::addTiles() %>%
    leaflet::addPolygons(stroke = T,
                         smoothFactor = 0.2,
                         fillOpacity = 0.2,
                         # fillColor = ~color,
                         weight = 1,
                         popup = ~htmltools::htmlEscape(popup))
  #
  #   if(.Platform$OS.type == "windows"){
  #     Sys.setlocale("LC_CTYPE",locate)
  #   }
  
  return(output)
}


#leafletGeo这个函数可以把一个数据框和一个地图组合在一起, 方便用leaflet调用, 
#其中名字的 变量为name, 数值的变量为value~
  

library(leaflet)
library(leaflet.providers)


regionNames("太原")

demomap("台湾")

dat = data.frame(name = regionNames("china"),
                 value = runif(34))
geojsonMap(dat,"china")

#.install.packages('rgeoda')
#.libPaths() 查文件位置
library(highcharter)
mpdta <- download_map_data("https://code.highcharts.com/mapdata/countries/cn/custom/cn-all-sar-taiwan.geo.json")

mpdta <- download_map_data("https://code.highcharts.com/mapdata/countries/cn/custom/cn-all-sar-taiwan.js")

mpdata<-get_data_from_map(mpdta)
head(mpdata)
mpdata$country<-'China'

write.csv(mpdata,"//Users//wulixin//Desktop//Chinamap.csv")

write.csv(leafletcn.map.names,"//Users//wulixin//Desktop//leafletcn.map.names.csv")



library(dplyr)
data("USArrests", package = "datasets")
USArrests <- mutate(USArrests, "woe-name" = rownames(USArrests))

hcmap(
   map = "countries/us/us-all", data = USArrests,
   joinBy = "woe-name", value = "UrbanPop", name = "Urban Population")

hcmap(map = "countries/us/us-all", data = USArrests,
    joinBy = "woe-name", value = "UrbanPop", name = "Urban Population",
   download_map_data = FALSE)
  

# 叠加一个高德地图
leaflet() %>%
  amap() %>%  
  addMarkers(lng=111.77, lat=37.14, popup="The birthplace of COS")


leaflet() %>%
  bmap() %>%  
  addMarkers(lng=111.77, lat=37.14, popup="The birthplace of COS")

#install.packages("geojsonio") # 如果未安装则进行安装
library(geojsonio)
china_data <- geojsonio::geojson_read('https://raw.githubusercontent.com/johan/world.geo.json/master/countries/CN.geo.json')



# read.geoShape这个函数可以把一个geojson格式的数据读取为一个
# SpatialPolygonsDataFrame对象, 方便sp或者leaflet包中的调用.
library(leaflet)
if(require(sp)){
  filePath = system.file("geojson/china.json",package = "leafletCN")
  map = read.geoShape(filePath)
  plot(map)
}

## leafletGeo这个函数可以把一个数据框和一个地图组合在一起, 
#  方便用leaflet调用, 其中名字的 变量为name, 数值的变量为value~

if(require(leaflet)){
  dat = data.frame(regionNames("china"),
                   runif(34))
  map = leafletGeo("china", dat)
  
  pal <- colorNumeric(
    palette = "Blues",
    domain = map$value)
  
  leaflet(map) %>% addTiles() %>%
    addPolygons(stroke = TRUE,
                smoothFactor = 1,
                fillOpacity = 0.7,
                weight = 1,
                color = ~pal(value),
                popup = ~htmltools::htmlEscape(popup)
    ) %>%
    addLegend("bottomright", pal = pal, values = ~value,
              title = "legendTitle",
              labFormat = leaflet::labelFormat(prefix = ""),
              opacity = 1)
}