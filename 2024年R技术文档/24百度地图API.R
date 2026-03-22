

# 安装并加载相关包
#install.packages("rjson") # 安装 rjson 包（若已经安装则不需要）
library(rjson) # 加载 rjson 包

# 定义 API Key
api_key <- "iytRmcdU2cz4BdPTZEbrp7SQWnl5msC0"

# 设置查询参数
query <- list(
  location = c(111.779,37.141), # 指定位置的经纬度
  zoom = 15, # 指定地图显示的缩放级别
  width = 800, # 指定生成地图的宽度
  height = 600 # 指定生成地图的高度
)


# 发送 HTTP GET 请求获取地图链接
url <- paste0("http://api.map.baidu.com/staticimage?", query$location[1], ",", query$location[2], ":", query$zoom, ":", query$width, ":", query$height, ":normal.png&ak=", api_key)
library(RCurl)
response <- getURLContent(url)

print(response)

library(devtools)
install_github('badbye/baidumap')

library(rjson)
options(remap.ak="iytRmcdU2cz4BdPTZEbrp7SQWnl5msC0")
library(baidumap)
# 定义 API 密钥 这里需要替换成自己的百度地图开发者密钥

options(baidumap.key="iytRmcdU2cz4BdPTZEbrp7SQWnl5msC0")
## colleges in beijing



# 创建地图对象
map <- getBaiduMap(points <- cbind(c(111.546875, 34.625), c(135.501667, 39.774))
                   for (i in seq_len(nrow(points))) {
                     map$addMarker(lnglat = points[i, ], title = paste("Point", i))$showInfoWindow(content = sprintf("<h3>%s</h3>", i))
                   }
)$setOptions(ak = ak)$addOverlay(type = "heatmap", data = list())

# 添加点标记到地图上

# 输出 HTML 页面
htmlwidgets::saveWidget(map, file = "map.html", selfcontained = FALSE)





2、# 安装包
3、library(devtools)
4、install_github('badbye/baidumap')
5、install_github('lchiffon/REmap')



library(echarts4r)
library(ECharts2Shiny)


flights <- read.csv(
  paste0(
    "https://raw.githubusercontent.com/plotly/datasets/",
    "master/2011_february_aa_flight_paths.csv"
  )
)

flights |>
  e_charts() |>
  e_geo() |>
  e_lines(
    start_lon,
    start_lat,
    end_lon,
    end_lat,
    name = "flights",
    lineStyle = list(normal = list(curveness = 0.3))
  )


# install.packages("rvest")
# install.packages("leafletCN")
# windows:
#   Sys.setlocale("LC_CTYPE", "eng")
## GAME begin~
library(rvest)
library(leafletCN)
# Sys.setlocale("LC_CTYPE", "eng")
doc = read_html("http://www.pm25s.com/cn/rank/")
## http://flukeout.github.io/
cities = doc %>% html_nodes(".cityrank a") %>%
  html_text()
# windows:
#   cities = iconv(cities, "UTF-8", "UTF-8")
AQI = doc %>% html_nodes("span[class^='lv']") %>%
  html_text() %>% .[c(F,F,T)] %>% as.numeric
dat = data.frame(city = cities, AQI = AQI)
geojsonMap(dat, "city",
           popup =  paste0(dat$city,":",dat$AQI),
           palette = "Reds", legendTitle = "AQI")