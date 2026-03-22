
library(highcharter)
require(dplyr)
library(Tushare)
library(lubridate)

pro <-pro_api(token ='fe8102bf83f5f83f6608aa46fa5e985c534c227786236a1192e5fd55')

stocks_names<-pro(api_name = 'stock_basic')

head(stocks_names)

area_count<-stocks_names%>%
  mutate(list_date=ymd(list_date))%>%
  filter(list_date>ymd('20120101'))%>%
  group_by(area)%>%
  count(.)%>%
  arrange(desc(n))
area_count$area
area_count$'woe-name'<-c("Zhejiang","Jiangsu","Guangdong","Beijing","Shanghai","Shenzhen","Shandong","NA","Fujian",
                         "Sichuan","Anhui","Hunan","Hubei","Henan","Jiangxi","Shaanxi","Tianjin","Chongqing","Liaoning","Hebei",
                         "Xinjiang","Guizhou","Jilin","Xizang","Yunnan","Gansu","Guangxi","Heilongjiang","Hainan","Inner Mongol",
                         "Shanxi","Ningxia","Qinghai")
#colnames(area_count)<-c("省市","上市公司数量")
area_count

mpdta <- download_map_data("https://code.highcharts.com/mapdata/countries/cn/custom/cn-all-sar-taiwan.js",
                           quiet = TRUE
)
#countries/cn/custom/cn-all-sar-taiwan
str(mpdta, 1)
cn_map<-get_data_from_map(mpdta)

hcmap(
  map = "countries/cn/custom/cn-all-sar-taiwan",data =area_count,
  joinBy = "woe-name", value = "n", name = "上市公司数量",legend="area")


#N22°38′17.54″ 东经E114°05′52.35

df_sz<-data.frame(area=c("深圳"),name=c("Shenzhen"),longitude=c("114.05"),latitude=c("38.17"))

area_geo<-area_count%>%
       left_join(cn_map)%>%
     select(area,name,n,longitude,latitude)
area_geo[6,]$longitude<-df_sz$longitude
area_geo[6,]$name<-df_sz$name
area_geo[6,]$latitude<-df_sz$latitude

area_geo$n<-as.numeric(area_geo$n)
area_geo$latitude<-as.numeric(area_geo$latitude)
area_geo$longitude<-as.numeric(area_geo$longitude)
area_geo<-area_geo%>%drop_na(.)

library(leaflet)

leaflet(area_geo) %>% addTiles() %>%
  addCircles(lng = ~longitude, lat = ~latitude, 
             radius = ~sqrt(n)* 10000, popup = ~area)%>%
  addMarkers(data=area_geo,clusterOptions = markerClusterOptions(),
clusterId = "quakesCluster") 

sum(area_count$n)
