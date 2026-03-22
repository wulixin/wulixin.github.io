

url<-"http://www.r-datacollection.com/materials/html/fortunes.html"

fortunes<-readLines(url)

fortunes



library(XML)
parsed_fortunes<-htmlParse(file=url)
print(parsed_fortunes)

#删除了body节点
h1<-list("body"=function(x) {NULL})

parsed_fortunes<-htmlTreeParse(url,handlers=h1,asTree=TRUE)
parsed_fortunes$children 


#把DOM树中处理器元素都丢弃了

h2<-list(startElement=function(node,...){
  name<-xmlName(node)
  if(name %in% c("div","title")){NULL} else {node}
},comment=function(node){NULL})


parsed_fortunes<-htmlTreeParse(url,handlers=h2,asTree=TRUE)
parsed_fortunes$children 


# 创建一个叫return的函数，用来返回前面产生的容器对象，使用能够给局部变量负值的超级负值符号

getItalics<-function(){
  i_container=character()
  list(i=function(node,...){
    i_container<<-c(i_container,xmlValue(node))
  },returnI=function() i_container)
}

h3<-getItalics()

invisible(htmlTreeParse(url,handlers=h3))

#把文档中出现的所有<i>节点输出到屏幕

h3$returnI()


parsed_doc<-htmlParse(file=url)

xpathSApply(doc=parsed_doc,path="/html/body/div/p/i")

#允许节点之间跳转 

xpathSApply(doc=parsed_doc,path="//body//p/i")

xpathSApply(doc=parsed_doc,path="//p/i")

#一次性进行多个查询

xpathSApply(parsed_doc,"//address|//title")

twoQueries<-c(address="//address",title="//title")

xpathSApply(parsed_doc,twoQueries)




library(timetk)
library(modeltime)
library(tibbletime)
library(TSstudio)
library(tseries)
library(fable)
library(fabletools)



library(mschart)
library(tidymodels)
library(tibbletime)
library(tidyverse)
library(timetk)

library(modelStudio)
library(modeltime.ensemble)
library(modeltime.gluonts)
library(modeltime.h2o)


library(officer)
library(officedown)
library(rvg)
library(flextable)
library(mschart)

#devtools::install_github("ianmoran11/mmtable2")
library(mmtable2)
library(ggdist)
library(ggside)
library(DataEditR)
library(openxlsx)
library(DataExplorer)

library(gghalves)
library(patchwork)
library(portfoliodown)
#devtools::install_github("business-science/portfoliodown")
library(plotly)





## highcharts

charts <- lapply(1:9, function(x) {
  hchart(ts(cumsum(rnorm(100))))
})

if (interactive()) {
  hw_grid(charts, rowheight = 300)
}


##plotly 

p <- plot_ly(mpg, x = ~cty, y = ~hwy, alpha = 0.3) 
subplot(
  add_markers(p, size = ~cyl, name = "default"),
  add_markers(p, size = ~cyl, sizes = c(1, 500), name = "custom")
)



# create three visualizations of the diamonds dataset
subplot(
  plot_ly(diamonds, x = ~cut),
  plot_ly(diamonds, x = ~cut, y = ~clarity),
  plot_ly(diamonds, x = ~cut, color = ~clarity, colors = "Accent"))


