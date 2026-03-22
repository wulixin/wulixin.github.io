


library(plotly)

data(iris)

axis = list(showline=FALSE,
            zeroline=FALSE,
            gridcolor='#ffff',
            ticklen=4,
            titlefont=list(size=13))


fig <- iris %>%
  plot_ly()
fig <- fig %>%
  add_trace(
    type = 'splom',
    dimensions = list(
      list(label='sepal length', values=~Sepal.Length),
      list(label='sepal width', values=~Sepal.Width),
      list(label='petal length', values=~Petal.Length),
      list(label='petal width', values=~Petal.Width)
    ),
    color = ~Species, colors = c('#636EFA','#EF553B','#00CC96') ,
    marker = list(
      size = 7,
      line = list(
        width = 1,
        color = 'rgb(230,230,230)'
      )
    )
  )
fig <-  fig %>% style(diagonal = list(visible = FALSE))
fig <- fig %>%
  layout(
    hovermode='closest',
    dragmode= 'select',
    plot_bgcolor='rgba(240,240,240, 0.95)',
    xaxis=list(domain=NULL, showline=F, zeroline=F, gridcolor='#ffff', ticklen=4),
    yaxis=list(domain=NULL, showline=F, zeroline=F, gridcolor='#ffff', ticklen=4),
    xaxis2=axis,
    xaxis3=axis,
    xaxis4=axis,
    yaxis2=axis,
    yaxis3=axis,
    yaxis4=axis
  )

fig


###PCA  

library(plotly)
library(stats)
data(iris)
X <- subset(iris, select = -c(Species))

prin_comp <- prcomp(X)

explained_variance_ratio <- summary(prin_comp)[["importance"]]['Proportion of Variance',]

explained_variance_ratio <- 100 * explained_variance_ratio

components <- prin_comp[["x"]]

components <- data.frame(components)
components <- cbind(components, iris$Species)
components$PC3 <- -components$PC3
components$PC2 <- -components$PC2

axis = list(showline=FALSE,
            zeroline=FALSE,
            gridcolor='#ffff',
            ticklen=4,
            titlefont=list(size=13))

fig <- components %>%
  plot_ly()  %>%
  add_trace(
    type = 'splom',
    dimensions = list(
      list(label=paste('PC 1 (',toString(round(explained_variance_ratio[1],1)),'%)',sep = ''), values=~PC1),
      list(label=paste('PC 2 (',toString(round(explained_variance_ratio[2],1)),'%)',sep = ''), values=~PC2),
      list(label=paste('PC 3 (',toString(round(explained_variance_ratio[3],1)),'%)',sep = ''), values=~PC3),
      list(label=paste('PC 4 (',toString(round(explained_variance_ratio[4],1)),'%)',sep = ''), values=~PC4)
    ),
    color = ~iris$Species, colors = c('#636EFA','#EF553B','#00CC96')
  ) %>%
  style(diagonal = list(visible = FALSE)) %>%
  layout(
    legend=list(title=list(text='color')),
    hovermode='closest',
    dragmode= 'select',
    plot_bgcolor='rgba(240,240,240, 0.95)',
    xaxis=list(domain=NULL, showline=F, zeroline=F, gridcolor='#ffff', ticklen=4),
    yaxis=list(domain=NULL, showline=F, zeroline=F, gridcolor='#ffff', ticklen=4),
    xaxis2=axis,
    xaxis3=axis,
    xaxis4=axis,
    yaxis2=axis,
    yaxis3=axis,
    yaxis4=axis
  )

fig


library(plotly)
library(stats)
library(MASS)

db = Boston

prin_comp <- prcomp(db, rank. = 4)

components <- prin_comp[["x"]]
components <- data.frame(components)
components <- cbind(components, db$medv)
components$PC2 <- -components$PC2
colnames(components)[5] = 'Median_Price'

tot_explained_variance_ratio <- summary(prin_comp)[["importance"]]['Proportion of Variance',]
tot_explained_variance_ratio <- 100 * sum(tot_explained_variance_ratio)

tit = 'Total Explained Variance = 99.56'

axis = list(showline=FALSE,
            zeroline=FALSE,
            gridcolor='#ffff',
            ticklen=4)

fig <- components %>%
  plot_ly() %>%
  add_trace(
    type = 'splom',
    dimensions = list(
      list(label='PC1', values=~PC1),
      list(label='PC2', values=~PC2),
      list(label='PC3', values=~PC3),
      list(label='PC4', values=~PC4)
    ),
    color=~Median_Price,
    marker = list(
      size = 7
    )
  ) %>% style(diagonal = list(visible = F)) %>%
  layout(
    title= tit,
    hovermode='closest',
    dragmode= 'select',
    plot_bgcolor='rgba(240,240,240, 0.95)',
    xaxis=list(domain=NULL, showline=F, zeroline=F, gridcolor='#ffff', ticklen=4),
    yaxis=list(domain=NULL, showline=F, zeroline=F, gridcolor='#ffff', ticklen=4),
    xaxis2=axis,
    xaxis3=axis,
    xaxis4=axis,
    yaxis2=axis,
    yaxis3=axis,
    yaxis4=axis
  )
options(warn=-1)
fig



##
library(plotly)
library(stats)
data(iris)
X <- subset(iris, select = -c(Species))
prin_comp <- prcomp(X, rank. = 2)
components <- prin_comp[["x"]]
components <- data.frame(components)
components <- cbind(components, iris$Species)
components$PC2 <- -components$PC2

fig <- plot_ly(components, x = ~PC1, y = ~PC2, color = ~iris$Species, colors = c('#636EFA','#EF553B','#00CC96'), type = 'scatter', mode = 'markers')%>%
  layout(
    legend=list(title=list(text='color')),
    plot_bgcolor='#e5ecf6',
    xaxis = list(
      title = "0",
      zerolinecolor = "#ffff",
      zerolinewidth = 2,
      gridcolor='#ffff'),
    yaxis = list(
      title = "1",
      zerolinecolor = "#ffff",
      zerolinewidth = 2,
      gridcolor='#ffff'))
fig




####

data("iris")

X <- subset(iris, select = -c(Species))

prin_comp <- prcomp(X, rank. = 3)

components <- prin_comp[["x"]]
components <- data.frame(components)
components$PC2 <- -components$PC2
components$PC3 <- -components$PC3
components = cbind(components, iris$Species)

tot_explained_variance_ratio <- summary(prin_comp)[["importance"]]['Proportion of Variance',]
tot_explained_variance_ratio <- 100 * sum(tot_explained_variance_ratio)

tit = 'Total Explained Variance = 99.48'

fig <- plot_ly(components, x = ~PC1, y = ~PC2, z = ~PC3, color = ~iris$Species, colors = c('#636EFA','#EF553B','#00CC96') ) %>%
  add_markers(size = 12)


fig <- fig %>%
  layout(
    title = tit,
    scene = list(bgcolor = "#e5ecf6")
  )

fig



########

library(plotly)
library(stats)
library(base)
library(mlbench)
data(PimaIndiansDiabetes)

X <- subset(PimaIndiansDiabetes, select = -c(diabetes))
prin_comp <- prcomp(X)
explained_variance_ratio <- summary(prin_comp)[["importance"]]['Proportion of Variance',]

cumsum <- cumsum(explained_variance_ratio)

data <- data.frame(cumsum,seq(1, length(cumsum), 1))

colnames(data) <- c('Explained_Variance','Components')

fig <-  plot_ly(data = data, x = ~Components, y = ~Explained_Variance, type = 'scatter', mode = 'lines', fill = 'tozeroy') %>%
  layout(
    xaxis = list(
      title = "# Components", tickvals = seq(1, length(cumsum), 1)),
    yaxis = list(
      title = "Explained Variance"))
fig



#####

library(plotly)
library(stats)
data(iris)
X <- subset(iris, select = -c(Species))
prin_comp <- prcomp(X, rank = 2)
components <- prin_comp[["x"]]
components <- data.frame(components)
components <- cbind(components, iris$Species)
components$PC2 <- -components$PC2
explained_variance <- summary(prin_comp)[["sdev"]]
explained_variance <- explained_variance[1:2]
comp <- prin_comp[["rotation"]]
comp[,'PC2'] <- - comp[,'PC2']
loadings <- comp
for (i in seq(explained_variance)){
  loadings[,i] <- comp[,i] * explained_variance[i]
}

features = c('sepal_length', 'sepal_width', 'petal_length', 'petal_width')

fig <- plot_ly(components, x = ~PC1, y = ~PC2, color = ~iris$Species, colors = c('#636EFA','#EF553B','#00CC96'), type = 'scatter', mode = 'markers') %>%
  layout(
    legend=list(title=list(text='color')),
    plot_bgcolor = "#e5ecf6",
    xaxis = list(
      title = "0"),
    yaxis = list(
      title = "1"))
for (i in seq(4)){
  fig <- fig %>%
    add_segments(x = 0, xend = loadings[i, 1], y = 0, yend = loadings[i, 2], line = list(color = 'black'),inherit = FALSE, showlegend = FALSE) %>%
    add_annotations(x=loadings[i, 1], y=loadings[i, 2], ax = 0, ay = 0,text = features[i], xanchor = 'center', yanchor= 'bottom')
}

fig


library(plotly)

fig <- plot_ly() 
# fig <- fig %>% add_trace( ... )
# fig <- fig %>% layout( ... ) 

library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)

app <- Dash$new()
app$layout(
  htmlDiv(
    list(
      dccGraph(figure=fig) 
    )
  )
)

app$run_server(debug=TRUE, dev_tools_hot_reload=FALSE)
