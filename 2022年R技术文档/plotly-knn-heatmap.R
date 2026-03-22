


library(tidyverse)
library(tidymodels)
library(plotly)

make_moons <- read.csv(file = "data/make_moons.csv")
make_moons$y <- as.character(make_moons$y)
set.seed(123)
make_moons_split <- initial_split(make_moons, prop = 3/4)
make_moons_training <- make_moons_split %>%
  training()
make_moons_test <- make_moons_split %>%
  testing()
train_index <- as.integer(rownames(make_moons_training))
test_index <- as.integer(rownames(make_moons_test))
make_moons[train_index,'split'] = 'Train Split Label'
make_moons[test_index,'split'] = 'Test Split Label'
make_moons$y <- paste(make_moons$split,make_moons$y)

fig <- plot_ly(data = make_moons, x = ~X1, y = ~X2, type = 'scatter', mode = 'markers',alpha = 0.5, symbol = ~y, symbols = c('square','circle','square-dot','circle-dot'),
               marker = list(size = 12,
                             color = 'lightyellow',
                             line = list(color = 'black',width = 1)))

fig




library(plotly)
library(pracma)
library(kknn)
library(tidymodels)

make_moons <- read.csv(file = "data/make_moons.csv")
make_moons_classification <- make_moons
make_moons$y <- as.character(make_moons$y)
set.seed(123)
make_moons_split <- initial_split(make_moons, prop = 3/4)
make_moons_training <- make_moons_split %>%
  training()
make_moons_test <- make_moons_split %>%
  testing()
train_index <- as.integer(rownames(make_moons_training))
test_index <- as.integer(rownames(make_moons_test))

mesh_size = .02
margin = 0.25
x_min =  min(make_moons$X1) - margin
x_max = max(make_moons$X1) + margin
y_min = min(make_moons$X2) - margin
y_max = max(make_moons$X2) + margin
xrange <- seq(x_min, x_max, mesh_size)
yrange <- seq(y_min, y_max, mesh_size)
xy <- meshgrid(x = xrange, y = yrange)
xx <- xy$X
yy <- xy$Y

make_moons_classification$y <- as.factor(make_moons_classification$y)

knn_dist <- nearest_neighbor(neighbors = 15, weight_func = 'rectangular') %>%
  set_engine('kknn') %>%
  set_mode('classification') %>%
  fit(y~., data = make_moons_classification)

dim_val <- dim(xx)
xx1 <- matrix(xx, length(xx), 1)
yy1 <- matrix(yy, length(yy), 1)
final <- data.frame(xx1, yy1)
colnames(final) <- c('X1','X2')
pred <- knn_dist %>%
  predict(final, type = 'prob')

predicted <- pred$.pred_1
Z <- matrix(predicted, dim_val[1], dim_val[2])

fig <- plot_ly(x = xrange, y= yrange, z = Z, colorscale='RdBu', type = "contour")
fig






library(plotly)
library(pracma)
library(kknn)
library(tidymodels)

make_moons <- read.csv(file = "data/make_moons.csv")
make_moons_classification <- make_moons
make_moons$y <- as.character(make_moons$y)
set.seed(123)
make_moons_split <- initial_split(make_moons, prop = 3/4)
make_moons_training <- make_moons_split %>% 
  training()
make_moons_test <- make_moons_split %>% 
  testing()
train_index <- as.integer(rownames(make_moons_training))
test_index <- as.integer(rownames(make_moons_test))

mesh_size = .02
margin = 0.25
x_min =  min(make_moons$X1) - margin
x_max = max(make_moons$X1) + margin
y_min = min(make_moons$X2) - margin
y_max = max(make_moons$X2) + margin
xrange <- seq(x_min, x_max, mesh_size)
yrange <- seq(y_min, y_max, mesh_size)
xy <- meshgrid(x = xrange, y = yrange)
xx <- xy$X
yy <- xy$Y

make_moons_classification$y <- as.factor(make_moons_classification$y)

knn_dist <- nearest_neighbor(neighbors = 15, weight_func = 'rectangular') %>% 
  set_engine('kknn') %>% 
  set_mode('classification') %>%
  fit(y~., data = make_moons_classification)
make_moons[train_index,'split'] = 'Train Split Label'
make_moons[test_index,'split'] = 'Test Split Label'
make_moons$y <- paste(make_moons$split,make_moons$y)

dim_val <- dim(xx)
xx1 <- matrix(xx, length(xx), 1)
yy1 <- matrix(yy, length(yy), 1)
final <- data.frame(xx1, yy1)
colnames(final) <- c('X1','X2')
pred <- knn_dist %>%
  predict(final, type = 'prob')
predicted <- pred$.pred_1
Z <- matrix(predicted, dim_val[1], dim_val[2])


fig <- plot_ly(symbols = c('square','circle','square-dot','circle-dot'))%>%
  add_trace(x = xrange, y= yrange, z = Z, colorscale='RdBu', type = "contour", opacity = 0.5) %>%
  add_trace(data = make_moons, x = ~X1, y = ~X2, type = 'scatter', mode = 'markers', symbol = ~y ,
            marker = list(size = 12,
                          color = 'lightyellow',
                          line = list(color = 'black',width = 1)))
fig



library(pracma)
library(plotly)
library(tidyverse)
library(tidymodels)
library(plyr)

data(iris) # We will use the iris data, which is included in R by default

mesh_size = .02
margin = 1

db_split <- initial_split(iris, prop = 3/4)
train_data <- training(db_split)
test_data <- testing(db_split)

# Create a mesh grid on which we will run our model
l_min = min(iris$Sepal.Length) - margin
l_max = max(iris$Sepal.Length) + margin
w_min = min(iris$Sepal.Width) - margin
w_max = max(iris$Sepal.Width) + margin
lrange = seq(l_min, l_max, mesh_size)
wrange = seq(w_min, w_max, mesh_size)

mg = meshgrid(lrange, wrange)
ll = mg$X
ww = mg$Y

library(kknn)
# Create classifier, run predictions on grid
model = nearest_neighbor( neighbors = 15, weight_func = 'inv' ) %>% 
  set_engine("kknn") %>% 
  set_mode("classification") %>% 
  fit(Species ~ Sepal.Length + Sepal.Width, data = train_data)


ll1 <- matrix(ll, length(ll), 1)
ww1 <- matrix(ww, length(ww), 1)
final <- data.frame(ll1, ww1)

colnames(final) = c("Sepal.Length", "Sepal.Width" )

pred <- model %>%
  predict(final, type = 'prob')

dim_val <- dim(ll)
proba_setosa <- matrix(pred$.pred_setosa, dim_val[1], dim_val[2])
proba_versicolor <- matrix(pred$.pred_versicolor, dim_val[1], dim_val[2])
proba_virginica <- matrix(pred$.pred_virginica, dim_val[1], dim_val[2])

# Compute the classifier confidence
Z <- array(c(proba_setosa, proba_versicolor, proba_virginica), dim = c(dim_val[1],dim_val[2],3))
diff = aaply(Z, c(1,2), max) -  (aaply(Z, c(1,2), sum) - aaply(Z,c(1,2), max))

# Overlay the heatmap of the confidence on the scatter plot of the examples
fig <- plot_ly() 
fig <- fig %>% add_trace(data=test_data, x = ~Sepal.Length, y = ~Sepal.Width, symbol = ~Species, split = ~Species, symbols = c('square-dot','circle-dot','diamond'),
                         type = 'scatter', mode = 'markers',  
                         marker = list(size = 12, line = list(width = 1.5), color = 'lightyellow'))%>% layout(title="Prediction Confidence on Test Split")
fig <- fig %>% add_trace(x = lrange, y = wrange, z = diff, type = 'heatmap')

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
