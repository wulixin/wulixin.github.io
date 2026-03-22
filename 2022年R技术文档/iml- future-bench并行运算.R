


set.seed(42)
library("iml")
library("future")
library("future.callr")
library(bench)

library("randomForest")
data("Boston", package = "MASS")
rf <- randomForest(medv ~ ., data = Boston, n.trees = 10)
X <- Boston[which(names(Boston) != "medv")]
predictor <- Predictor$new(rf, data = X, y = Boston$medv)


#Going parallel


# Creates a PSOCK cluster with 2 cores
plan("callr", workers = 2)


#Now we can easily compute feature importance in parallel. 
imp <- FeatureImp$new(predictor, loss = "mae")
library("ggplot2")
plot(imp)


bench::system_time({
  plan(sequential)
  FeatureImp$new(predictor, loss = "mae")
})
#> process    real 
#>   1.56s    1.3s
bench::system_time({
  plan("callr", workers = 2)
  FeatureImp$new(predictor, loss = "mae")
})
#> process    real 
#>   1.72s   3.92s


bench::system_time({
  plan(sequential)
  FeatureImp$new(predictor, loss = "mae", n.repetitions = 20)
})
#> process    real 
#>    5.1s   4.72s

bench::system_time({
  plan("callr", workers = 2)
  FeatureImp$new(predictor, loss = "mae", n.repetitions = 20)
})
#> process    real 
#>   1.72s   5.51s



bench::system_time({
  plan(sequential)
  Interaction$new(predictor)
})
#> process    real 
#>   8.26s   7.87s
bench::system_time({
  plan("callr", workers = 2)
  Interaction$new(predictor)
})
#> process    real 
#>   1.72s   7.61s
#>   

bench::system_time({
  plan(sequential)
  FeatureEffects$new(predictor)
})
#> process    real 
#>   945ms   823ms
bench::system_time({
  plan("callr", workers = 2)
  FeatureEffects$new(predictor)
})
#> process    real 
#>   6.88s   10.3s