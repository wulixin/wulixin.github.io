


library(parsnip)
library(recipes)
library(tidyclust)
library(tidymodels)
library(tidyselect)
library(tidyverse)
set.seed(1234)



dashboardthemes	Customise the Appearance of 'shinydashboard' Applications using Themes
dashCoreComponents	Core Interactive UI Components for 'Dash'
dashHtmlComponents	Vanilla HTML Components for 'Dash'
dashPivottable	Interactive React-Based Pivot Tables for Dash
dashTable

kmeans_spec <- k_means(num_clusters = 3) %>%
  set_engine("stats") 

kmeans_spec

kmeans_spec_fit <- kmeans_spec %>%
  fit(~., data = mtcars)
kmeans_spec_fit



predict(kmeans_spec_fit, mtcars[1:4, ])


extract_cluster_assignment(kmeans_spec_fit)

extract_centroids(kmeans_spec_fit)










