
library(lares)

# The data we'll use is the Titanic dataset
data(dft)
df <- subset(dft, select = -c(Ticket, PassengerId, Cabin))

###################Classification: Binary
r <- h2o_automl(df, y = Survived, max_models = 1, impute = FALSE, target = "TRUE")
plot(r)

r$metrics
r$plots$metrics

r$plots$response
$conf_matrix
head(r$importance)

#############Classification: Multi-Categorical
r <- h2o_automl(df, Pclass, ignore = c("Fare", "Cabin"), max_time = 30, plots = FALSE)

plot(r)


###############Regression

r <- h2o_automl(df, y = "Fare", ignore = "Pclass", exclude_algos = NULL, quiet = TRUE)
print(r)

plot(r)

lares::mplot_splits(tag = results$label, 
                    score = results$pred,
                    split = 8)







#font_families()
names<-c("万泰生物","信捷电气","九洲药业","卫信康","石英股份","健友股份","交建股份")

china_stocks %>%
  unnest(stock.prices)%>%
  filter(name %in% names)%>%
  group_by(name) %>%
  plot_time_series(desc(Date), Close, 
                   .facet_ncol = 3, .facet_scales = "free",
                   .interactive = TRUE)


china_stocks

#########Visualizing Transformations & Sub-Groups

china_stocks %>%
  unnest(stock.prices)%>%
  filter(name %in% names)%>%
  group_by(name) %>%
  plot_time_series_boxplot(Date, Close,             # Apply a Log Transformation
                           .facet_ncol = 3, 
                           .facet_scales = "free", .period = "1 month",
                           .interactive = interactive)


#Time Series Machine Learning (cutting-edge) with Modeltime - 30+ Models (Prophet, ARIMA, XGBoost, Random Forest, & many more)
#Deep Learning with GluonTS (Competition Winners)
#Time Series Preprocessing, Noise Reduction, & Anomaly Detection
#Feature engineering using lagged variables & external regressors
#Hyperparameter Tuning
#Time series cross-validation
#Ensembling Multiple Machine Learning & Univariate Modeling Techniques (Competition Winner)
#Scalable Forecasting - Forecast 1000+ time series in parallel
china_stocks %>%
  unnest(stock.prices)%>%
  filter(name %in% names)%>%
  #filter_by_time(Date, "2022-09", "2022-12") %>%
  group_by(name) %>%
  summarise_by_time(Date, .by = "day",volume = SUM(Volume)) %>%
  plot_time_series(desc(Date), volume, .facet_ncol = 2, .interactive = FALSE, .y_intercept = 0)


#####三十日均线

roll_avg_10 <- slidify(.f = AVERAGE, .period = 10,
                       .align = "center", .partial = TRUE)

# Apply the rolling function
# Apply the rolling function

# Apply roll apply Function
china_stocks %>%
  unnest(stock.prices)%>%
  filter(name %in% names)%>%
  #filter_by_time(Date, "2022-09", "2022-12") %>%
  group_by(name) %>%
  mutate(rolling_avg_10 = slidify_vec(Close,  ~ AVERAGE(.), 
                                      .period = 10, .partial = TRUE))


