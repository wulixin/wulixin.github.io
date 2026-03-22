

#extend_timeseries(): This extends each one of our time series into the future by 52 timestamps (this is one year for our weekly data set).
#nest_timeseries(): This converts our data to the nested data format indicating that our future data will be the last 52 timestamps (that we just extended).
#split_nested_timeseries(): This adds indicies for the train / test splitting so we can develop accuracy metrics and determine which model to use for which time series.
library(modelDown)
library(modelStudio)
library(modeltime)


nested_data_tbl <- walmart_sales_weekly %>%
  select(id, Date, Weekly_Sales) %>%
  set_names(c("id", "date", "value")) %>%
  extend_timeseries(
    .id_var        = id,
    .date_var      = date,
    .length_future = 52
  ) %>%
  nest_timeseries(
    .id_var        = id,
    .length_future = 52
  ) %>%
  split_nested_timeseries(
    .length_test = 52
  )

nested_data_tbl

############ xgb

rec_xgb <- recipe(value ~ ., extract_nested_train_split(nested_data_tbl, 1)) %>%
  step_timeseries_signature(date) %>%
  step_rm(date) %>%
  step_zv(all_predictors()) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

wflw_xgb <- workflow() %>%
  add_model(boost_tree("regression") %>% set_engine("xgboost")) %>%
  add_recipe(rec_xgb)

wflw_xgb

########## rec——prophet 

rec_prophet <- recipe(value ~ date, extract_nested_train_split(nested_data_tbl)) 

wflw_prophet <- workflow() %>%
  add_model(
    prophet_reg("regression", seasonality_yearly = TRUE) %>% 
      set_engine("prophet")
  ) %>%
  add_recipe(rec_prophet)

wflw_prophet

###########  sparkly 

nested_modeltime_tbl <- nested_data_tbl %>%
  modeltime_nested_fit(
    wflw_xgb,
    wflw_prophet,
    
    control = control_nested_fit(allow_par = TRUE, verbose = TRUE)
  )

nested_modeltime_tbl

nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  table_modeltime_accuracy(.interactive = F)



#############test forecast 

nested_modeltime_tbl %>%
  extract_nested_test_forecast() %>%
  group_by(id) %>%
  plot_modeltime_forecast(.facet_ncol = 2, .interactive = F)

#########close clusters and shutdown spark 
# Unregisters the Spark Backend
parallel_stop()

# Disconnects Spark
spark_disconnect_all()





