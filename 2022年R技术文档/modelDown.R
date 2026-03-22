



library(modelDown)

require("ranger")
require("breakDown")
require("DALEX")
library(janitor)
library(janitor)
library(dplyr)
humans <- starwars %>%
  filter(species == "Human")

t1 <- humans %>%
  tabyl(eye_color)

t1

t1 %>%
  adorn_totals("row") %>%
  adorn_pct_formatting()

############## 二维数据的处理
## 行业、概念，产业
## 上市时间，股价高低，交易量特点


t2 <- humans %>%
  tabyl(gender, eye_color)

t2

t2 %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()


### 三个维度的数据 

library(purrr)
humans %>%
  tabyl(eye_color, skin_color, gender, show_missing_levels = FALSE) %>%
  adorn_totals("row") %>%
  adorn_percentages("all") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns %>%
  adorn_title


humans %>%
  tabyl(gender, eye_color) %>%
  adorn_totals(c("row", "col")) %>%
  adorn_percentages("row") %>% 
  adorn_pct_formatting(rounding = "half up", digits = 0) %>%
  adorn_ns() %>%
  adorn_title("combined") %>%
  knitr::kable()


humans %>%
  group_by(gender) %>%
  summarise(pct_above_165_cm = mean(height > 165, na.rm = TRUE), .groups = "drop")%>%
  adorn_pct_formatting()


mtcars %>%
  count(cyl, gear) %>%
  rename(proportion = n) %>%
  adorn_percentages("col", na.rm = TRUE, proportion) %>%
  adorn_pct_formatting(,,,proportion) # the commas say to use the default values of the other arguments

library(tidyr) # for spread()
mpg_by_cyl_and_am <- mtcars %>%
  group_by(cyl, am) %>%
  summarise(mpg = mean(mpg), .groups = "drop") %>%
  spread(am, mpg)

mpg_by_cyl_and_am


######
set.seed(1)
raw_data <- data.frame(sex = rep(c("m", "f"), 3000),
                       age = round(runif(3000, 1, 102), 0))
raw_data$agegroup = cut(raw_data$age, quantile(raw_data$age, c(0, 1/3, 2/3, 1)))

comparison <- raw_data %>%
  tabyl(agegroup, sex, show_missing_levels = F) %>%
  adorn_totals(c("row", "col")) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1)

comparison
#######后面不仅仅增加了百分比还增加了数量

comparison %>%
  adorn_ns()


formatted_ns <- attr(comparison, "core") %>% # extract the tabyl's underlying Ns
  adorn_totals(c("row", "col")) %>% # to match the data.frame we're appending to
  dplyr::mutate_if(is.numeric, format, big.mark = ",")

comparison %>%
  adorn_ns(position = "rear", ns = formatted_ns)



###

###股价，上市时间，如何把连续变量变为离散变量的重要性！！！！



##########################


# Generate simple modelDown page
HR_data_selected <- HR_data[1000:3000,]
HR_glm_model <- glm(left~., HR_data_selected, family = "binomial")
explainer_glm <- explain(HR_glm_model, data=HR_data_selected, y = HR_data_selected$left)

modelDown::modelDown(explainer_glm,
                     modules = c("model_performance", "variable_importance",
                                 "variable_response"),
                     output_folder = tempdir(),
                     repository_name = "HR",
                     device = "png",
                     vr.vars= c("average_montly_hours"),
                     vr.type = "ale")

# More complex example with all modules
HR_ranger_model <- ranger(as.factor(left) ~ .,
                          data = HR_data, num.trees = 500, classification = TRUE, probability = TRUE)
explainer_ranger <- explain(HR_ranger_model,
                            data = HR_data, y = HR_data$left, function(model, data) {
                              return(predict(model, data)$prediction[,2])
                            }, na.rm=TRUE)

# Two glm models used for drift detection
HR_data1 <- HR_data[1:4000,]
HR_data2 <- HR_data[4000:nrow(HR_data),]
HR_glm_model1 <- glm(left~., HR_data1, family = "binomial")
HR_glm_model2 <- glm(left~., HR_data2, family = "binomial")
explainer_glm1 <- explain(HR_glm_model1, data=HR_data1, y = HR_data1$left)
explainer_glm2 <- explain(HR_glm_model2, data=HR_data2, y = HR_data2$left)

modelDown::modelDown(list(explainer_glm1, explainer_glm2),
                     modules = c("auditor", "drifter", "model_performance", "variable_importance",
                                 "variable_response"),
                     output_folder = tempdir(),
                     repository_name = "HR",
                     remote_repository_path = "some_user/remote_repo_name",
                     device = "png",
                     vr.vars= c("average_montly_hours", "time_spend_company"),
                     vr.type = "ale")





library(tidymodels)
library(modeltime)
library(tidyverse)
library(timetk)


data_tbl <- walmart_sales_weekly %>%
  select(id, Date, Weekly_Sales) %>%
  set_names(c("id", "date", "value"))

data_tbl


data_tbl %>%
  group_by(id) %>%
  plot_time_series(
    date, value, .interactive = F, .facet_ncol = 2
  )


nested_data_tbl <- data_tbl %>%
  
  # 1. Extending: We'll predict 52 weeks into the future.
  extend_timeseries(
    .id_var        = id,
    .date_var      = date,
    .length_future = 52
  ) %>%
  
  # 2. Nesting: We'll group by id, and create a future dataset
  #    that forecasts 52 weeks of extended data and
  #    an actual dataset that contains 104 weeks (2-years of data)
  nest_timeseries(
    .id_var        = id,
    .length_future = 52,
    .length_actual = 52*2
  ) %>%
  
  # 3. Splitting: We'll take the actual data and create splits
  #    for accuracy and confidence interval estimation of 52 weeks (test)
  #    and the rest is training data
  split_nested_timeseries(
    .length_test = 52
  )

nested_data_tbl

#####################Prophet

rec_prophet <- recipe(value ~ date,  extract_nested_train_split(nested_data_tbl)) 

wflw_prophet <- workflow() %>%
  add_model(
    prophet_reg("regression", seasonality_yearly = TRUE) %>% 
      set_engine("prophet")
  ) %>%
  add_recipe(rec_prophet)


#######################XGBoost

rec_xgb <- recipe(value ~ .,  extract_nested_train_split(nested_data_tbl)) %>%
  step_timeseries_signature(date) %>%
  step_rm(date) %>%
  step_zv(all_predictors()) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

wflw_xgb <- workflow() %>%
  add_model(boost_tree("regression") %>% set_engine("xgboost")) %>%
  add_recipe(rec_xgb)

##################################Step 2: Nested Modeltime Tables

nested_modeltime_tbl <- modeltime_nested_fit(
  # Nested data 
  nested_data = nested_data_tbl,
  
  # Add workflows
  wflw_prophet,
  wflw_xgb
)

nested_modeltime_tbl


###################################Step 3: Logged Attributes

nested_modeltime_tbl %>% 
  extract_nested_test_accuracy() %>%
  table_modeltime_accuracy(.interactive = F)


#################################Extract Nested Test Forecast

nested_modeltime_tbl %>% 
  extract_nested_test_forecast() %>%
  group_by(id) %>%
  plot_modeltime_forecast(
    .facet_ncol  = 2,
    .interactive = FALSE
  )

################################Extract Nested Error Logs

nested_modeltime_tbl %>% 
  extract_nested_error_report()


#################################Step 4: Select the Best

best_nested_modeltime_tbl <- nested_modeltime_tbl %>%
  modeltime_nested_select_best(
    metric                = "rmse", 
    minimize              = TRUE, 
    filter_test_forecasts = TRUE
  )


###################################Extract Nested Best Model Report

best_nested_modeltime_tbl %>%
  extract_nested_best_model_report()


################################Extract Nested Best Test Forecasts

best_nested_modeltime_tbl %>%
  extract_nested_test_forecast() %>%
  group_by(id) %>%
  plot_modeltime_forecast(
    .facet_ncol  = 2,
    .interactive = FALSE
  )

###############################Step 5: Refitting and Future Forecast

nested_modeltime_refit_tbl <- best_nested_modeltime_tbl %>%
  modeltime_nested_refit(
    control = control_nested_refit(verbose = TRUE)
  )

nested_modeltime_refit_tbl


###############################Extract Nested Future Forecast

nested_modeltime_refit_tbl %>%
  extract_nested_future_forecast() %>%
  group_by(id) %>%
  plot_modeltime_forecast(
    .interactive = FALSE,
    .facet_ncol  = 2
  )


