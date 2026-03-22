library(timetk)
library(modeltime)
library(lares)
library(tidymodels)
library(modelStudio)
library(ModelMetrics)
library(DALEX)

# The data we'll use is the Titanic dataset
data(dft)
df <- subset(dft, select = -c(Ticket, PassengerId, Cabin))


r <- h2o_automl(df, y = Survived, max_models = 1, impute = FALSE, target = "TRUE")

plot(r)

r$metrics
r$plots$metrics
#r$response
#r$conf_matrix
#r$ROC

head(r$importance)


r$plots$importance

r <- h2o_automl(df, Pclass, ignore = c("Fare", "Cabin"), max_time = 30, plots = FALSE)
#
plot(r)

r <- h2o_automl(df, y = "Fare", ignore = "Pclass", exclude_algos = NULL, quiet = TRUE)
print(r)
plot(r)


##########Visualize K-MEANS Clusters for Serveral K

Sys.unsetenv("LARES_FONT") # Temporal
data("iris")
df <- subset(iris, select = c(-Species))
df <- df[sample(nrow(df)), ]

# Calculate and plot
result <- clusterVisualK(df, ks = 2:4)
plot(result$plot)

# You can use the data generated as well
lapply(result$data, function(x) head(x$cluster, 10))

library(lareshiny)


daily_portfolio(), etf_sector(), splot_change(),
splot_divs(), splot_etf(), splot_growth(), splot_roi(), 
splot_summary(), splot_types(), 
stocks_file(), stocks_obj(), stocks_quote(), stocks_report()


splot_roi(dat$close, n_days = 365, historical = TRUE, ma = c(12, 50), save = FALSE)
library(lubridate)
forecast_arima(
  ymd(dat$date),
  dat$close,
  n_future = 10,
  ARMA = 8,
  ARMA_min = 5,
  AR = 5,
  MA = 15,
  wd_excluded = NA,
  plot = TRUE,
  plot_days = 360,
  project = NA)

library(prophet)
prophesize(
  df,
  n_future = 60,
  country = NULL,
  trend.param = 0.05,
  logged = FALSE,
  pout = 0.03,
  project = "Prophet Forecast"
)

## Not run: 
api_key <- get_credentials()$openai$secret_key
# Open question:
gpt_ask("Can you write an R function to plot a dummy histogram?", api_key)

##### The following examples return dataframes:
# Classify each element based on categories:
gpt_classify(1:10, c("odd", "even"))

# Add all tags that apply to each element based on tags:
gpt_tag(
  c("I love chocolate", "I hate chocolate", "I like Coke"),
  c("food", "positive", "negative", "beverage"))

# Extract specific information:
gpt_extract(
  c("My mail is 123@test.com", "30 Main Street, Brooklyn, NY, USA", "+82 2-312-3456", "$1.5M"),
  c("email", "full state name", "country of phone number", "amount as number"))

# Format values
gpt_format(
  c("March 27th, 2021", "12-25-2023 3:45PM", "01.01.2000", "29 Feb 92"),
  format = "ISO Date getting rid of timestamps")

# Convert units
gpt_convert(c("50C", "300K"), "Fahrenheit")

# Create a table with data
gpt_table("5 random people's address in South America, email, phone, age between 18-30")

# Translate text to any language
gpt_translate(
  rep("I love you with all my heart", 5),
  language = c("spanish", "chinese", "japanese", "russian", "german"))




data(dft) # Titanic dataset

# TRAIN A SIMPLE MODEL
dfm <- h2o_automl(dft,
                  y = "Survived",
                  ignore = c("Ticket", "PassengerId", "Cabin"),
                  max_models = 1
)

# EXPLAINER
explainer <- h2o_explainer(df = dfm$datasets$test, model = dfm$model, y = "Survived")
explainer$data <- na.omit(explainer$data)

# CATEGORICAL EXAMPLE
class <- dalex_variable(explainer, vars = c("Pclass", "Sex"))
class$plot

# NUMERICAL EXAMPLE
num <- dalex_variable(explainer, vars = c("Fare", "Age"))
num$plot

# LOCAL EXAMPLE
local <- dalex_local(explainer, row = 1)
# OR YOU COULD MANUALLY INPUT THE OBSERVATION
local <- dalex_local(explainer, observation = explainer$data[1, ])
local$plot

# xai2shiny's UI (needs to be installed from ModelOriented/xai2shiny)
xai2shiny(explainer, run = TRUE)

## End(Not run)


##
##weighted cross tabulation 
##
data(dft) # Titanic dataset
crosstab(dft, Survived, Pclass, total = FALSE)
# Show values in percentages
crosstab(dft, Pclass, Survived, prow = TRUE)
crosstab(dft, Pclass, Survived, pall = TRUE)
# Weighted by another variable
crosstab(dft, Survived, Pclass, wt = Fare, prow = TRUE)

#write.csv(stocks_names,"//Users//wulixin//Desktop//stocksfull.csv")

formatHTML("Text test", color = "#000000")
formatHTML(c(123, 456), color = "orange", size = 120, bold = TRUE)

# Multiple quotes at the same time
stocks_quote(c("VTI", "VOO", "TSLA"))

## Not run: 
# CRAN
df <- stocks_hist(symbols = c("VTI", "FB", "FIW"), from = Sys.Date() - 180)
print(head(df))
plot(df)

