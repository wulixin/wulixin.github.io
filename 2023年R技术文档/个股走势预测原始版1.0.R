

#############################个股走势预测投票模型
library(TSstudio)
#library(bsts)
library(forecastML)
library(DT)
library(dygraphs)
library(ggplot2)
library(plotly)
library(highcharter)
library(ggvis)
library(RColorBrewer)
#other packages
library(lubridate)
library(dplyr)
library(forcats)
library(MAPA)
library(magick)
library(nnfor)
library(data.table)

library(PerformanceAnalytics)
library(TTR)
library(nnfor)
library(forecast)
library(xts)
library(zoo)
library(PortfolioAnalytics)
library(Quandl)
library(astsa)
library(flexdashboard)
library(dplyr)
#finance packages 
library(readr)
library(devtools)
library(foreach)
library(Tushare)
library(lubridate)
library(tidyquant)
#dat <- tq_get(params$symbol, get = "stock.prices", from = params$start, to = params$end)
library(quantmod)
library(TTR)
library(tidyquant)
library(Quandl)
library(xts)

today<-ymd(Sys.Date())
pro <-pro_api(token ='fe8102bf83f5f83f6608aa46fa5e985c534c227786236a1192e5fd55')

###获取数据
dat<-pro(api_name = 'daily', ts_code='002672.SZ',start_date= today-years(1))

#####
dat<-dat%>%
  mutate(open=as.numeric(as.character(open)),
         high=as.numeric(as.character(high)),
         close=as.numeric(as.character(close)),
         low=as.numeric(as.character(low)),
         vol=as.numeric(as.character(vol)),
         pre_close=as.numeric(as.character(pre_close)))
colnames(dat)<-c("ts_code","date","open","high","low","close","pre_close","change","pct_change","volume","amount")


library(xgboost)
library(tidymodels)
library(modeltime)
library(tidyverse)
library(lubridate)
library(timetk)
library(h2o)
library(Hmisc)


m000638<-china_stocks%>%filter(ts_code=="000638.SZ")%>%unnest(stock.prices)

m000638 %>%
  plot_time_series(desc(ymd(Date)), Close, .interactive = FALSE)

# Split Data 80/20
splits <- initial_time_split(m000638, prop = 0.9)

# Model 1: auto_arima ----
model_fit_arima_no_boost <- arima_reg() %>%
  set_engine(engine = "auto_arima") %>%
  fit(Close ~ Date, data = training(splits))

# Model 2: arima_boost ----
library(lubridate)
model_fit_arima_boosted <- arima_boost(
  min_n = 2,
  learn_rate = 0.015
) %>%
  set_engine(engine = "auto_arima_xgboost") %>%
  fit(Close ~ Date + as.numeric(Date),
      data = training(splits))

# Model 3: ets ----Exponential Smoothing (Modeltime)
model_fit_ets <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(Close ~ Date, data = training(splits))
#> frequency = 12 observations per 1 year



# Model 4: prophet ----Model 4: Prophet (Modeltime)
model_fit_prophet <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(Close ~ Date, data = training(splits))
#> Disabling weekly seasonality. Run prophet with weekly.seasonality=TRUE to override this.
#> Disabling daily seasonality. Run prophet with daily.seasonality=TRUE to override this.


# Model 5: lm ----Linear Regression (Parsnip)
model_fit_lm <- linear_reg() %>%
  set_engine("lm") %>%
  fit(Close ~ as.numeric(Date), 
      data = training(splits))


# Model 6: earth ----MARS (Workflow)
# a Multivariate Adaptive Regression Spline model 
model_spec_mars <- mars(mode = "regression") %>%
  set_engine("earth") 

recipe_spec <- recipe(Close ~ Date, data = training(splits)) %>%
  step_date(Date, features = "month", ordinal = FALSE) %>%
  step_mutate(date_num = as.numeric(Date)) %>%
  step_normalize(date_num) %>%
  step_rm(Date)

wflw_fit_mars <- workflow() %>%
  add_recipe(recipe_spec) %>%
  add_model(model_spec_mars) %>%
  fit(training(splits))


## Step 3 - Add fitted models to a Model Table.

models_tbl <- modeltime_table(
  model_fit_arima_no_boost,
  model_fit_arima_boosted,
  model_fit_ets,
  model_fit_prophet,
  model_fit_lm,
  wflw_fit_mars
)

models_tbl

######Step 4 - Calibrate the model to a testing set.
#Calibrating adds a new column, .calibration_data, with the test predictions and residuals inside. A few notes on Calibration:
  
#  Calibration is how confidence intervals and accuracy metrics are determined
#Calibration Data is simply forecasting predictions and residuals that are calculated from out-of-sample data.
#After calibrating, the calibration data follows the data through the forecasting workflow.


calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(splits))

calibration_tbl

###Step 5 - Testing Set Forecast & Accuracy Evaluation
#Visualizing the Forecast vs Test Data Set
#Evaluating the Test (Out of Sample) Accuracy


calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = m000638
  ) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
    .interactive      = TRUE
  )

#Models 1&2: ARIMA & ARIMA Boost are performing well. Both models have “auto” components because we used Auto ARIMA. The XGBoost component has parameters that were specified. We can possibly get better accuracy by tuning, but because the ARIMA component is working well on this data, additional improvement may be low.
#Model 3: ETS(M,A,A) is performing the best. The 80% confidence interval is the most narrow of the bunch, indicating the hold out set is modeled well.
#Model 4: PROPHET is comparable to the ARIMA models, but has a slightly wider test error confidence interval.
#Model 5: LM is over-shooting the local trend. This is because the trend component is a simple linear line, which doesn’t account for the change points.
#Model 6: EARTH is overfitting the local trend. This is because we did not tune the number of change points, so the algorithm is auto-calculating the change points.

#######5B - Accuracy Metrics
##MAE - Mean absolute error, mae()
##MAPE - Mean absolute percentage error, mape()
##MASE - Mean absolute scaled error, mase()
##SMAPE - Symmetric mean absolute percentage error, smape()
##RMSE - Root mean squared error, rmse()
##RSQ - R-squared, rsq()

calibration_tbl %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = TRUE
  )


### Step 6 - Refit to Full Dataset & Forecast Forward

refit_tbl <- calibration_tbl %>%
  modeltime_refit(data = m000638)

refit_tbl %>%
  modeltime_forecast(h = "3 years", actual_data = m000638) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
    .interactive      = TRUE
  )



library("DALEX")
library("ranger")
library("modelStudio")

# fit a model
model <- ranger(score ~., data = happiness_train)

# create an explainer for the model    
explainer <- explain(model,
                     data = happiness_test,
                     y = happiness_test$score,
                     label = "Random Forest",
                     verbose = FALSE)

# make a studio for the model
modelStudio(explainer)



library(tidyverse)
library(lubridate)
library(timetk)

# Setup for the plotly charts (# FALSE returns ggplots)
interactive <- FALSE

#########Plotting Groups
library(ggtext)
library(showtext)
showtext_auto(enable = TRUE)
font_add('Songti', 'Songti.ttc')
#font_families()
names<-c("万泰生物","信捷电气","九洲药业","卫信康","石英股份","健友股份","交建股份")

china_stocks %>%
  unnest(stock.prices)%>%
  filter(name %in% names)%>%
  group_by(name) %>%
  plot_time_series(desc(Date), Close, 
                   .facet_ncol = 3, .facet_scales = "free",
                   .interactive = interactive)
  


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

roll_avg_10 <- slidify(.f = AVERAGE, .period = 10, .align = "center", .partial = TRUE)

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


library(funModeling)

status(heart_disease)

plot_num(heart_disease)

profiling_num(heart_disease)

library(dplyr)

# Select only two variables for this example
heart_disease_2=heart_disease %>% select(chest_pain, thal)

# Frequency distribution
freq(heart_disease_2)

##########
correlation_table(heart_disease, "has_heart_disease")


var_rank_info(heart_disease, "has_heart_disease")

cross_plot(data=heart_disease, input=c("age", "oldpeak"), target="has_heart_disease")

plotar(data=heart_disease, input = c("age", "oldpeak"), target="has_heart_disease", plot_type="boxplot")


##############equal_freq: Convert numeric variable to categoric

new_age=equal_freq(heart_disease$age, n_bins = 5)

# checking results
Hmisc::describe(new_age)



# Create machine learning model and get its scores for positive case 
fit_glm=glm(has_heart_disease ~ age + oldpeak, data=heart_disease, family = binomial)
heart_disease$score=predict(fit_glm, newdata=heart_disease, type='response')

# Calculate performance metrics
gain_lift(data=heart_disease, score='score', target='has_heart_disease')








########################




curr <- dat %>% tail(1)


library(tvthemes)
p <- dat %>%
  plot_ly(x = ~date, type="candlestick",
          open = ~open, close = ~close,
          high = ~high, low = ~low) %>%
  layout(title = "股票走势",
         xaxis = list(rangeslider = list(visible = F)))

p


##交易策略可视化

#从指标到信号到交易规则再到交易策略,四步走最后可视化,实现2*3策略!

today<-ymd(Sys.Date())

stock_prices_xts<-as.xts(OHLCV(dat),order.by=ymd(dat$date))
# Rename
names(stock_prices_xts) <- c("Open", "High", "Low", "Close","Adjusted","Volume")
#SPY<- adjustOHLC(stock_prices)
S<-stock_prices_xts
##逃顶指标
ratio <- S$Close/((S$High + S$Low)/2)
avgratio <- SMA(ratio,n=2)
out<-(avgratio-1)*1000
dat$date<-ymd(dat$date)
S.TAOPAO<-xts(x=out,order.by =dat$date)

S.SMA.14 <- SMA(Cl(S), n = 14)
S.SMA.7 <- SMA(Cl(S), n = 7)
S.SMA.20 <- SMA(Cl(S), n = 20)
S.SMA.30 <- SMA(Cl(S), n = 30)
S.SMA.100 <- SMA(Cl(S), n = 100)
S.RSI.14 <- RSI(Cl(S))
S.RSI.SellLevel <- xts(rep(71, NROW(S)), index(S))
S.RSI.BuyLevel <- xts(rep(31, NROW(S)), index(S))
highchart(type = "stock") %>% 
  # create axis :)
  hc_yAxis_multiples(
    create_yaxis(3, height = c(2, 1, 1), turnopposite = TRUE)
  ) %>% 
  # series :D
  hc_add_series(S, yAxis = 0, name = "股票") %>% 
  hc_add_series(S.SMA.7, yAxis = 0, name = "Fast MA 7") %>%
  hc_add_series(S.SMA.14, yAxis = 0, name = "Fast MA 14") %>%
  hc_add_series(S.SMA.20, yAxis = 0, name = "Fast MA 20") %>%
  hc_add_series(S.SMA.30, yAxis = 0, name = "Fast MA 30") %>%
  hc_add_series(S.SMA.100, yAxis = 0, name = "Slow MA 100") %>% 
  hc_add_series(S$Volume, color = "gray", yAxis = 1, name = "Volume", type = "column") %>% 
  hc_add_series(S.RSI.14, yAxis = 2, name = "Osciallator", color = hex_to_rgba("green", 0.7)) %>%
  hc_add_series(S.RSI.SellLevel, color = hex_to_rgba("red", 0.7),
                yAxis = 2, name = "Sell level") %>% 
  hc_add_series(S.RSI.BuyLevel, color = hex_to_rgba("blue", 0.7),
                yAxis = 2, name = "Buy level") 



## `r params$symbol` 交易卖出信号

#追涨杀跌,精准的逃顶指标!数学家高斯关于布兰运动的研究成果应用！

dygraph(data = S.TAOPAO,main="逃顶指标") %>% 
  dyHighlight(highlightCircleSize = 5, 
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = FALSE, highlightSeriesOpts = list(strokeWidth = 3)) %>%
  dyRangeSelector()



## `r params$symbol` 交易数据查询

df<-dat

df_index<-df%>%mutate(month=month(date))%>%
  mutate(pct_change=as.numeric(pct_change))%>%
  select(close,pct_change,volume,month)%>%
  group_by(month)%>%
  summarize(meanClose=mean(close),mean_vol=mean(volume))

medvol<-median(df_index$mean_vol)
maxvol<-max(df_index$mean_vol)
medclose<-median(df_index$meanClose)
maxclose<-max(df_index$meanClose)
datatbl<-df%>%select(date,high,low,close,pct_change,volume)
datatable(datatbl,filter = 'top', options = list(
  pageLength = 15, autoWidth = TRUE)) %>% 
  formatStyle('close',color = styleInterval(c(medclose,maxclose), c('green', 'blue', 'red')),
              fontWeight = styleInterval(12.5, c('normal', 'bold'))) %>%
  formatStyle('pct_change',color = styleInterval(c(0, 5), c('green', 'blue', 'red')),
              fontWeight = styleInterval(5, c('normal', 'bold'))) %>%
  formatStyle( 
    'volume',color = styleInterval(c(medvol, maxvol), c('green', 'blue', 'red')),
    backgroundColor = styleInterval(10000, c('gray', 'yellow'))) 



##  `r params$symbol` 交易量监控


p2 <- plot_ly(dat, x = ~date, y = ~ volume, type = 'bar') %>%
  layout(yaxis = list(title = '交易量'))

p2



## `r params$symbol`超级机器学习

library(nnfor)
library(fma)
a<-dat%>%arrange(date)#filter(trade_date>="2018-10-30")
y<-ts(a$close,frequency=252)


###Model-1 ELM
fit<-elm(y, m = frequency(y), hd = 30, type = "ridge", reps = 20, comb = "mean", lags =10,
         keep = NULL, difforder = NULL, outplot = FALSE,
         sel.lag = TRUE, direct = TRUE,allow.det.season = TRUE, det.type = "auto", 
         xreg = NULL, xreg.lags = NULL, xreg.keep = NULL, barebone = TRUE, model = NULL, retrain =  TRUE)
plot(fit)


library(fable)
library(fabletools)
autoplot(forecast(fit,h=9))

###Model-1 ELM_SPLINEF


fit<-splinef(y, h=10)
autoplot(fit)%>%ggtitle(subtitle = "ELM超级机器学习算法")

## 多维度模型

library(forecastHybrid)
quickModel <- hybridModel(y)
plot(quickModel, type = "fit")


## `r params$symbol`  ARIMA  MODELS

airforecast <- forecast(auto.arima(a$close), level = 95)
hchart(airforecast)%>%hc_add_theme(hc_theme_ffx())

########上面这两个模型预测出现错误

## `r params$symbol`  ETS  MODELS


x1 <- forecast(ets(a$close), h = 48, level = 95)
hchart(x1)%>%hc_add_theme(hc_theme_ffx())



## `r params$symbol`模拟人脑细胞预测
# install.packages("remotes")
#install_github("robjhyndman/forecast")
library(forecast)
library(fma)
library(fable)
fit2 <- nnetar(y, lambda=0.01)
autoplot(forecast(fit2,h=20))+
  ggtitle("NNFOR神经网络算法预测未来走势") +
  xlab("date") +
  ylab("price")

##  Meta AI人工智能预测


library(prophet)
library(lubridate)
history <- data.frame(ds =ymd(dat$date),
                      y = dat$close)
m <- prophet(history)
future <- make_future_dataframe(m, periods = 15)
forecast <- predict(m, future)
plot(m, forecast)+
  ggtitle("MetaAI人工智能算法预测走势") +
  xlab("date") +
  ylab("price")

## 多维度评估 
mapafit <- mapaest(dat$close,outplot=0)
mapacalc(m,mapafit,outplot=2)
#
plotmapa(mapafit)
fit <- mlp(y)
print(fit)
plot(fit)
frc <- forecast(fit,h=36)
plot(frc)

##  `r params$symbol` 量化指标体系

chartSeries(stock_prices_xts, type = "bars",theme = chartTheme('white',up.col='red',dn.col='blue'))
zoomChart('2018-4::')
addTA(EMA(Cl(stock_prices_xts)), on=1, col=6)
#addTA(OpCl(AAPL), col=4, type='b', lwd=2)
##Add Moving Average Convergence Divergence indicator to chart.
addMACD(fast = 12, slow = 26, signal = 9, type = "EMA", histogram = TRUE)
##Apply options or futures expiration vertical bars to current chart.
addExpiry(type = "options", lty = "dotted")
##William's percent R indiator to the current chart.
addWPR(n = 14)
addRSI(n = 14, maType = "EMA", wilder = TRUE)
##Add Parabolic Stop and Reversal indicator overlay to chart
addSAR(accel = c(0.02, 0.2), col = "black")
##
addBBands(n = 20, sd = 2, maType = "SMA", draw = 'bands', on = -1)
## ADD Commodity Channel Index
addCCI(n = 20, maType="SMA", c=0.015)
##Add Rate Of Change indicator to chart.可以用来查看涨停的次数
addROC(n = 1, type = c("discrete", "continuous"), col = "red")
##Add Directional Movement Index
addADX(n = 14, maType="EMA", wilder=TRUE)
zoomChart()

