

Time Series Foundations - Visualization, Preprocessing, Noise Reduction, & Anomaly Detection
Feature Engineering using lagged variables & external regressors
Hyperparameter Tuning - For both sequential and non-sequential models
Time Series Cross-Validation (TSCV)
Ensembling Multiple Machine Learning & Univariate Modeling Techniques (Competition Winner)
Deep Learning with GluonTS (Competition Winner)


library(modeltime)

library(tidyverse)
library(lubridate)
library(timetk)
library(Tushare)




today<-ymd(Sys.Date())
pro <-pro_api(token ='fe8102bf83f5f83f6608aa46fa5e985c534c227786236a1192e5fd55')

###获取数据
dat<-pro(api_name = 'daily', ts_code='002424.SZ',start_date= today-years(2))

#####
dat<-dat%>%
  mutate(open=as.numeric(as.character(open)),
         high=as.numeric(as.character(high)),
         close=as.numeric(as.character(close)),
         low=as.numeric(as.character(low)),
         vol=as.numeric(as.character(vol)),
         pre_close=as.numeric(as.character(pre_close)))
colnames(dat)<-c("ts_code","date","open","high","low","close","pre_close","change","pct_change","volume","amount")

###############Part 1: Autocorrelation

dat%>%
  plot_time_series(date, close, .interactive =TRUE)

ACF Plot: The autocorrleation (y-axis), which is the relationship between the series
and each progressive lag (x-axis) with the series.

PACF Plot: The partial-autocorrelation vs lags. 
The Partial Autocorrelation shows how much each progressive ACF adds to the predictability. 
In other words, lags that are correlated with each other are de-weighted so the most important lags are present.
###

dat %>%
  plot_acf_diagnostics(date, close, .interactive =TRUE)

head(dat)
dat$year<-year(ymd(dat$date))
#######这里加入群股的分析
dat %>%
  group_by(year) %>%
  plot_time_series(date, close, 
                   .facet_ncol = 3, 
                   .facet_scale = "free",
                   .interactive = TRUE)


dat %>%
  group_by(year) %>%
  plot_acf_diagnostics(
    date, close,               # ACF & PACF
    .lags = "14 days",         # 14-Days of hourly lags
    .interactive = TRUE )


################个股  VS 板块  

walmart_sales_weekly %>%
  select(id, Date, Weekly_Sales, Temperature, Fuel_Price) %>%
  group_by(id) %>%
  plot_acf_diagnostics(
    Date, Weekly_Sales,        # ACF & PACF
    .ccf_vars           = c(Temperature, Fuel_Price),   # CCFs
    .show_ccf_vars_only = TRUE,                         # Toggle just CCFs?
    .lags               = "2 years",                    # Lags
    .interactive        = interactive
  )
###############股价数据可视化
m4_daily %>%
  group_by(id) %>%
  plot_time_series(date, value, 
                   .facet_ncol = 2, .facet_scales = "free",
                   .interactive = interactive)

##############trend and smooth 

m4_weekly %>%
  group_by(id) %>%
  plot_time_series(date, value, 
                   
                   # Smoother
                   .smooth = TRUE, 
                   .smooth_span = 0.25,           # <- Uses % of data
                   # .smooth_period = "52 weeks", # <- Uses windows of data
                   
                   .facet_ncol = 2, .facet_scales = "free",
                   .interactive = interactive)


######################Visualizing Transformations & Sub-Groups

m4_hourly %>%
  group_by(id) %>%
  plot_time_series(date, log(value),             # Apply a Log Transformation
                   .color_var = week(date),      # Color applied to Week transformation
                   # Facet formatting
                   .facet_ncol = 2, 
                   .facet_scales = "free", 
                   .interactive = interactive)


########################Static ggplot2 Visualizations & Customizations
taylor_30_min %>%
  plot_time_series(date, value, 
                   .color_var = month(date, label = TRUE),
                   
                   .interactive = FALSE,   # <- Returns static ggplot
                   
                   # Customization
                   .title = "Taylor's MegaWatt Data",
                   .x_lab = "Date (30-min intervals)",
                   .y_lab = "Energy Demand (MW)",
                   .color_lab = "Month") +
  scale_y_continuous(labels = scales::comma_format())


##############
# Static ggplot
taylor_30_min %>%
  plot_time_series(date, value, .color_var = week(date),
                   .interactive = FALSE, .color_lab = "Week")

# INTERACTIVE Plotly
taylor_30_min %>%
  plot_time_series(date, value, .color_var = week(date),
                   .interactive = TRUE, .plotly_slider = TRUE, .color_lab = "Week")


# 2. Visualize anomalies


walmart_sales_weekly %>%
  group_by(Store, Dept) %>%
  plot_anomaly_diagnostics(Date, Weekly_Sales, 
                           .facet_ncol = 3, .interactive = FALSE)

##########3. Make a seasonality plot

taylor_30_min %>%
  plot_seasonal_diagnostics(date, value, .interactive = FALSE)



##########4. Inspect autocorrelation, partial autocorrelation (and cross correlations too)

taylor_30_min %>%
  plot_acf_diagnostics(date, value, .lags = "1 week", .interactive = FALSE)



library(fable)
# Load libraries
library(fpp2)         # An older forecasting framework
library(fpp3)         # A newer tidy forecasting framework
library(timetk)       # An even newer tidy forecasting framework
library(tidyverse)    # Collection of data manipulation tools
library(tidyquant)    # Business Science ggplot theme
library(cowplot)      # A ggplot add-on for arranging plots

# Quarterly Australian production data as tibble
aus <- tsibbledata::aus_production %>% as_tibble()
# Check structure
aus %>% str()


#######2.2 fpp2 Method: From tibble to ts

# Convert tibble to time series object
aus_prod_ts <- ts(aus[, 2:7],  # Choose columns
                  start = c(1956, 1),  # Choose start date
                  end = c(2010, 2),    # Choose end date
                  frequency = 4)       # Choose frequency per yr
# Check it out
aus_prod_ts %>% tail()


########2.3 fpp3 Method: From ts to tsibble
# Convert ts to tsibble and keep wide format
aus_prod_tbl_wide <- aus_prod_ts %>%    # TS object
  as_tsibble(index = "index",           # Set index column
             pivot_longer = FALSE)      # Wide format

######2.3.2 Pivot Long
# Convert ts to tsibble and pivot to long format
aus_prod_tbl_long <- aus_prod_ts %>%    # TS object
  as_tsibble(index = "index",           # Set index column
             pivot_longer = TRUE)       # Long format



#####2.4 timetk Method: From tsibble/ts to tibble
########2.4.1 Pivot Wide

# Convert tsibble to tibble, keep wide format
aus <- tsibbledata::aus_production %>% 
  tk_tbl() %>%
  mutate(Quarter = as_date(as.POSIXct.Date(Quarter)))

####2.4.2 Pivot Long
# Quarterly Australian production data to long format
aus_long <- aus %>% 
  rename(date = Quarter) %>%
  pivot_longer(
    cols = c("Beer","Tobacco","Bricks",
             "Cement","Electricity","Gas"))



################3. Time Series Plots

Trend: A long-term increase or decrease in the data; a “changing direction”.

Seasonality: A seasonal pattern of a fixed and known period. If the frequency is unchanging and associated with some aspect of the calendar, then the pattern is seasonal.

Cycle: A rise and fall pattern not of a fixed frequency. If the fluctuations are not of a fixed frequency then they are cyclic.

Seasonal vs Cyclic: Cyclic patterns are longer and more variable than seasonal patterns in general.

###########3.1 fpp2 Method: Plot Multiple Series On Same Axes

# Using fpp2
aus_prod_ts %>%               # TS object
  autoplot(facets=FALSE)  


###3.2 fpp3 Method: Plot Multiple Series On Same Axes
# Using fpp3
aus_prod_tbl_long %>%    # Data in long format
  autoplot(value) 



####3.3 ggplot Method: Plot Multiple Series On Same Axes
# Using ggplot
aus_long %>%
  ggplot(aes(date, value, group = name, color = name)) +
  geom_line()


#######3.4 fpp2 Method: Plot Multiple Series On Separate Axes
# Using fpp2
aus_prod_ts %>%  
  autoplot(facets=TRUE)   # With facetting


# Using fpp3
aus_prod_tbl_long %>%
  ggplot(aes(x = index, y = value, group = key)) + 
  geom_line() + 
  facet_grid(vars(key), scales = "free_y")   # With facetting

# Using timetk   timeTK数据可视化
aus_long %>% 
  plot_time_series(
    .date_var = date,
    .value = value,
    .facet_vars = c(name), # Group by these columns
    .color_var = name, 
    .interactive = FALSE,
    .legend_show = FALSE
  )

###############4. Seasonal Plots
#Use seasonal plots for identifying time periods in which the patterns change.

##############4.1 fpp2 Method: Plot Individual Seasons
# Monthly plot of anti-diabetic scripts in Australia 
a1 <- a10 %>%
  autoplot() 
# Seasonal plot
a2 <- a10 %>% 
  ggseasonplot(year.labels.left = TRUE,   # Add labels
               year.labels = TRUE) 
# Arrangement of plots
plot_grid(a1, a2, ncol=1, rel_heights = c(1, 1.5))

###########4.2 fpp3 Method: Plot Individual Seasons
# Monthly plot of anti-diabetic scripts in Australia
a1 <- a10 %>%
  as_tsibble() %>%
  autoplot(value)
# Seasonal plot
a2 <- a10 %>%
  as_tsibble() %>%
  gg_season(value, labels="both")   # Add labels
# Arrangement of plots
plot_grid(a1, a2, ncol=1, rel_heights = c(1, 1.5))

####################4.3 ggplot Method: Plot Individual Seasons

# Convert ts to tibble
a10_tbl <- fpp2::a10 %>%
  tk_tbl()
# Monthly plot of anti-diabetic scripts in Australia 
a1 <- a10_tbl %>% 
  plot_time_series(
    .date_var = index,
    .value    = value,
    .smooth   = TRUE,
    .interactive = FALSE,
    .title = "Monthly anti-diabetic scripts in Australia"
  )
# New time-based features to group by
a10_tbl_add <- a10_tbl %>% 
  mutate( 
    month = factor(month(index, label = TRUE)),  # Plot this
    year = factor(year(index))  # Grouped on y-axis
  )
# Seasonal plot
a2 <- a10_tbl_add %>%
  ggplot(aes(x = month, y = value, 
             group = year, color = year)) + 
  geom_line() + 
  geom_text(
    data = a10_tbl_add %>% filter(month == min(month)),
    aes(label = year, x = month, y = value),
    nudge_x = -0.3) + 
  geom_text(
    data = a10_tbl_add %>% filter(month == max(month)),
    aes(label = year, x = month, y = value),
    nudge_x = 0.3) + 
  guides(color = FALSE)
# Arrangement of plots
plot_grid(a1, a2, ncol=1, rel_heights = c(1, 1.5))


library(forecast)
#########5.1 fpp2 Method: Plot Subseries on Same Axes
# Monthly beer production in Australia 1992 and after
beer_fpp2 <- fpp2::ausbeer %>%
  window(start = 1992)    
# Time series plot
b1 <- beer_fpp2 %>% 
  autoplot() 
# Subseries plot
b2 <- beer_fpp2 %>% 
  ggsubseriesplot() 
# Plot it
plot_grid(b1, b2, ncol=1, rel_heights = c(1, 1.5))


library(feasts)
####################5.2 fpp3 Method: Plot Subseries on Separate Axes
# Monthly beer production in Australia 1992 and after
beer_fpp3 <- fpp2::ausbeer %>%
  as_tsibble() %>%
  filter(lubridate::year(index) >= 1992)
# Time series plot
b3 <- beer_fpp3 %>% 
  autoplot(value) 
# Subseries plot
b4 <- beer_fpp3 %>%
  gg_subseries(value) 
# Plot it
plot_grid(b3, b4, ncol=1, rel_heights = c(1, 1.5))



##################5.3 timetk Method: Plot Subseries on Separate Axes
# Monthly beer production in Australia 1992 and after
ausbeer_tbl <- fpp2::ausbeer %>%
  tk_tbl() %>%
  filter(year(index) >= 1992) %>%
  mutate(index = as_date(index))
# Time series plot
b1 <- ausbeer_tbl %>%
  plot_time_series(
    .date_var = index,
    .value    = value,
    .interactive = FALSE
  )
# Subseries plot
b2 <- ausbeer_tbl %>%
  mutate(
    quarter = str_c("Quarter ", as.character(quarter(index)))
  ) %>%
  plot_time_series(
    .date_var = index,
    .value = value,
    .facet_vars = quarter,
    .facet_ncol = 4, 
    .color_var = quarter, 
    .facet_scales = "fixed",
    .interactive = FALSE,
    .legend_show = FALSE
  )
# Plot it
plot_grid(b1, b2, ncol=1, rel_heights = c(1, 1.5))



#######6. Lag Plots   Use lag plots to check for randomness.

########6.1 fpp2 Method: Plot Multiple Lags
# Plot of non-seasonal oil production in Saudi Arabia
o1 <- fpp2::oil %>%
  autoplot()
# Lag plot of non-seasonal oil production
o2 <- gglagplot(oil, do.lines = FALSE)
# Plot both
plot_grid(o1, o2, ncol=1, rel_heights = c(1,2))


6.2 fpp3 Method: Plot Multiple Lags
# Plot of non-seasonal oil production
o1 <- oil %>%
  as_tsibble() %>%
  autoplot(value)
# Lag plot of non-seasonal oil production
o2 <- oil %>%
  as_tsibble() %>%
  gg_lag(y=value, geom = "point") 
# Plot it
plot_grid(o1, o2, ncol=1, rel_heights = c(1,2))



6.3 timetk Method (Hack?) : Plot Multiple Lags
# Convert to tibble and create lag columns
oil_lag_long <- oil %>%
  tk_tbl(rename_index = "year") %>%
  tk_augment_lags(      # Add 9 lag columns of data
    .value = value, 
    .names = "auto", 
    .lags = 1:9) %>%
  pivot_longer(         # Pivot from wide to long
    names_to = "lag_id", 
    values_to = "lag_value", 
    cols = value_lag1:value_lag9)  # Exclude year & value



# Time series plot
o1 <- oil %>%
  tk_tbl(rename_index = "year") %>%  
  mutate(year = ymd(year, truncated = 2L)) %>%  
  plot_time_series(
    .date_var = year, 
    .value = value,
    .interactive = FALSE)
# timetk Method: Plot Multiple Lags
o2 <- oil_lag_long %>%
  plot_time_series(
    .date_var = value,     # Use value instead of date
    .value = lag_value,    # Use lag value to plot against
    .facet_vars = lag_id,  # Facet by lag number
    .facet_ncol = 3,
    .interactive = FALSE, 
    .smooth = FALSE,
    .line_alpha = 0,      
    .legend_show = FALSE,
    .facet_scales = "fixed"
  ) + 
  geom_point(aes(colour = lag_id)) + 
  geom_abline(colour = "gray", linetype = "dashed") 
# Plot it
plot_grid(o1, o2, ncol=1, rel_heights = c(1,2))



7.1 fpp2 Method: Plot ACF + PACF
# ACF plot 
o1 <- ggAcf(oil, lag.max = 20)
# PACF plot
o2 <- ggPacf(oil, lag.max = 20)
# Plot both
plot_grid(o1, o2, ncol = 1)


7.2 fpp3 Method: Plot ACF + PACF
# Convert to tsibble
oil_tsbl <- oil %>% as_tsibble()
# ACF Plot
o1 <- oil_tsbl %>%
  ACF(lag_max = 20) %>%
  autoplot()
# PACF Plot
o2 <- oil_tsbl %>%
  PACF(lag_max = 20) %>%
  autoplot() 
# Plot both
plot_grid(o1, o2, ncol = 1)



7.3 timetk Method: Plot ACF & PACF
# Using timetk
oil %>%
  tk_tbl(rename_index = "year") %>%
  plot_acf_diagnostics(
    .date_var = year,
    .value    = value,
    .lags     = 20,
    .show_white_noise_bars = TRUE, 
    .interactive = FALSE
  )


