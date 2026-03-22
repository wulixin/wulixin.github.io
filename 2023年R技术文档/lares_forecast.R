

library(lares)
library(Tushare)

pro <-pro_api(token ='fe8102bf83f5f83f6608aa46fa5e985c534c227786236a1192e5fd55')

###获取数据
dat<-pro(api_name = 'daily', ts_code='600809.SH',start_date= today-years(2))

#####
dat<-dat%>%
  mutate(open=as.numeric(as.character(open)),
         high=as.numeric(as.character(high)),
         close=as.numeric(as.character(close)),
         low=as.numeric(as.character(low)),
         vol=as.numeric(as.character(vol)),
         pre_close=as.numeric(as.character(pre_close)))
colnames(dat)<-c("ts_code","date","open","high","low","close","pre_close","change","pct_change","volume","amount")

df<-dat%>%select(date,close)
prophesize(df,
  dat$close,
  n_future = 60,
  country = NULL,
  trend.param = 0.05,
  logged = FALSE,
  pout = 0.03,
  project = "Prophet Forecast"
)

forecast_arima(dat$date,as.numeric(dat$close),n_future = 20,ARMA = 8,ARMA_min = 5,
  AR = 5,MA = 5,wd_excluded = NA,plot = TRUE,plot_days = 20,project = NA)
