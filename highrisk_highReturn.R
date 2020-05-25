

# get stock data
library(tidyverse)
library(BatchGetSymbols)
library(GetBCBData)

first.date <- '2008-01-01' # last date is Sys.Date by default

# get stock data
df.ibov <- GetIbovStocks()
mkt.idx <- c('^BVSP')
my.tickers <- c(mkt.idx, paste0(df.ibov$tickers, '.SA') )

df.prices <- BatchGetSymbols(tickers = my.tickers, first.date = first.date,
                             freq.data = 'yearly')[[2]]

tab.stocks <- df.prices %>%
  na.omit() %>%
  group_by(ticker) %>%
  summarise(mean.ret = mean(ret.adjusted.prices),
            sd.ret = sd(ret.adjusted.prices)) %>%
  mutate(ticker = str_replace_all(ticker, fixed('.SA'), '') )

tab.mkt.idx <- tab.stocks %>%
  filter(ticker %in% mkt.idx)



tab.stocks <- tab.stocks %>%
  filter(!(ticker %in% mkt.idx))

# get CDI (risk free rate) 
my.id <- c(CDI = 4389)

tab.CDI <- gbcbd_get_series(my.id, first.date = first.date) %>%
  rename(ticker = series.name ) %>%
  mutate(ref.date = format(ref.date, '%Y'),
         value = value/100) %>%
  group_by(ref.date, ticker) %>%
  summarise(ret = mean(value)) %>%
  group_by(ticker) %>%
  summarise(mean.ret = mean(ret),
            sd.ret = sd(ret))



library(ggplot2)

p <- ggplot(tab.stocks, aes(x = sd.ret, y = mean.ret, group = ticker)) + 
  geom_point() + 
  geom_text(data = tab.stocks, aes(x = sd.ret, y = mean.ret, label = ticker), nudge_y = 0.03,
            check_overlap = TRUE, nudge_x = 0.05 ) + 
  geom_point(data = tab.CDI, aes(x = sd.ret, y = mean.ret, color = ticker), size =5) +
  geom_point(data = tab.mkt.idx, 
             aes(x = sd.ret, y = mean.ret, color = ticker), size =5) +
  labs(x = 'Risk (standard deviation)', y ='Expected Returns (average)', 
       title = 'Mean X Variance map for B3',
       subtitle = paste0(nrow(tab.stocks), ' stocks, ', lubridate::year(min(df.prices$ref.date)), 
                         ' - ', lubridate::year(max(df.prices$ref.date)))) + 
  scale_x_continuous(labels = scales::percent) + 
  scale_y_continuous(labels = scales::percent)  

