


dataprices = pro(api_name="daily",trade_date='20221014')

df_all<-dataprices%>%
  filter(pct_chg > 8 & pct_chg < 21)%>%left_join(stocks_names)

df_all$list_date<-ymd(df_all$list_date)
latest_month<-Sys.Date()-days(350)

stocks_name<-stocks_names%>%filter(name %in% df_all$name & ymd('20220201')>ymd(list_date))
#stocks_name<-stocks_names%>%
#  filter(name %in% c())

china_stocks <- stocks_name %>%
  mutate(
    stock.prices = map(ts_code, 
                       function(.x) get_stock_prices(.x)
    ),
    log.returns  = map(stock.prices, 
                       function(.x) get_log_returns(.x)),
    mean.log.returns = map_dbl(log.returns, ~ mean(.$Log.Returns)),
    sd.log.returns   = map_dbl(log.returns, ~ sd(.$Log.Returns)),
    n.trade.days = map_dbl(stock.prices, nrow)
  )%>%as_tibble()



### 高深一点数据可视化


require(dplyr)
require(purrr)
require(tidyr)
require(gapminder)

gp <- china_stocks %>%unnest(stock.prices)%>%
  distinct(industry, .keep_all = TRUE)
head(gp)


gp2 <-china_stocks %>%unnest(stock.prices)%>%
  nest(-industry) %>%
  mutate(
    data = map(data, mutate_mapping, hcaes(x = Date, y = Close), drop = TRUE),
    data = map(data, list_parse)
  ) %>%
  rename(ttdata = data)

head(gp2)

gptot <- left_join(gp, gp2)

hc <- hchart(
  gptot,
  "point",
  hcaes(
    Date,
    Close,
    name = name,
    size = Volume,
    group = industry
  )
) %>%
  hc_yAxis(type = "logarithmic")

hc %>%
  hc_tooltip(useHTML = TRUE, pointFormatter = tooltip_chart(accesor = "ttdata"))

hc %>%
  hc_tooltip(useHTML = TRUE, pointFormatter = tooltip_chart(
    accesor = "ttdata",
    hc_opts = list(chart = list(type = "column"))
  ))

hc %>%
  hc_tooltip(
    useHTML = TRUE,
    positioner = JS("function () { return { x: this.chart.plotLeft + 10, y: 10}; }"),
    pointFormatter = tooltip_chart(
      accesor = "ttdata",
      hc_opts = list(
        title = list(text = "point.country"),
        xAxis = list(title = list(text = "lifeExp")),
        yAxis = list(title = list(text = "gdpPercap"))
      )
    )
  )

hc %>%
  hc_tooltip(
    useHTML = TRUE,
    pointFormatter = tooltip_chart(
      accesor = "ttdata",
      hc_opts = list(
        legend = list(enabled = TRUE),
        series = list(list(color = "gray", name = "point.name"))
      )
    )
  )







gp <- gapminder %>%
  arrange(desc(year)) %>%
  distinct(country, .keep_all = TRUE)

gp2 <- gapminder %>%
  nest(-country) %>%
  mutate(
    data = map(data, mutate_mapping, hcaes(x = lifeExp, y = gdpPercap), drop = TRUE),
    data = map(data, list_parse)
  ) %>%
  rename(ttdata = data)

gptot <- left_join(gp, gp2)

hc <- hchart(
  gptot,
  "point",
  hcaes(
    lifeExp,
    gdpPercap,
    name = country,
    size = pop,
    group = continent
  )
) %>%
  hc_yAxis(type = "logarithmic")

hc %>%
  hc_tooltip(useHTML = TRUE, pointFormatter = tooltip_chart(accesor = "ttdata"))

hc %>%
  hc_tooltip(useHTML = TRUE, pointFormatter = tooltip_chart(
    accesor = "ttdata",
    hc_opts = list(chart = list(type = "column"))
  ))

hc %>%
  hc_tooltip(
    useHTML = TRUE,
    positioner = JS("function () { return { x: this.chart.plotLeft + 10, y: 10}; }"),
    pointFormatter = tooltip_chart(
      accesor = "ttdata",
      hc_opts = list(
        title = list(text = "point.country"),
        xAxis = list(title = list(text = "lifeExp")),
        yAxis = list(title = list(text = "gdpPercap"))
      )
    )
  )

hc %>%
  hc_tooltip(
    useHTML = TRUE,
    pointFormatter = tooltip_chart(
      accesor = "ttdata",
      hc_opts = list(
        legend = list(enabled = TRUE),
        series = list(list(color = "gray", name = "point.name"))
      )
    )
  )

