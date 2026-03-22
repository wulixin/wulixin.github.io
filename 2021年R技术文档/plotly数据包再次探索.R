


library(plotly)
library(ggplot2)

m <- highlight_key(mpg)

p <- ggplot(m, aes(displ, hwy)) + geom_point()

gg <- highlight(ggplotly(p), "plotly_selected")

crosstalk::bscols(gg, DT::datatable(m))


library(ggforce)
ggplot(mpg, aes(displ, hwy)) + 
  geom_point() +
  geom_mark_hull(aes(filter = model == "corvette", label = model)) +
  labs(
    title = "Fuel economy from 1999 to 2008 for 38 car models",
    caption = "Source: https://fueleconomy.gov/",
    x = "Engine Displacement", 
    y = "Miles Per Gallon"
  )


#最近五天同一个板块股票的涨跌幅

plot_ly(diamonds, x = ~cut, color = ~clarity, colors = "Accent")


library(plotly)

# load the diamonds dataset from the ggplot2 package
data(diamonds, package = "ggplot2")


# create three visualizations of the diamonds dataset
plot_ly(diamonds, x = ~cut)
plot_ly(diamonds, x = ~cut, y = ~clarity)
plot_ly(diamonds, x = ~cut, color = ~clarity, colors = "Accent")

# doesn't produce black bars
plot_ly(diamonds, x = ~cut, color = "black")
# produces red bars with black outline
plot_ly(
  diamonds, 
  x = ~cut, 
  color = I("red"), 
  stroke = I("black"), 
  span = I(2)
)



layout(
  plot_ly(diamonds, x = ~cut),
  title = "My beatiful histogram"
)


diamonds %>%
  plot_ly(x = ~cut) %>%
  layout(title = "My beatiful histogram")


diamonds %>%
  plot_ly() %>% 
  add_histogram(x = ~cut)



diamonds %>%
  dplyr::count(cut) %>%
  plot_ly() %>% 
  add_bars(x = ~cut, y = ~n)


library(dplyr)

diamonds %>%
  plot_ly(x = ~cut) %>% 
  add_histogram() %>%
  group_by(cut) %>%
  summarise(n = n()) %>%
  add_text(text = ~scales::comma(n), y = ~n, 
    textposition = "top middle", 
    cliponaxis = FALSE)



diamonds %>%
  plot_ly(x = ~cut) %>% 
  add_histogram() %>%
  group_by(cut) %>%
  summarise(n = n()) %>% 
  plotly_data()


p <- ggplot(diamonds, aes(x=clarity, y=log(price), color=clarity)) +
  ggforce::geom_sina(alpha = 0.1) + 
  stat_summary(fun.data = "mean_cl_boot", color = "black") +
  facet_wrap(~cut)

p
# WebGL is a lot more efficient at rendering lots of points
toWebGL(ggplotly(p))



m <- lm(log(price) ~ log(carat), data = diamonds)
diamonds <- modelr::add_residuals(diamonds, m)
p <- ggplot(diamonds, aes(x = clarity, y = resid, color = clarity)) +
  ggforce::geom_sina(alpha = 0.1) + 
  stat_summary(fun.data = "mean_cl_boot", color = "black") +
  facet_wrap(~cut)
toWebGL(ggplotly(p))


library(GGally)
m <- lm(log(price) ~ log(carat) + cut, data = diamonds)
gg <- ggcoef(m)
# dynamicTicks means generate new axis ticks on zoom
ggplotly(gg, dynamicTicks = TRUE)

