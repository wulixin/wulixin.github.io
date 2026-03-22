



IPO_data<-pro(api_name="new_share",start_date='20220101', end_date='20230101')
library(lares)
library(DataExplorer)

introduce(IPO_data)

plot_intro(IPO_data)

plot_missing((IPO_data))

#### 分类变量 
plot_bar(IPO_data)

### 连续变量

plot_histogram(IPO_data)

### 高价发行圈钱，市盈率比较高的现象依然是存在的

qq_data <- IPO_data[, c('amount','ballot','pe','funds','market_amount','price')]

plot_qq(qq_data, sampled_rows = 1000L)

### 中签率与股价和市盈率的关系
plot_correlation(na.omit(IPO_data), maxcat = 5L)


############ 主成分分析

pca_df <- na.omit(qq_data)

plot_prcomp(pca_df, variance_cap = 0.9, nrow = 2L, ncol = 2L)


##########  

## Reduce data size for demo purpose
arr_delay_df <- qq_data

## Call boxplot function  股价在0~60区间市盈率是比较合理的
plot_boxplot(arr_delay_df, by = "price")




create_report(iris)
create_report(airquality, y = "Ozone")

# Load library
library(ggplot2)
library(data.table)
library(rmarkdown)

# Set some missing values
diamonds2 <- data.table(diamonds)
for (j in 5:ncol(diamonds2)) {
  set(diamonds2,
      i = sample.int(nrow(diamonds2), sample.int(nrow(diamonds2), 1)),
      j,
      value = NA_integer_)
}

# Create customized report for diamonds2 dataset
create_report(
  data = diamonds2,
  output_format = html_document(toc = TRUE, toc_depth = 6, theme = "flatly"),
  output_file = "report.html",
  output_dir = getwd(),
  y = "price",
  config = configure_report(
    add_plot_prcomp = TRUE,
    plot_qq_args = list("by" = "cut", sampled_rows = 1000L),
    plot_bar_args = list("with" = "carat"),
    plot_correlation_args = list("cor_args" = list("use" = "pairwise.complete.obs")),
    plot_boxplot_args = list("by" = "cut"),
    global_ggtheme = quote(theme_light())
  )
)

## Configure report without `configure_report`
config <- list(
  "introduce" = list(),
  "plot_intro" = list(),
  "plot_str" = list(
    "type" = "diagonal",
    "fontSize" = 35,
    "width" = 1000,
    "margin" = list("left" = 350, "right" = 250)
  ),
  "plot_missing" = list(),
  "plot_histogram" = list(),
  "plot_density" = list(),
  "plot_qq" = list(sampled_rows = 1000L),
  "plot_bar" = list(),
  "plot_correlation" = list("cor_args" = list("use" = "pairwise.complete.obs")),
  "plot_prcomp" = list(),
  "plot_boxplot" = list(),
  "plot_scatterplot" = list(sampled_rows = 1000L)
)


