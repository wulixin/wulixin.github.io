

knitr (GITHUB, WEBSITE)
kableExtra (GITHUB, WEBSITE)
formattable (GITHUB, WEBSITE)
DT (GITHUB, WEBSITE)
pander (GITHUB, WEBSITE)
huxtable (GITHUB, WEBSITE)
reactable (GITHUB, WEBSITE)
flextable (GITHUB, WEBSITE)
ftextra (GITHUB, WEBSITE)
pixiedust (GITHUB)
tangram (GITHUB)
ztable (GITHUB)
condformat (GITHUB)
stargazer (CRAN)
xtable (CRAN)

library(gt)
library(tidyverse)
library(glue)

# Define the start and end dates for the data range
start_date <- "2010-06-07"
end_date <- "2010-06-14"

# Create a gt table based on preprocessed
# `sp500` table data
sp500 %>%
  filter(date >= start_date & date <= end_date) %>%
  select(-adj_close) %>%
  gt() %>%
  tab_header(
    title = "S&P 500",
    subtitle = glue("{start_date} to {end_date}")
  ) %>%
  fmt_date(
    columns = date,
    date_style = 3
  ) %>%
  fmt_currency(
    columns = c(open, high, low, close),
    currency = "USD"
  ) %>%
  fmt_number(
    columns = volume,
    suffixing = TRUE
  )