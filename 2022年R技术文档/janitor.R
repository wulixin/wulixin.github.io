

set.seed(1)
raw_data <- data.frame(sex = rep(c("m", "f"), 3000),
                       age = round(runif(3000, 1, 102), 0))
##############
raw_data$agegroup = cut(raw_data$age, quantile(raw_data$age, c(0, 1/3, 2/3, 1)))

comparison <- raw_data %>%
  tabyl(agegroup, sex, show_missing_levels = F) %>%
  adorn_totals(c("row", "col")) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1)

comparison




library(tidygraph)
library(tidymodels)
library(tidyquant)
library(tidyselect)
library(tidyr)
library(tidyverse)

library(tibble)
library(tsibble)
library(tibbletime)
library(tsibbledata)

library(dplyr)
library(dbplyr)
library(dtplyr)

###############
library(dplyr)
library(tsibble)
library(lubridate)

mth <- make_date("2018") + months(0:3)
tsibble(mth = mth, index = mth)

library(janitor)
