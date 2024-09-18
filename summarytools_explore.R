
library(summarytools)
1. Overview
2. Frequency Tables: freq()
3. Cross-Tabulations: ctable()
4. Descriptive Statistics: descr()
5. Data Frame Summaries: dfSummary()
6. Grouped Statistics: stby()
7. Grouped Statistics: group_by()
8. Tidy Tables : tb()
9. Directing Output to Files
10. Package Options
11. Format Attributes
12. Fine-Tuning Looks : CSS
13. Shiny Apps
14. Graphs in R Markdown
15. Languages & Term Customization
16. Vignette Setup

freq(iris$Species, plain.ascii = FALSE, style = "rmarkdown")

freq(iris$Species, report.nas = FALSE, headings = FALSE)


freq(iris$Species, 
     report.nas = FALSE, 
     totals     = FALSE, 
     cumul      = FALSE, 
     headings   = FALSE)

freq(tobacco)

ctable(x = tobacco$smoker, 
       y = tobacco$diseased, 
       prop = "r")   # Show row proportions


descr(iris)


descr(iris,
      stats     = c("mean", "sd"),
      transpose = TRUE,
      headings  = FALSE)


view(dfSummary(iris))


dfSummary(tobacco, 
          plain.ascii  = FALSE, 
          style        = "grid", 
          graph.magnif = 0.75, 
          valid.col    = FALSE,
          tmp.img.dir  = "/tmp")


(iris_stats_by_species <- stby(data      = iris, 
                               INDICES   = iris$Species, 
                               FUN       = descr, 
                               stats     = "common", 
                               transpose = TRUE))


with(tobacco, 
     stby(data    = BMI, 
          INDICES = age.gr, 
          FUN     = descr,
          stats   = c("mean", "sd", "min", "med", "max"))
)


library(dplyr)
tobacco$gender %<>% forcats::fct_explicit_na()
tobacco %>% 
  group_by(gender) %>% 
  descr(stats = "fivenum")



library(magrittr)
iris %>%
  descr(stats = "common") %>%
  tb()

