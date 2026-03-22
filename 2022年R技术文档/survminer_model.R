######################################
#
#      生存模型在退市股，ST股领域的应用
#
#######################################


library(patchwork)
library(survival)
library(tidyquant)
library(janitor)
library(tidyverse)
library(survminer)
BiocManager::install("RTCGA.clinical") # data for examples


library(survminer)
library(RTCGA.clinical)
survivalTCGA(BRCA.clinical, OV.clinical,
             extract.cols = "admin.disease_code") -> BRCAOV.survInfo
library(survival)
fit <- survfit(Surv(times, patient.vital_status) ~ admin.disease_code,
               data = BRCAOV.survInfo)
# Visualize with survminer
ggsurvplot(fit, data = BRCAOV.survInfo, risk.table = TRUE)


ggsurvplot(
  fit,                     # survfit object with calculated statistics.
  data = BRCAOV.survInfo,  # data used to fit survival curves. 
  risk.table = TRUE,       # show risk table.
  pval = TRUE,             # show p-value of log-rank test.
  conf.int = TRUE,         # show confidence intervals for 
  # point estimaes of survival curves.
  xlim = c(0,2000),        # present narrower X axis, but not affect
  # survival estimates.
  break.time.by = 500,     # break X axis in time intervals by 500.
  ggtheme = theme_minimal(), # customize plot and risk table with a theme.
  risk.table.y.text.col = T, # colour risk table text annotations.
  risk.table.y.text = FALSE # show bars instead of names in text annotations
  # in legend of risk table
)



head(lung)
library("survival")
data("lung")
fit <- survfit(Surv(time, status) ~ sex, data = lung)

###Log-rank (survdiff)
ggsurvplot(fit, data = lung, pval = TRUE, pval.method = TRUE)

###log-rank(comp)
ggsurvplot(fit, data = lung, pval = TRUE, pval.method = TRUE,
           log.rank.weights = "1")


###Gehan-Breslow (generalized Wilcoxon)
ggsurvplot(fit, data = lung, pval = TRUE, pval.method = TRUE,
           log.rank.weights = "n", pval.method.coord = c(5, 0.1),
           pval.method.size = 3)




########Compute a Cox model interaction 

res.cox <- coxph(Surv(time, status) ~ ph.karno * age, data=lung)
summary(res.cox, conf.int = FALSE)

#######Visualization of the hazard ratios using the function ggforest().

ggforest(res.cox, data = lung)

head(lung)

lung$ph.karno_age <- lung$ph.karno * lung$age

res.cox2 <- coxph(Surv(time, status) ~ ph.karno + age + ph.karno_age, data = lung)
summary(res.cox2 , conf.int = FALSE)

ggforest(res.cox2, data=lung)

