#
# Survival analysis with the ALL data
#
library(tidyverse)
library(survival)
library(survminer)
library(coin)
#
data("glioma", package = "coin")
leuk <- read_csv(file="DataRegressBook/Chap3/leuk.csv")

plot(survfit(Surv(time, cens) ~ group, data = leuk), main = "Acute Lymphoblastic Leukemia", lty = c(2,1), ylab = "Probability", xlab = "Time weeks", legend.text = c("Control", "6-MP"), legend.bty = "n")
plot(survfit(Surv(time, cens) ~ group, data = leuk), main = "Acute Lymphoblastic Leukemia", lty = c(1,2) )
plot(survfit(Surv(time, cens) ~ group, data = leuk), main = "Acute Lymphoblastic Leukemia", lty = c(1,2), ylab = "Probability", xlab = "Time weeks")
survdiff(Surv(time, cens) ~ group, data=leuk)
#
g3 <- subset(glioma, histology == "Grade3")
g4 <- subset(glioma, histology == "GBM")
#
plot(survfit(Surv(time, event) ~ group, data = g3), main = "Grade III Glioma", lty = c(2,1), ylab = "Probability", xlab = "Survival Months")
plot(survfit(Surv(time, event) ~ group, data = g4), main = "Grade IV Glioma", lty = c(2,1), ylab = "Probability", xlab = "Survival Months")
# probability estimation with exact distribution
logrank_test(Surv(time, event) ~ group, data = g3, distribution = "exact")
logrank_test(Surv(time, event) ~ group, data = g4, distribution = "exact")
#
logrank_test(Surv(time, event) ~ group | histology, data = glioma, distribution = approximate(B = 10000))
#
# With ggplot using the survminer package 
# For the ALL example
fit <- survfit(Surv(time, cens) ~ group, data = leuk)
ggsurvplot(fit, data = leuk)
# marking the cenros points on the 6-MP data
ggsurvplot(fit, data = leuk, censor.shape="|", censor.size = 4, linetype = c(1,2))
# More comlex visualization. 
ggsurvplot(
  fit, 
  data = leuk, 
  size = 0.5,                 # change line size
  linetype = c("solid", "dashed"), # different line type
  palette = c("lancet"), # color red, blue or custom palettes lancet
  title    = "Acute Lymphoblastic Leukemia", # plot main title
  xlab = "Time in weeks",   # customize X axis label.
  conf.int = TRUE,          # Add confidence interval
  pval = TRUE,              # Add p-value from log-rank test
  risk.table = TRUE,        # Add risk table
  risk.table.col = "strata",# Risk table color by groups
  legend.labs = c("6-MP", "Control"),    # Change legend labels
  risk.table.height = 0.25, # Useful to change when you have multiple groups
  surv.median.line = "hv",  # add the median survival pointer.
  ggtheme = theme_bw()      # Change ggplot2 theme
)
# For the glioma example
ggsurvplot(
  survfit(Surv(time, event) ~ group, data = g3), 
  data = g3, 
  size = 0.5,                 # change line size
  linetype = c("dashed", "solid"), # different line type
  palette = c("red","blue"), # color red, blue or custom palettes lancet
  title   = "Grade III Glioma", # plot main title
  xlab = "Survival in Months", # customize X axis label.
  conf.int = TRUE,          # Add confidence interval
  pval = TRUE,              # Add p-value from log-rank test
  risk.table = TRUE,        # Add risk table
  risk.table.col = "strata",# Risk table color by groups
  legend.labs = c("Control","RIT"),    # Change legend labels
  risk.table.height = 0.25, # Useful to change when you have multiple groups
  surv.median.line = "hv",  # add the median survival pointer.
  ggtheme = theme_bw()      # Change ggplot2 theme
)
# # #
# For the other example pf glioma
ggsurvplot(
survfit(Surv(time, event) ~ group, data = g4), 
data = g4, 
size = 0.5,                 # change line size
linetype = c("dashed", "solid"), # different line type
palette = c("red","blue"), # color red, blue or custom palettes lancet
title   = "Grade IV Glioma", # plot main title
xlab = "Survival in Months", # customize X axis label.
conf.int = TRUE,          # Add confidence interval
pval = TRUE,              # Add p-value from log-rank test
risk.table = TRUE,        # Add risk table
risk.table.col = "strata",# Risk table color by groups
legend.labs = c("Control","RIT"),    # Change legend labels
risk.table.height = 0.25, # Useful to change when you have multiple groups
surv.median.line = "hv",  # add the median survival pointer.
ggtheme = theme_bw()      # Change ggplot2 theme
)
#
# Example with more than one variable Colon data from the survival package
#
data(colon)
fit <- survfit(Surv(time,status)~rx, data=colon)
ggsurvplot(fit, data = colon, size = 0.5, linetype = "strata", legend = c(0.2, 0.2), break.time.by = 500)
# more complicated
ggsurvplot(fit, data = colon, 
           size = 0.5, 
           linetype = "strata", 
           legend = c(0.3, 0.2), 
           break.time.by = 500,
           pval = TRUE,
           palette = "lancet",
           risk.table = TRUE, 
           risk.table.y.text.col = TRUE
          )
