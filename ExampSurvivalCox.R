# Example from Glioma Radioimmunotherapy
# Chapter 11 HSAUR 3
#
library("tidyverse")
library("car")
library("survival")
library("survminer")
library("coin")

data("glioma", package = "coin")

# first we split the screen in two
layout(matrix(1:2, ncol=2))
g3 <- subset(glioma, histology == "Grade3")
g4 <- subset(glioma, histology == "GBM")

plot(survfit(Surv(time, event) ~ group, data = g3), main = "Grade III Glioma", lty = c(2,1), ylab = "Probability", xlab = "Survival Months")
plot(survfit(Surv(time, event) ~ group, data = g4), main = "Grade IV Glioma", lty = c(2,1), ylab = "Probability", xlab = "Survival Months")

# Patients treated with the novel Radioimmunotherapy survive longer
# to assess if the in the plot is right we can perform
# a log-rank test
survdiff(Surv(time, event) ~ group, data = g3)
# to runn an exact test Coin has a permutation funtion that can calculate an exact test
# defunct surv_test(Surv(time, event) ~ group, data = g3, distribution = "exact")
# defunct surv_test(Surv(time, event) ~ group, data = g4, distribution = "exact")
# More interesting question is the novel Radioimmunotherapy is superior for both groups
# is done stratifying od blocking (by block) with respect to tumor grading 
# defunct surv_test(Surv(time, event) ~ group | histology, data = glioma, distribution = approximate(B = 10000))
# probability estimation with exact distribution
logrank_test(Surv(time, event) ~ group, data = g3, distribution = "exact")
logrank_test(Surv(time, event) ~ group, data = g4, distribution = "exact")
# To answer the question whether the survival times differ for grade III patients 
logrank_test(Surv(time, event) ~ group | histology, data = glioma, distribution = approximate(nresample = 10000L))

ggsurvplot(
  survfit(Surv(time, event) ~ group, data = g3), 
  data = g3, 
  size = 0.5,                      # change line size
  linetype = c("dashed", "solid"), # different line type
  palette = "lancet",              # color red, blue or custom palettes lancet
  title   = "Grade III Glioma",    # plot main title
  xlab = "Survival in Months",     # customize X axis label.
  conf.int = TRUE,                 # Add confidence interval
  pval = TRUE,                     # Add p-value from log-rank test
  risk.table = TRUE,               # Add risk table
  risk.table.col = "strata",       # Risk table color by groups
  legend.labs = c("Control","Treated"),# Change legend labels
  risk.table.height = 0.30,        # Useful to change when you have multiple groups
  surv.median.line = "hv",         # add the median survival pointer.
  ggtheme = theme_bw()             # Change ggplot2 theme
)

# Other survival plots with ggplot
ggsurvplot(
survfit(Surv(time, event) ~ group, data = g4), 
data = g4, 
size = 0.5,                  # change line size
linetype = c("dashed", "solid"), # different line type
palette = "lancet",          # color red, blue or custom palettes lancet
title   = "Grade IV Glioma", # plot main title
xlab = "Survival in Months", # customize X axis label.
conf.int = TRUE,             # Add confidence interval
pval = TRUE,                 # Add p-value from log-rank test
risk.table = TRUE,           # Add risk table
risk.table.col = "strata",   # Risk table color by groups
legend.labs = c("Control","Treated"),    # Change legend labels
risk.table.height = 0.30,    # Useful to change when you have multiple groups
surv.median.line = "hv",     # add the median survival pointer.
ggtheme = theme_bw()         # Change ggplot2 theme
)

#Cox proportional-hazards regression
#library ("carData")
data("Rossi", package = "carData")
args(coxph)
Rossi[1:5, 1:10]
# Cox model and estimation of model tests
mod.allison <- coxph(Surv(week, arrest) ~ fin + age + race + wexp + mar + paro + prio, data = Rossi)
summary(mod.allison)
# car has an ANOVA to estimate Type-II likelihood test for Cox models
Anova(mod.allison)

# Plots of the Cox model
plot(survfit(mod.allison), xlab="Weeks", ylab="Proportion Not Rearrested")
plot(survfit(mod.allison), ylim = c(0.7, 1), xlab="Weeks", ylab="Proportion Not Rearrested")

ggsurvplot(
survfit(mod.allison), 
data = Rossi, 
size = 0.5,                      # change line size
linetype = c("solid","dashed"), # different line type
palette = "simpsons",              # color palette
title   = "Rossi data",          # plot main title
xlab = "Weeks",                  # customize X axis label.
ylab = "Proportion Not Rearrested", # customize Y axis label
ylim = c(0.7, 1),             # customize Y limits
conf.int = TRUE,             # Add confidence interval
pval = FALSE,                 # Add p-value from log-rank test
risk.table = FALSE,           # Add risk table
risk.table.col = "strata",   # Risk table color by groups
surv.median.line = "none",
legend = "none",
risk.table.height = 0.25,    # Useful to change when you have multiple groups
ggtheme = theme_bw()         # Change ggplot2 theme
)

# To study the fin variable
Rossi.fin <- with (Rossi, data.frame(fin=c(0, 1), age=rep(mean(age), 2), race=rep(mean(race == "other"), 2),
                                     wexp=rep(mean(wexp == "yes"), 2), mar=rep(mean(mar == "not married"), 2),
                                     paro=rep(mean(paro == "yes"), 2), prio=rep(mean(prio), 2)))
plot(survfit(mod.allison, newdata=Rossi.fin), conf.int=TRUE, lty=c(1,2), ylim=c(0.6, 1), xlab="Weeks", 
     ylab="Proportion Not Rearrested")
legend("bottomleft", legend=c("fin - no", "fin = yes"), lty=c(1, 2), insert=0.02)

ggsurvplot(
  survfit(mod.allison, newdata=Rossi.fin), 
  data = Rossi, 
  size = 0.5,                     # change line size
  linetype = c("solid","dashed"), # different line type
  palette = "lancet",             # color palette
  title   = "Rossi data",         # plot main title
  xlab = "Weeks",                 # customize X axis label.
  ylab = "Proportion Not Rearrested", # customize Y axis label
  ylim = c(0.7, 1),           # customize Y limits
  conf.int = TRUE,            # Add confidence interval
  pval = FALSE,               # Add p-value from log-rank test
  risk.table = FALSE,         # Add risk table
  risk.table.col = "strata",  # Risk table color by groups
  surv.median.line = "none",
  legend = "none",
  risk.table.height = 0.25,   # Useful to change when you have multiple groups
  ggtheme = theme_bw()        # Change ggplot2 theme
  )

# Efects of hormonal treatment in women suffering from node-possitive breast cancer, from 
# the German Breast Cancer Study Group 2, GBSG2.  The hypothetical prognotic factors are age,
# menopausal status, tumor size, tumor grade, number of possitive lymph nodes, progesterone 
# receptor, estrogen receptor, and if hormonal therapy was applied.
# GBSG2 data from the TH.data package.

data("GBSG2", package = "TH.data")
ggsurvplot(
  survfit(Surv(time, cens) ~ horTh, data = GBSG2), 
  data = GBSG2, 
  size = 0.5,                     # change line size
  linetype = c("solid","dashed"), # different line type
  palette = "lancet",             # color palette
  title   = "Hormonal Therapy",   # plot main title
  xlab = "Survival Time in Days", # customize X axis label.
  ylab = "Probability",           # customize Y axis label
  ylim = c(0.0, 1.0),             # customize Y limits
  conf.int = FALSE,            # Add confidence interval
  pval = FALSE,               # Add p-value from log-rank test
  risk.table = FALSE,         # Add risk table
  risk.table.col = "strata",  # Risk table color by groups
  surv.median.line = "none",
  legend.labs = c("No","Yes"),
  risk.table.height = 0.25,   # Useful to change when you have multiple groups
  ggtheme = theme_bw()        # Change ggplot2 theme
  )

GBSG2_coxph <- coxph(Surv(time, cens) ~ ., data = GBSG2)
summary(GBSG2_coxph)
ci <- confint(GBSG2_coxph)
exp(cbind(coef(GBSG2_coxph), ci))["horThyes",]
# exp for the Cox regression coeficient for hormonal therapy
(GBSG2_zph <- cox.zph(GBSG2_coxph))

plot(GBSG2_zph, var = "age")
plot(GBSG2_zph, var = "tsize")
plot(GBSG2_zph, var = "pnodes")

# Martingale residuas areestimated by the residuals 
layout(matrix(1:3, ncol = 3))
res <- residuals(GBSG2_coxph)
plot(res ~ age, data = GBSG2, ylim = c(-2.5, 1.5), pch = ".", ylab = "Martingale Residuals")
abline(h = 0, lty = 2)
plot(res ~ pnodes, data = GBSG2, ylim = c(-2.5, 1.5), pch = ".", ylab = "")
abline(h = 0, lty = 2)
plot(res ~ log(progrec), data = GBSG2, ylim = c(-2.5, 1.5), pch = ".", ylab = "")
abline(h = 0, lty = 2)

# In R tree-structure regression models are aplicable to censored responses in survival analysis. The
# ctree function from package partykit

GBSG2_ctree <- ctree(Surv(time, cens) ~ ., data = GBSG2)
plot(GBSG2_ctree)

# in the tree-structure regression the most important variable is the pnodes, number of possitve lymph 
# nodes, corresponding to the p-value associated with the varable in the Cox regression.
