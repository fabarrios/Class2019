# Form the Cox regression appendix in the Car book
# 
# Example from Rossi and a finantial aid model
# To study the fin variable

data("Rossi", package = "carData")

mod.allison <- coxph(Surv(week, arrest) ~ fin + age + race + wexp + mar + paro + prio, data = Rossi)
summary(mod.allison)

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

Rossi.2 <- unfold(Rossi, time="week", event="arrest", cov=11:62, cov.names="employed")
mod.allison.2 <- coxph(Surv(start, stop, arrest.time) ~ fin + age + race + wexp + mar + paro + prio + employed, data=Rossi.2)
summary(mod.allison.2)

# the use of unfold
Rossi.3 <- unfold(Rossi, "week", "arrest", 11:62, "employed", lag=1)
mod.allison.3 <- coxph(Surv(start, stop, arrest.time) ~ fin + age + race + wexp + mar + paro + prio + employed, data=Rossi.3)
summary(mod.allison.3)

# Model testing, scalled daown version of the Rossi data
mod.allison.4 <- coxph(Surv(week, arrest) ~ fin + age + prio, data=Rossi)
mod.allison.4

cox.zph(mod.allison.4)
par(mfrow=c(2, 2))
plot(cox.zph(mod.allison.4))

mod.allison.5 <- coxph(Surv(start, stop, arrest.time) ~ fin + age + age:stop + prio, data=Rossi.2)
mod.allison.5
