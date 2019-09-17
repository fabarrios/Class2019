# Example from Glioma Radioimmunotherapy
# Chapter 11 HSAUR 3
#


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

#Cox proportional-hazards regression
#library ("carData")
args(coxph)
Rossi[1:5, 1:10]

mod.allison <- coxph(Surv(week, arrest) ~ fin + age + race + wexp + mar + paro + prio, data = Rossi)
summary(mod.allison)

# car has an ANOVA to estimate Type-II likelihood test for Cox models
Anova(mod.allison)
plot(survfit(mod.allison), xlab="Weeks", ylab="Proportion Not Rearrested")
plot(survfit(mod.allison), ylim = c(0.7, 1), xlab="Weeks", ylab="Proportion Not Rearrested")

