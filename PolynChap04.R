# Examples form CAR book
# Polynomial regression and Splines
# # #
# will use the SLID data in carData sbout the income in Canadian population
library(tidyverse)
library(multcomp)
library(car)

summary(SLID)
nrow(SLID)

scatterplot(log(wages) ~ age, data=SLID, subset = age >= 18 & age <= 65, pch=".")
# ggplot(data = SLID) + geom_point(mapping = aes(x = age, y = log(wages)))

mod.quad.1 <- lm(log(wages) ~ poly(age, 2, raw=TRUE), data=SLID, subset = age >= 18 & age <= 65)
S(mod.quad.1)

mod.quad.2 <- update(mod.quad.1, ~ age + I(age^2))
S(mod.quad.2)

mod.quad.3 <- update(mod.quad.1, ~ poly(age, 2))
S(mod.quad.3)

plot(predictorEffects(mod.quad.1, residuals=TRUE), partial.residuals=list(cex=0.35, col=gray(0.5), lty=2))


# Regression Splines the splines library is part of R but it is not loaded

library("splines")

#

mod.spline <- update(mod.quad.1, ~ ns(education, df=5))
plot(predictorEffects(mod.spline, residuals=TRUE), partial=list(cex=0.35, col=gray(0.5), lty=2))


# for the tamble 4.21 in the book.
hers <- read_csv("DataRegressBook/Chap3/hersdata.csv")
hers <- mutate(hers, nonwhite = factor(nonwhite))
hers <- mutate(hers, smoking = factor(smoking))
hers <- mutate(hers, drinkany = factor(drinkany))

modHDL_spline <- lm(HDL ~ ns(BMI, df=5) + age10 + nonwhite + smoking + drinkany, data = hers)
S(modHDL_spline)
confint(modHDL_spline)
# scatter plot of the sbp data over weight 
dfbetas(modHDL_spline)
ggplot(data = hers) + geom_point(mapping = aes(x = BMI, y = HDL))
