# Multiple linear regression
# 
#
# Examples from the HERS data set in the chapter 4
library(car)
library(emmeans)
library(tidyverse)

# we will use the library effects also, it uses lattice

# Chapter 4 examples. 4.1
hers <- read_csv("DataRegressBook/Chap3/hersdata.csv")
hers_nodi <- filter(hers, diabetes == "no")
# Simple linear model with HERS data for women without diabetes
ggplot(data = hers_nodi, mapping = aes(x = exercise, y = glucose)) + 
  geom_boxplot(na.rm = TRUE) + facet_grid( . ~ diabetes) + geom_jitter(height = 0.15, width = 0.15)
# The simple linear model adjust the exercise like in table 4.1
hers_nodi_Fit <- lm(glucose ~ exercise, data = hers_nodi)
summary(hers_nodi_Fit)
S(hers_nodi_Fit)

# Chap 4 4.2 Multiple linear regresor model
# and to obtain the table 4.2 with multiple linear model

hers_nodi_Fit2 <- lm(glucose ~ exercise + age + drinkany + BMI, data = hers_nodi)
S(hers_nodi_Fit2)

# Chap 4  4.3 Categorical predictors
# we are using the same file hers <- read_csv("DataRegressBook/Chap3/hersdata.csv")
# Multilevel categorical predictors using the linear model for women without diebetes
# To get table 4.4 Regression of physical activity on glucose

hers_nodi <- mutate(hers_nodi, physact = factor(physact, levels=c("much less active","somewhat less active","about as active","somewhat more active","much more active")))
levels(hers_nodi$physact)
ggplot(data = hers_nodi, mapping = aes(x = physact, y = glucose)) + geom_boxplot(na.rm = TRUE)
glucose_fit_act <- lm(glucose ~ physact, data = hers_nodi)

# betaStar <- coef(glucose_fit_act)
# betaStar
# Xstar <- model.matrix(glucose ~ physact, data = hers_nodi)
# and the Covariant matrix
# Cov_glucose_betaStar <- vcov(glucose_fit_act)
# Where the square roots of the diagonal elements are the standart errors 
# sqrt(diag(Cov_glucose_betaStar))
# Using the car Anova function

Anova(glucose_fit_act, type="II")
S(glucose_fit_act)
layout(matrix(1:4, nrow = 2))
plot(glucose_fit_act)

glucose_emmeans <- emmeans(glucose_fit_act, "physact")

# Contrasts
# contrasts using the adjusted parameters for a categorical variable with several categories
# or multipe oridinals
# R call categorical variables factors and their categories levels
# so we have factors with different levels
# in these cases we can estimate contrasts of the adjueted parameters.

Contrasts_glu = list(MAvsLA          = c(-1, -1, 0,  1,  1),
                     MAvsLAforMuch   = c(-1,  0, 0,  0,  1),
                     MAvsLAforSome   = c( 0, -1, 0,  1,  0),
                     MLAvsC          = c(-1,  0, 1,  0,  0),
                     SLAvsC          = c( 0, -1, 1,  0,  0),
                     SMAvsC          = c( 0,  0,-1,  1,  0),
                     MMAvsC          = c( 0,  0,-1,  0,  1),
                     LinTrend_phys   = c(-2, -1, 0,  1,  2))

# compare the results with emmeans adjusted with Sidak, FWE.

contrast(glucose_emmeans, Contrasts_glu, adjust="sidak")

# With adjust="none", results will be the same as the aov method.
# For the Bonferroni correction adjust "bonferroni" this is talbe 4.6

contrast(glucose_emmeans, Contrasts_glu, adjust="bonferroni")

# Same cotrasts with multicomp library
Input = ("
Contrast.Name     MLA SLA AAA SMA MMA
 MAvsLA           -1  -1   0   1   1
 MAvsLAforMuch    -1   0   0   0   1
 MAvsLAforSome     0  -1   0   1   0
 MLAvsC           -1   0   1   0   0
 SLAvsC            0  -1   1   0   0
 SMAvsC            0   0  -1   1   0
 MMAvsC            0   0  -1   0   1
 LinearTrending   -2  -1   0   1   2
")
Cont_glucose_Matriz = as.matrix(read.table(textConnection(Input), header=TRUE, row.names=1))
Cont_glucose_Matriz
G = glht(glucose_fit_act, linfct = mcp(physact = Cont_glucose_Matriz))
G$linfct
summary(G, test=adjusted("single-step"))

# From the CAR book exmples of chap 4.
# Multiple linear regression, extends the regular linear regression to more than one predictor.
# From the Canadian occupational-prestige data
summary(Prestige)
# a multiple linear model of prestige on education and the log base 2 of the salary and women.
# result in partial slopes

prestige.mod <- lm(prestige ~ education + log2(income) + women,
    data=Prestige)
S(prestige.mod)

# predictor effect plots, graphs that can be used to visualize the effect of each of the predictors
# in a fitted predictor model.
# effects library uses the lattice package, that uses theme that controls many grafical elements like line type 
# and colors, etc. help("effectsTheme")

library(effects)
plot(predictorEffects(prestige.mod))

# Factors in linear regression
# Cathegorical variables

Prestige$type
class(Prestige$type)

Prestige$type <- factor(Prestige$type, levels=c("bc", "wc", "prof"))
levels(Prestige$type)

# Linear model with one factor: one-way ANOVA
# The simplest linear model with factors known as "one-way analisis of variance"
# one factor and no numeric predictors.

summary(Baumann[, c(1, 6)]) 
xtabs(~ group, data=Baumann)

Tapply(post.test.3 ~ group, mean, data=Baumann)
Tapply(post.test.3 ~ group, sd, data=Baumann)

plot(post.test.3 ~ group, data=Baumann, xlab="Group",
    ylab="Reading Score")

S(baum.mod.1 <- lm(post.test.3 ~ group, data=Baumann))
emmeans(baum.mod.1, pairwise ~ group)
emmeans(baum.mod.1, trt.vs.ctrl ~ group)

# Extracting women and adding type
prestige.mod.1 <- update(prestige.mod, ~ . - women + type)
S(prestige.mod.1)

# beta 1,2,3 sl 1,2 two digits

bhat <- coef(prestige.mod.1)
int1 <- round(bhat[1], 2)
int2 <- round(bhat[1] + bhat[4], 2)
int3 <- round(bhat[1] + bhat[5], 2)
sl1 <- round(bhat[2], 2)
sl2 <- round(bhat[3], 2)
bhat <- round(bhat, 2)

plot(predictorEffects(prestige.mod.1, 
      predictors = ~ type + education + income))

emmeans(prestige.mod.1, pairwise ~ type)$contrasts

# Confounding 
# when one predictor (independet variable) can depend in some of the 
# same variables that the responce function
# Example 4 
# Example of BMI (Body mass index) and LDL (colesterol) using the HERS cohort data Table 4.12

LDL_fit_bmi <- lm(LDL ~ BMI, data = hers)
S(LDL_fit_bmi)
LDL_fit_all <- lm(LDL ~ BMI + age + nonwhite + smoking + drinkany, data = hers)
S(LDL_fit_all)

# Chapter 4, Interaction
# Now for the group with Hormone Therapy the complete HERS cohort
# Model of cholesterol LDL and the effect of Hormone Therapy (HT) and
# Statin use model is at the one year visit (to se the HT effect)

library(MASS)
library(multcomp)

hers <- mutate(hers, HT = factor(HT))
hers <- hers %>% mutate(HT = relevel(HT, ref = "placebo"))

# For the first year visit LDL levels at one year LDL1
LDL1_model <- lm(LDL1 ~ HT * statins, data = hers)
S(LDL1_model)

# For the table 4.15 of the book, to test for the linear 
# combination of the coefficients for the interaction to
# TEST the linear combination of the coefficients for HT
# and the statin interaction b1 and b3 the third part of table 4.15

coefeq <- matrix(data=0, nrow = 1, ncol = length(LDL_model$coefficients))
colnames(coefeq) <- names(LDL_model$coefficients)
coefeq
coefeq[1, "HThormone therapy"] <- 1
coefeq[1, "HThormone therapy:statinsyes"] <- 1
coefeq %*% LDL_model$coefficients

# coeftest <- glht(model= LDL_model, linfct= coefeq, rhs= 0, alternative= "greater")
coeftest <- glht(model= LDL_model, linfct= coefeq, rhs= 0)
summary(coeftest)

# Example of LDL after one year of HT and physact
# This is the table 4.16 of the book, effect of the hormone therapy
# combined with effects of physical activity at the one year visit
# now to estimate the linear combination of the coefficients
hers <- mutate(hers, physact = factor(physact, levels=c("much less active","somewhat less active","about as active","somewhat more active","much more active")))
LDL1phys_model <- lm(LDL1 ~ HT * physact, data = hers)
S(LDL1phys_model)

# The coefficients test for interaction
# can be done with glht()

coef_LDL1phys <- matrix(data=0, nrow = 1, ncol = length(LDL1phys_model$coefficients))
colnames(coef_LDL1phys) <- names(LDL1phys_model$coefficients)

# E[LDL|x] = b0 + b1 HT + b2 physact + b3 HT:physact
# we will look at slope = b3 BMIc; coeff = 0, 0, 0, 0, 0, 1, 1, 1, 1
# to test of significance

coef_LDL1phys[1, "HThormone therapy:physactsomewhat less active"] <- 1
coef_LDL1phys[1, "HThormone therapy:physactabout as active"] <- 1
coef_LDL1phys[1, "HThormone therapy:physactsomewhat more active"] <- 1
coef_LDL1phys[1, "HThormone therapy:physactmuch more active"] <- 1
coef_LDL1phys %*% LDLphys_model$coefficients
coef_LDL1phys
coef_LDL1phystest <- glht(model= LDL1phys_model, linfct= coef_LDL1phys, rhs= 0)
S(coef_LDL1phystest)

# Example of BMI and statins, with interaction
# Model of LDL and BMI*statins and other variables
hers <- mutate(hers, statins = factor(statins))
hers <- mutate(hers, nonwhite = factor(nonwhite))
hers <- mutate(hers, smoking = factor(smoking))
hers <- mutate(hers, drinkany = factor(drinkany))
hers <- mutate(hers, BMIc = BMI - mean(BMI, na.rm=TRUE))
LDLbmi_model <- lm(LDL ~ statins*BMIc + age + nonwhite + smoking + drinkany, data = hers)
summary(LDLbmi_model)

# For the table 4.17 of the book, to test for the linear 
# combination of the coefficients for the interaction to
# TEST the linear combination of the coefficients for HT

coef_LDLbmi <- matrix(data=0, nrow = 1, ncol = length(LDLbmi_model$coefficients))
colnames(coef_LDLbmi) <- names(LDLbmi_model$coefficients)

# E[LDL|x] = b0 + b1 statins + b2 BMIc + b3 statins:BMIc + b4 age
#           + b5 nonwhite + b6 smoking + b7 + drinkany
# we will look at slope = b2 + b3 BMIc; coeff = 0, 0, 1, 1, 0, 0, 0, 0

coef_LDLbmi[1, "statinsyes"] <- 0
coef_LDLbmi[1, "BMIc"] <- 1
coef_LDLbmi[1, "statinsyes:BMIc"] <- 1
coef_LDLbmi %*% LDLbmi_model$coefficients
coef_LDLbmi
coef_LDLbmitest <- glht(model= LDLbmi_model, linfct= coef_LDLbmi, rhs= 0)
summary(coef_LDLbmitest)

# Comp of regular linear model and robust and some standard error handling
#
glucose_model <- lm(glucose ~ diabetes + BMI + age + drinkany, data= hers)

# MASS rlm()
glucose_rmodel <- rlm(glucose ~ diabetes + BMI + age + drinkany, data= hers)
#
glucose_rmodel_bi <- rlm(glucose ~ diabetes + BMI + age + drinkany, psi= psi.bisquare, data= hers)
