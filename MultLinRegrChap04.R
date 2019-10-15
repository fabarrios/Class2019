# Multiple linear regression
# 
#
# Examples from the HERS data set in the chapter 4
library(car)
library(emmeans)
library(multcomp)
library(tidyverse)
# we will use the library effects also, it uses MASS

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

# and for obtaining the table 4.2 with multiple linear model
hers_nodi_Fit2 <- lm(glucose ~ exercise + age + drinkany + BMI, data = hers_nodi)
summary(hers_nodi_Fit2)

# Chap 4  4.4
# we are usingthe same file hers <- read_csv("DataRegressBook/Chap3/hersdata.csv")
# Multilever categorical multiple linear model for women without diebetes
# To get table 4.4 Regression of physical activity on glucose
# hers_nodi$physact <- factor(hers_nodi$physact)
# hers_nodi <- mutate(hers_nodi, physact = factor(physact))
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
Anova(glucose_fit_act, type="II")
summary(glucose_fit_act)
layout(matrix(1:4, nrow = 2))
plot(glucose_fit_act)

glucose_lstsqr <- emmeans(glucose_fit_act, "physact")

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
contrast(glucose_lstsqr, Contrasts_glu, adjust="sidak")
# compare the results with least-squares adjusted with sidak, FWE.
# With adjust="none", results will be the same as the aov method.
contrast(glucose_lstsqr, Contrasts_glu, adjust="bonferroni")

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
