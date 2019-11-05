# Examples HSAUR2 Logistic regression.
# Erythrocyte sedimentation rate (ESR). ESR increases when protein levels rise
# associated with chronic infectons, rheumatic conditions, etc
# the level of ESR is not important as long as it is < 20mm/hr (healthy)
# to asses ESR as useful diagnostic tool.
# ESR > 20mm/hr for proteins fibrinogen and gamma globulin, using glm
library(tidyverse)
library(HSAUR2)
library(emmeans)
library(multcomp)
#
data("plasma", package = "HSAUR2")
layout(matrix(1:2, ncol = 2))
cdplot(ESR ~ fibrinogen, data = plasma)
cdplot(ESR ~ globulin, data = plasma)
# glm general linear model default is logistic for binomial distrbution
plasma_glm01 <- glm(ESR ~ fibrinogen, data = plasma, family = binomial())
summary(plasma_glm01)
# coeff fibrinogen is sifnificative 5%
# one unit change in this variable increases the log-odds in favor of
# ESR > 20mm/hr
confint(plasma_glm01, parm = "fibrinogen")
exp(coef(plasma_glm01)["fibrinogen"])
exp(confint(plasma_glm01, parm = "fibrinogen"))
anova(plasma_glm01, test = "Chisq")
# full model with two variables
plasma_glm02 <- glm(ESR ~ fibrinogen + globulin, data = plasma, family = binomial())
summary(plasma_glm02)
# Comparing the resudial deviance of the models:
# residual deviance 01: 24.84 residual deviance 02: 22.971 -> 1.869 (1.87)
# to test for significance R take the lgm with a chisq
# the 1.87 -> globulin has no influence in the ESR
anova(plasma_glm01, plasma_glm02, test = "Chisq")
# Estimates conditional probability of a ESR > 20 for all observations
prob <- predict(plasma_glm02, type = "response")
layout(matrix(1:1, ncol = 1))
plot(globulin~ fibrinogen, data = plasma, xlim = c(2, 6), ylim = c(25, 55), pch = ".")
symbols(plasma$fibrinogen, plasma$globulin, circles = prob, add = TRUE)
#
data("womensrole", package = "HSAUR2")
fmod <- cbind(agree, disagree) ~ gender + education
womensrole_glm01 <- glm(fmod, data = womensrole, family = binomial())
summary(womensrole_glm01)
role.fitted01 <- predict(womensrole_glm01, type = "response")
# definition of a plot of a fitted object
myplot <- function(role.fitted) {
  f <- womensrole$gender == "Female"
  plot(womensrole$education, role.fitted, type = "n", ylab= "Probability of agreeing", xlab= "Education", ylim = c(0,1))
  lines(womensrole$education[!f], role.fitted[!f], lty = 1)
  lines(womensrole$education[f], role.fitted[f], lty = 2)
  lgtxt <- c("Fitted (Males)", "Fitted (Females)")
  legend("topright", lgtxt, lty = 1:2, bty = "n")
  y <- womensrole$agree / (womensrole$agree + womensrole$disagree)
  text(womensrole$education, y, ifelse(f, "\\VE", "\\MA"), family = "HersheySerif", cex = 1.25)
}
#
myplot(role.fitted01)
# Now with interaction terms
fm02 <- cbind(agree, disagree) ~ gender * education
womensrole_glm02 <- glm(fm02, data = womensrole, family = binomial())
summary(womensrole_glm02)
role.fitted02 <- predict(womensrole_glm02, type = "response")
myplot(role.fitted02)
plot(womensrole_glm02)
# To estimate de fit, we will use deviace residual
# 
res <- residuals(womensrole_glm02, type = "deviance")
plot(predict(womensrole_glm02), res, xlab= "Fitted values", ylab= "Residuals", ylim = max(abs(res)) * c(-1,1))
abline(h = 0, lty = 2)
#
# Examples chp 5
# from the data WCGS, the modle for the Cardiac H Diseace CHD table 5.2
wcgs <- read_csv(file="DataRegressBook/Chap2/wcgs.csv")
wcgs %>%
    select(chd69, age) %>%
    summary()
wcgs <- mutate(wcgs, chd69 = factor(chd69))
CHD_glm01 <- glm(chd69 ~ age, data = wcgs, family = binomial())
summary(CHD_glm01)
confint(CHD_glm01, parm = "age")
exp(coef(CHD_glm01)["age"])
anova(CHD_glm01, test = "Chisq")
# For the model of CHD risc for the presence or arcus table 5.4
wcgs %>%
    select(arcus) %>%
    summary()
wcgs %>% mutate(wcgs, arcus = factor(arcus))
CHDarc_glm01 <- glm(chd69 ~ arcus, data = wcgs, family = binomial())
# CHDarc_glm01 <- glm(chd69 ~ arcus, data = filter(wcgs, !is.na(arcus)), family = binomial())
summary(CHDarc_glm01)
exp(coef(CHDarc_glm01)["arcus"])
exp(confint(CHDarc_glm01, parm = "arcus"))
#
wcgs <- wcgs %>%
    mutate(arcus_f = fct_recode(factor(arcus),
                                "Arcus senilis" = "1",
                                "No arcus senilis" = "0"),
           arcus_f = fct_relevel(arcus_f, "Arcus senilis"))
wcgs %>%
    filter(complete.cases(arcus_f, sbp, weight)) %>%
    ggplot(aes(x = weight, y = sbp, group = arcus_f)) +
    geom_point(size=2, shape = 1) + 
    stat_smooth(method=lm, color="red") +
    stat_smooth(method=loess, se=FALSE, color="blue") +
    labs(title = "SBP vs. Weight by Arcus Senilis status",
         caption = "WCGStudy subjects with arcus senilis status") + 
    facet_wrap(~ arcus_f) +
    theme_bw()
CHDarc_glm02 <- glm(chd69 ~ arcus_f, data = wcgs, family = binomial())
summary(CHDarc_glm01)
# For the example in table 5.5 Logistic model of CHD and age as categorical factor
CHDagec_glm <- glm(chd69 ~ agec , data = wcgs, family = binomial())
summary(CHDagec_glm)
exp(coef(CHDagec_glm))
#
CHDagec_lstsqr <- emmeans(CHDagec_glm, "agec")
# Contrasts
Contrasts_CHDagec = list(oneVSbase   = c(-1, 1, 0, 0, 0),
                         twoVSbase   = c(-1, 0, 1, 0, 0),
                         threeVSbase = c(-1, 0, 0, 1, 0),
                         fourVSbase  = c(-1, 0, 0, 0, 1))
# Following estimate the coeficients contrasts in the log odds ratio
contrast(CHDagec_lstsqr, Contrasts_CHDagec, adjust="sidak")
contrast(CHDagec_lstsqr, Contrasts_CHDagec, adjust="bonferroni")
#

# For multiple logistic model for CHD risk
CHDmult_glm <- glm(chd69 ~ age + chol + bmi + sbp + smoke , data = wcgs, family = binomial())
summary(CHDmult_glm)
# For chp 5 the example of interactions for continuous and categorical variables.
# example of table 5.16 with age and arcus, with age as continuous
CHDaa_glm <- glm(chd69 ~ arcus * age , data = wcgs, family = binomial())
summary(CHDaa_glm)
