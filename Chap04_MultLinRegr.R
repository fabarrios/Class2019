# Multiple linear linear regression
#
# Chapter 4 examples. 4.1
library(tidyverse)
hers <- read_csv("DataRegressBook/Chap3/hersdata.csv")
hers_nodi <- filter(hers, diabetes == "no")
# Simple linear model with HERS data for women without diabetes
ggplot(data = hers_nodi, mapping = aes(x = exercise, y = glucose)) + 
  geom_boxplot(na.rm = TRUE) + facet_grid( . ~ diabetes) + geom_jitter(height = 0.15, width = 0.15)
# The simple linear model adjust the exercise like in table 4.1
hers_nodi_Fit <- lm(glucose ~ exercise, data = hers_nodi)
summary(hers_nodi_Fit)
# and for obtaining the table 4.2 with multiple linear model
hers_nodi_Fit2 <- lm(glucose ~ exercise + age + drinkany + BMI, data = hers_nodi)
summary(hers_nodi_Fit2)
#
