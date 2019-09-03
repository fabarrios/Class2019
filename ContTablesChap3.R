# #############################################################################
# Contingency tables
# frequency variables
# Two-bytwo contngency table for CHD and corneal arcus
# in the WCGS data set for CHD
library(tidyverse)
library(gmodels)
library(car)
#
wcgs <- read_csv(file="DataRegressBook/Chap2/wcgs.csv")
tab_arcus <- table(wcgs$chd69, wcgs$arcus, dnn = c('CHD','arcus'))
ftab_arcus <- ftable(chd69~arcus~chd69, data= wcgs, dnn = c('CHD','arcus'))
CrossTable(wcgs$chd69, wcgs$arcus, dnn = c('CHD','arcus'))
chisq.test(tab_arcus)
# chisq.test(tab_wcgs, simulate.p.value = TRUE)

# Multiple categories
#
# for the example in 3.7
tab_agec <- ftable(agec~chd69, data=wcgs, dnn = c('CHD', 'agec'))
chisq.test(tab_agec)
CrossTable(wcgs$chd69, wcgs$agec, dnn = c('CHD','arcus'))
# Fisher exact test, for small data table Chp 3 example table 3.6 Female
# partner's HIV status by AIDS dignosis of male partner
#
female.partner = matrix(c(3,4,2,22), nrow = 2)
fisher.test(female.pertner)
# Now with examples from https://thomaselove.github.io/431notes-2017/WCGS-Study.html
#
wcgs %>%
    dplyr::select(sbp, age, weight, height) %>%
    cor() %>%      # obtain correlation coefficients for this subgroup
    signif(., 3)   # round them off to three significant figures before printing
# With the pairs function
pairs (~ sbp + age + weight + height, data=wcgs, main="Simple Scatterplot Matrix")

# Now with the car package using scatterplotMatrix function
car::scatterplotMatrix(~ sbp + age + weight + height, 
                       data=wcgs, 
                       main="Scatterplot Matrix via car")
# Examples taken from T. Love and couple of functions
# With examples from the  Chang’s R Graphics Cookbook, called panel.hist and panel.cor.
# panel.hist and panel.cor modified from Chang
`panel.hist` <- function(x, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

`panel.cor` <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- cor(x, y)
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * (1 + abs(r)) / 2)
}
# Using these two functions we have the folowing
pairs (~ sbp + age + weight + height, data=wcgs, 
       main="Augmented Scatterplot Matrix", 
       upper.panel = panel.smooth,
       diag.panel = panel.hist,
       lower.panel = panel.cor)

# For ggplot2 the Gally package has scatterplot for correlation matrices
GGally::ggcorr(select(wcgs, sbp, age, weight, height), 
               name = "Pearson r", label = TRUE)
# And has a ggpairs with density plots
GGally::ggpairs(select(wcgs, sbp, age, weight, height), 
                title = "Scatterplot Matrix via ggpairs")
