# Exer. 8.2.3 Daniel
# Patients suffering from rheumatic diseases or osteoporosis often suffer critical loss in bone
# mineral density (BMD).
#
Exr8.2.2 <- read_csv(file="EXR_C08_S02_02.csv")
ggplot(data=Exr8.2.2, aes(group=GROUP, y=BMD)) + geom_boxplot()
Exr8.2.2_M <- Exr8.2.2 %>%
  + mutate(GROUP = case_when(
    + GROUP == 1 ~ "RA",
    + GROUP == 2 ~ "LUPUS",
    + GROUP == 3 ~ "PMRTA",
    + GROUP == 4 ~ "OA",
    + GROUP == 5 ~ "O" ))
ggplot(data=Exr8.2.2_M, aes(x=GROUP, y=BMD)) + geom_boxplot()
Exr2_2.aov <- aov(BMD ~ GROUP, data=Exr8.2.2_M)
summary(Exr2_2.aov)
#
# Exer 8.2.3
#
Exr8.2.3 <- read_csv(file="EXR_C08_S02_03.csv")
ggplot(data=Exr8.2.3, aes(x=Group, y=calcium)) + geom_boxplot()
Exr3.aov <- aov(calcium ~ Group, data=Er8.2.3)
summary(Exr3.aov)
TukeyHSD(Exr3.aov)
plot(TukeyHSD(Exr3.aov), las=2)