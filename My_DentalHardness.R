df_dental
library(lattice)

aovht <- aov(df_dental$hardness~df_dental$temperature)
aovht


attach(df_dental)

df_dental$temperature <- factor(df_dental$temperature)
df_dental$method <- factor(df_dental$method)
df_dental$alloy <- factor(df_dental$alloy)
df_dental$dentist <- factor(df_dental$dentist)


summary(df_dental)
str(df_dental)

boxplot(df_dental$hardness ~ df_dental$temperature)

hist(df_dental[df_dental$temperature==1500,]$hardness)
hist(df_dental[df_dental$temperature==1600,]$hardness)
hist(df_dental[df_dental$temperature==1700,]$hardness)


shapiro.test(df_dental[df_dental$temperature==1500,]$hardness)$p.value
shapiro.test(df_dental[df_dental$temperature==1600,]$hardness)$p.value
shapiro.test(df_dental[df_dental$temperature==1700,]$hardness)$p.value


aovht1 <- aov(df_dental$hardness~df_dental$temperature)
aovht1

summary(aovht1)
