Cereal1

attach(Cereal1)

Cereal1$Months <- factor(Cereal1$Months , labels = c("0","1","2","3"))

hist(Cereal1[Cereal1$Months==0,]$CalciumContent)
hist(Cereal1[Cereal1$Months==1,]$CalciumContent)
hist(Cereal1[Cereal1$Months==2,]$CalciumContent)
hist(Cereal1[Cereal1$Months==3,]$CalciumContent)

shapiro.test(Cereal1[Cereal1$Months==0,]$CalciumContent)$p.value
shapiro.test(Cereal1[Cereal1$Months==1,]$CalciumContent)$p.value
shapiro.test(Cereal1[Cereal1$Months==2,]$CalciumContent)$p.value
shapiro.test(Cereal1[Cereal1$Months==3,]$CalciumContent)$p.value

# All the values of Shapiro test have p-value > 0.05. So do not reject Null Hypothesis

leveneTest(Cereal1$CalciumContent ~ Cereal1$Months)

# Do not reject Null Hypothesis

# Perform ANOVA

aovCereal <- aov(Cereal1$CalciumContent ~ Cereal1$Months)
summary(aovCereal)


TukeyHSD(aovCereal)
plot(TukeyHSD(aovCereal))


