data(PlantGrowth)

attach(PlantGrowth)
head(PlantGrowth)
tail(PlantGrowth)
names(PlantGrowth)
hist(group~weight)
str(PlantGrowth)
summary(PlantGrowth)

boxplot(weight ~ group)
as.factor()

hist(PlantGrowth[PlantGrowth$group=="1",]$weight)
histogram()
model <- lm(weight ~ group)
summary(model)

anova(model)
confint(model )

install.packages("factoextra")
library(factoextra)

data(decathlon2)
attach(decathlon2)
summary(decathlon2)
names(decathlon2)

names(decathlon2)
decathlon2.active <- decathlon2[1:23, 1:10]
names(decathlon2.active)
head(decathlon2.active[, 1:6])
decathlon2.active

cor(decathlon2.active)
evd <- eigen(cor(decathlon2.active))
evd$values

pcd <- prcomp(decathlon2.active , scale. = FALSE  )
pcd

names(decathlon2.active)
names(pcd)
summary(pcd)
pcd$sdev


8000+6750

qf(0.95,4 , 60)

6750/3
8000/16

fstat <- 2250 /  500

fstat

fcrit <- qf(0.95,3,16)

fcrit

qf(0.99,8,29)

qf(0.95,3,36)
