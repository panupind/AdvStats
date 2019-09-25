install.packages("car")
library(car)
Prestige
remove(myPresige)
names(Prestige)
myPrestige = data.frame(Prestige[1:2])
myPrestige
attach(myPrestige)
IQR(income)
summary(myPrestige$income)
summary(myPrestige$education)

mymodel = lm(income~education)
summary(mymodel)

Prestige = data.frame(Prestige)

education.c <- scale(Prestige$education, center = TRUE , scale = FALSE)
mymodel.c = lm(income~education.c)
summary(mymodel.c)

summary(Prestige$type)


names(Prestige)
myPrestige1 <- data.frame(Prestige[1:4])
names(myPrestige1)

cor(myPrestige1$women, myPrestige1$income)

corrplot(cor(myPrestige1))

cor(myPrestige1)


mymodel1 = lm(myPrestige1$income~myPrestige1$education+myPrestige1$women + myPrestige1$prestige , data= myPrestige1)
summary(mymodel1)


anova(mymodel1)
str(anova(mymodel1))


tail(myPrestige1)
#Model 1 
#Residual standard error: 3483 on 100 degrees of freedom
#Multiple R-squared:  0.3336,	Adjusted R-squared:  0.3269 
#F-statistic: 50.06 on 1 and 100 DF,  p-value: 2.079e-10

#Model2
#Residual standard error: 2575 on 98 degrees of freedom
#Multiple R-squared:  0.6432,	Adjusted R-squared:  0.6323 

#Adjusted R-squared difference

0.6323-0.3269


myPrestige1


income.c = scale(myPrestige1$income , scale = FALSE , center = TRUE)
prestige.c = scale(myPrestige1$prestige , scale = FALSE , center = TRUE)
women.c = scale(myPrestige1$women , scale = FALSE , center = TRUE)

education.c = scale(myPrestige1$education , scale = FALSE , center = TRUE)

income.c= myPrestige1$income
women.c = myPrestige1$women
prestige.c = myPrestige1$prestige

set.seed(1)
myPrestige2 <- data.frame(education.c, income.c , women.c , prestige.c)
myPrestige2

cor(myPrestige2)
corrplot(cor(myPrestige2))


mymodel2 = lm(myPrestige2$income.c~ (myPrestige2$education.c + myPrestige2$women.c + myPrestige2$prestige.c) , data= myPrestige2)
summary(mymodel2)
