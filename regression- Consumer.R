## Read Consumer data

Consumer <- read.csv(file.choose())

##View Structure of Data
str(Consumer)
tail(Consumer)



##Clean the data by removing the redundant 4 columns
Consumer$X <- NULL
Consumer$X.1 <- NULL
Consumer$X.2 <- NULL
Consumer$X.3 <- NULL

##Load the psych and lmtest Libraries
library(psych)
library(lmtest)

## Summary Statistics
describe(Consumer)
cor(Consumer)

library(corrplot)
corrplot(cor(Consumer))

with(Consumer, boxplot(Income, main="Income (1000) US$"))
with(Consumer, boxplot(HouseholdSize, main="Household Size"))
with(Consumer, boxplot(AmountCharged, main="Amount Charged US$"))

with(Consumer, plot(HouseholdSize, AmountCharged, pch=19, cex=0.8))
with(Consumer, plot(Income, AmountCharged, pch=19, cex=0.8))


## Simple Regressions with one Independent Variable
reg1 <- lm(AmountCharged ~ Income, data=Consumer)
reg1
summary(reg1)
anova(reg1)
anvRes <- anova(reg1)

#Calculating Residual Standard Error
#Square root of the Residual Sum of Square's Mean from ANOVA table
sqrt(25699404/48)

anvRes$Df[2]
anvRes$`Sum Sq`[2]

resError <- anvRes$`Sum Sq`[2] / anvRes$Df[2]
sqrt(resError)


reg2 <- lm(AmountCharged ~ HouseholdSize, data=Consumer)
reg2
summary(reg2)
anova(reg2)

## Multiple Regression with 2 Inependent Variables
reg3 <- lm(AmountCharged ~ Income + HouseholdSize, data=Consumer)
reg3
summary(reg3)
anova(reg3)

##Extract the fitted values and residual values from the reg3 output
fitted(reg3)
residuals(reg3)
fit3 <- fitted(reg3)
res3 <- residuals(reg3)


##Merge the fitted and residual values with Consumer dataset for comparison sake
ConsumerReg <- cbind(Consumer, fit3, res3)

##Plot the actual versus fitted values in a plot
with(ConsumerReg, plot(fit3,res3, pch=19, cex=0.6))
with(ConsumerReg, lines(fit3,res3, pch=19, cex=0.6))
with(ConsumerReg, plot(fit3, pch=19, cex=0.6 , col = "Red"))
with(ConsumerReg, plot(res3, pch=19, cex=0.6 , col = "Blue") , add.scatter)

abline(a=0,b=0)

# Here are the characteristics of a well-behaved 
# residual vs. fits plot and what they suggest about 
# the appropriateness of the simple linear regression model:



### The residuals "bounce randomly" around the 0 line. 
### This suggests that the assumption that the relationship is linear is reasonable.



### The residuals roughly form a "horizontal band" around the 0 line. 
##@ This suggests that the variances of the error terms are equal.



### No one residual "stands out" from the basic random pattern of residuals. 
### This suggests that there are no outliers.


## Prediction of new observations
newobs <- data.frame(Income = 40, HouseholdSize = 3)
newobs
predict.lm(reg3, newdata=newobs)

 newobs <- data.frame(Income = c(40,50), HouseholdSize = c(3, 4))
 predict.lm(reg3, newdata=newobs)


##Assumptions

##Linear relationship
with(Consumer, plot(HouseholdSize, AmountCharged, pch=19, cex=0.6))
with(Consumer, plot(Income, AmountCharged, pch=19, cex=0.6))



##Multivariate normality Test
with(Consumer, shapiro.test(Income))
with(Consumer, shapiro.test(HouseholdSize))

with(Consumer, qqnorm(Income, pch=19, cex=0.6))
with(Consumer, qqline(Income, col='red'))


##No or little multicollinearity - Correlation or VIF test
library(corrplot)
corrplot(cor(Consumer))

# Values greater than 10 might suggest correlation effect.
#install.packages("DAAG")
library(DAAG)
vif(reg3)


##No auto-correlation - Durbin Watson Test
#Null Hypothesis - there is no autocorrelation
dwtest(reg3)


## Homoscedasticity
#Null hypothesis : Data is homoscedastic
?gqtest(reg3)




## k-fold cross validation technique to generalize liear regression model
## install.packages("DAAG")

library(DAAG)

kfold <- cv.lm(data = Consumer, 
              form.lm = formula(AmountCharged ~ Income + HouseholdSize), 
              m=3, #No.of folds
              dots = FALSE, #pch styling
              seed=29, #Random Seed generator
              plotit=TRUE, #Plot the predicted values of cv
              printit=TRUE) #print the results of cv
