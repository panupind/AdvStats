library(psych)
library(car)

Sys.unsetenv("http_proxy")
Sys.unsetenv("https_proxy")

install.packages("psych")

## Summary Statistics


####Coupon and Instore Promotion Data
df<-read.csv(file.choose())

#Step 2: Clearly identify the factors in the data
df$promotion<-factor(df$promotion, labels=c("1","2","3"))
df$coupon<-factor(df$coupon, labels=c("1","2"))

###Calculate the means for promotion and coupon

tapply(df$sales,list(df$promotion, df$coupon), mean)

##Note : High sales and high coupon gives relatively higher sales values
## As the intensity of promotion and coupons are reduced, sales reduces accordingly


###Create a plot to identify the interaction effects between promotion & coupon

interaction.plot(df$promotion,df$coupon,df$sales)

##Both lines are almost parallel hence we can say interaction effect is negligible

## Test for normality for all the 3 groups in promotion

cat("Normality p-values by Factor Place: ")
#for (i in unique(factor(df$promotion))){
#  cat(shapiro.test(df[df$promotion==i, ]$sales)$p.value," ")
#}



####P values are greater than 0.05, hence we do not reject the null hypothesis

####If for loop is confusing, use single test as follows for each promotion and coupon levels.
shapiro.test(df[df$promotion==1,]$sales)$p.value
shapiro.test(df[df$promotion==2,]$sales)$p.value
shapiro.test(df[df$promotion==3,]$sales)$p.value

###Run the normality assumption test for both groups in coupon

#cat("Normality p-values by Factor Place: ")
#for (i in unique(factor(df$coupon))){
#  cat(shapiro.test(df[df$coupon==i, ]$sales)$p.value," ")
#}

####P values are greater than 0.05, hence we do not reject the null hypothesis

shapiro.test(df[df$coupon==1, ]$sales)$p.value
shapiro.test(df[df$coupon==2, ]$sales)$p.value

###Create a qqplot to check the normality of entire dataset visually

qqnorm(df$sales, pch=19, cex=0.6)
qqline(df$sales, col = 'red')

###From the image, we can see the entire dataset is normally distributed


######## Levene's test for variance

## Test for homogeneity of variance

####Testing variance visually using a boxplot
boxplot(df$sales~df$promotion)

###From the boxplot, we see that 2,3 have almost same variance .


####Check for variances in groups using Levene's test

leveneTest(df$sales~df$promotion)
leveneTest(df$sales~df$coupon)


####As all the p values are greater than 0.05, we do not reject the null hypothesis

##################################################################################
####ANOVA based on promotion

aov1 <- aov(df$sales~df$promotion)

summary(aov1)

##p value is less than 0.05, hence we reject the null hypothesis

TukeyHSD(aov1)
plot(TukeyHSD(aov1))

###We reject the null hypothesis as all the intervals do not contain zero within them

###Business Intuiton
###High, medium & low are boosting sales in some manner
###Jump from low to high gives a sales boost of 4.6
###Jump from medium to high gives a sales boost of 2.1
###If the costs of doing a high promotion are less than the above boosted sales values
### We can conclude saying that all stores can run the high promotion for better sales

###However, if the costs of doing high promotion are more than the boosted sales figures
### We can actually drop the high promotion stores to medium or even low




##################################################################################
##################################################################################
####ANOVA based on coupon
aov1 <- aov(df$sales~df$coupon)

summary(aov1)



# With Interaction
aov1 <- aov(df$sales~df$coupon*df$promotion)

summary(aov1)
TukeyHSD(aov1)
plot(TukeyHSD(aov1))


# Without Interaction = similar to 2 One way ANOVA
aov1 <- aov(df$sales~df$coupon+df$promotion)

summary(aov1)

####p value is less than 0.05, hence we reject the null hypothesis

TukeyHSD(aov1)
plot(TukeyHSD(aov1))

###Since the intervals do not contain zero, it means there is a significant difference
###In Business terms, I can offer higher coupons in all stores if my net profit increases
###We would need to calculate the net increase in profit using the sales boost of 2.666667


