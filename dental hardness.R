Sys.unsetenv("http_proxy")
Sys.unsetenv("https_proxy")

Sys.setenv( http_proxy = "www-proxy.usoracle.com:80")
Sys.setenv( https_proxy = "www-proxy.usoracle.com:80")

# Read data from clipboard - Another neat way to import data
# Copy the data from excel before executing below code

my_data <- read.table(file = "clipboard", 
                      sep = "\t", header=TRUE)

# Clearly identify the factors in the data
my_data$dentist<-factor(my_data$dentist)
my_data$method<-factor(my_data$method)
my_data$alloy<-factor(my_data$alloy)
my_data$temperature<-factor(my_data$temperature)

#Summary statistics
str(my_data)
summary(my_data)


#Visual summary
hist(my_data[my_data$temperature==1500,]$hardness)
hist(my_data[my_data$temperature==1600,]$hardness)
hist(my_data[my_data$temperature==1700,]$hardness)

boxplot(my_data$hardness~my_data$temperature)

# Statistical Summary
shapiro.test(my_data[my_data$temperature==1500,]$hardness)$p.value
shapiro.test(my_data[my_data$temperature==1600,]$hardness)$p.value
shapiro.test(my_data[my_data$temperature==1700,]$hardness)$p.value

install.packages("car")
library(car)
leveneTest(my_data$hardness~my_data$temperature)


# Run the ANOVA model

aov1 <- aov(my_data$hardness~my_data$temperature)

summary(aov1)

power.anova.test(groups=3,n=30,between.var = 41089, within.var = 20792, sig.level = 0.05)


# Non Parametric Test

kruskal.test(my_data$hardness~my_data$temperature)



############################################################

t.test(my_data$hardness, mu=720, alternative="t", conf.level=0.95)

wilcox.test(my_data$hardness, mu=720)

# n = 90
# delta = xbar-mu = 720 - 741.7778 = -21.7778
#sd = 145.7678

power.t.test(n=90,delta=-21.778,sd=145.7678, alternative="two.sided",sig.level=0.05)


# For increasing power of test to 80%
power.t.test(delta=-21.778,sd=145.7678,power=.8, alternative="two.sided",sig.level=0.05)


############################################################

#Two Sample t test

t.test(my_data[my_data$alloy==1,]$hardness,my_data[my_data$alloy==2,]$hardness,paired = FALSE)

wilcox.test(my_data[my_data$alloy==1,]$hardness,my_data[my_data$alloy==2,]$hardness,paired=FALSE)


# Calculate power of test

# n = 45
# delta = xbar1-vbar2 = -68.58
#sd = pooledSD between group1 and group2
pooledSD <- (((45-1)*(14688.12)+(45-1)*(25886.43))/(45+45-2))^0.5
pooledSD

power.t.test(n=45,delta=-68.58,sd=142.4334, alternative="two.sided",sig.level=0.05)

# How many observations per group are necessary for increasing power of test to 80%?
