library(car)

str(df_dental)

boxplot(df_dental$hardness ~ df_dental$method)

hist(df_dental[df_dental$method==1,]$hardness)
hist(df_dental[df_dental$method==2,]$hardness)
hist(df_dental[df_dental$method==3,]$hardness)

hist(df_dental[df_dental$method==1,]$hardness)

shapiro.test(df_dental[df_dental$method==1,]$hardness)$p.value
shapiro.test(df_dental[df_dental$method==2,]$hardness)$p.value
shapiro.test(df_dental[df_dental$method==3,]$hardness)$p.value

# conclusion is 2 of the 3 categories are found to have p-value > 0.05. So Null hypothesis is not rejected.
# Hence  2/3 samples are concluded to have normal distribution  based on p-values

leveneTest(df_dental$hardness~df_dental$method)

# p-value  for Levenes test is very low = 0.004138 
# which says reject Null Hypothesis , i.e variences are not equal
# Hence run Kruskal walis test than Anova test

krusTestMethod <- kruskal.test(df_dental$hardness~df_dental$method)
krusTestMethod

summary(krusTestMethod)



