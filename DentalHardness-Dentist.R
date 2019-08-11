library(car)

str(df_dental)

boxplot(df_dental$hardness ~ df_dental$dentist)

hist(df_dental[df_dental$dentist==1,]$hardness)
hist(df_dental[df_dental$dentist==2,]$hardness)
hist(df_dental[df_dental$dentist==3,]$hardness)
hist(df_dental[df_dental$dentist==4,]$hardness)
hist(df_dental[df_dental$dentist==5,]$hardness)


shapiro.test(df_dental[df_dental$dentist==1,]$hardness)$p.value
shapiro.test(df_dental[df_dental$dentist==2,]$hardness)$p.value
shapiro.test(df_dental[df_dental$dentist==3,]$hardness)$p.value
shapiro.test(df_dental[df_dental$dentist==4,]$hardness)$p.value
shapiro.test(df_dental[df_dental$dentist==5,]$hardness)$p.value

# conclusion is 4 of the 5 categories are found to have normal distribution  based on p-values

leveneTest(df_dental$hardness~df_dental$dentist)

# p-value  for Levenes test is very low , which says reject Null Hypothesis , i.e variebces are not equal
# Hence run Kruskal walis test than Anova test

krusTestdentist <- kruskal.test(df_dental$hardness~df_dental$dentist)
krusTestdentist

summary(krusTest)
