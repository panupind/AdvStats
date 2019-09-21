##Load the dataset into R

data1 <- read.csv(file.choose())




##Look at structure of data
str(data1)
head(data1)
tail(data1)
summary(data1)


##Delete the 7th column that is redundant
data1$X <- NULL


##Delete the last 3 rows with NA values
data1 <- data1[-c(31, 32, 33), ]

##Create a matrix out of the dataframe in order to use the same in the Model
X <- as.matrix(data1[, 1:6])
X

##Check the correlation between the variables
cor(X)


##Check the correlation between variables visually
install.packages("corrplot")

library(corrplot)

corrplot(cor(X))
library(psych)

KMO(X)
##Run the PCA Model on the Matrix X

PCA <- princomp(~X, scores = TRUE, cor=TRUE)
?princomp

##View the Proportion of Variance explained by each Factor
summary(PCA)

##View the loadings for each variable across the Factors
loadings(PCA)
Factors <- loadings(PCA)
Factors
print(Factors, digits = 2, cutoff = 0.4, sort=TRUE)


##The data does not need a rotation as the earlier model has explained data

##Do it still, for fun and check output
Rotation <- varimax(loadings(PCA))
Rotation


##View the Factor Scores
PCA$scores
PCA$scores[,1:2]

##Scree plot
plot(PCA, type="line")

##Plot the Factor Scores for Factor 1 & Factor 2
plot(PCA$scores[,1], PCA$scores[,2], col=c("Red", "Green"))

##Biplot to understand the direction of each variable wrt the 2 Factors 1 & 2
biplot(PCA, cex=.7)
