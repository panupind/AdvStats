##Upload the csv file into R
data2 <- read.csv(file.choose())

##Study the structure of data
str(data2)
head(data2)
tail(data2)

##Clean data
data2$X <- NULL
data2 <- data2[-c(31, 32, 33), ]

##Create a matrix out of the dataframe in order to be used in the Model
X2 <- as.matrix(data2[, 1:6])

# Determine Number of Factors to Extract using quantile distribution methods
install.packages("nFactors")

library(nFactors)
ev <- eigen(cor(X2)) # get eigenvalues

ev

nS <- nScree(x=ev$values)
plotnScree(nS)

##Run factor analysis model with number of factors limit to 2 and no rotation
Factor <- factanal(~X2, 
                   2, # Specify the number of factors to be extracted
                   rotation="none") # varimax and promax rotations are possible
Factor
print(Factor, digits=2, cutoff= .6, sort= TRUE)

#Calculate Factor Scores
library(psych)
factor.scores(data2[, 1:6],Factor$loadings)

##Uniqueness is the variance not explained by the 3 factors for each variable

##View the Factor Loadings
load <- Factor$loadings[,1:2]
load

##Plot the loadings of Factor 1 & Factor 2
plot(load, type= "n")
text(load, labels=names(data2), load=Factor$loadings[,1:2])


##Run factanal again with varimax rotation just for fun
Factor1 <- factanal(~X2, 2, rotation="varimax")

##View loadings
load1 <- Factor1$loadings[,1:2]
load1

##Plot the loadings for Factor 1 and Factor 2
plot(load1, type= "n")
text(load1, labels=names(data2), load1=Factor$loadings[,1:2])



##Run factanal again with promax rotation just for fun
Factor2 <- factanal(~X2, 2, rotation="promax")

##View Loadings
load2 <- Factor2$loadings[,1:2]
load2

##Plot the loadings as scatterplot
plot(load2, type= "n")
text(load2, labels=names(data2), load2=Factor$loadings[,1:2])


