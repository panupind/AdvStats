setwd("G:/Advanced Statistics/R-CSV")
mydata=read.csv("PCA-Brown.csv", header=TRUE)
mydata
attach(mydata)
mydata=mydata[,2:8]
library(nFactors)
ev = eigen(cor(mydata)) # get eigenvalues
ev
EigenValue=ev$values
EigenValue
Factor=c(1,2,3,4,5,6,7)
Scree=data.frame(Factor,EigenValue)
plot(Scree,main="Scree Plot", col="Blue",ylim=c(0,4))
lines(Scree,col="Red")
library(psych)
Unrotate=principal(mydata, nfactors=3, rotate="none")
print(Unrotate,digits=3)
UnrotatedProfile=plot(Unrotate,row.names(Unrotate$loadings))
Rotate=principal(mydata,nfactors=3,rotate="varimax")
print(Rotate,digits=3)
RotatedProfile=plot(Rotate,row.names(Rotate$loadings),cex=1.0)

