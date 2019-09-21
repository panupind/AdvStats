setwd("G:/Advanced Statistics/R-CSV")
data1=read.csv("PCA-nm.csv", header=TRUE)
data1
PreventCav=data1$V1
ShinyTeeth=data1$V2
StrengthGum=data1$V3
Fresh=data1$V4
Decay=8-data1$V5
Attractive=data1$V6
mydata=data.frame(PreventCav,ShinyTeeth,StrengthGum,Fresh,Decay,Attractive)
mydata
attach(mydata)
library(nFactors)
ev = eigen(cor(mydata)) # get eigenvalues
ev
EigenValue=ev$values
EigenValue
Factor=c(1,2,3,4,5,6)
Scree=data.frame(Factor,EigenValue)
plot(Scree,main="Scree Plot", col="Blue")
lines(Scree,col="Red")
library(psych)
Unrotate=principal(mydata, nfactors=6, rotate="none")
print(Unrotate,digits=3)
UnrotatedProfile=plot(Unrotate,row.names(Unrotate$loadings))
Rotate=principal(mydata,nfactors=2,rotate="varimax")
print(Rotate,digits=3)
RotatedProfile=plot(Rotate,row.names(Rotate$loadings),cex=1.0)
Rotate$scores
factor.scores(mydata, f=Rotate$loadings,  method = "Harman" )
                                          
