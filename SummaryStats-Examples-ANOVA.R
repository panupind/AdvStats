gasMgCars.mean <- c(25.8, 22.68 , 21.29)
gasMgCars.sd <- c(2.56, 3.67, 2.76) 
gasMgCars.n <- c(31,31,14)

gasMgCars.frame <- data.frame(gasMgCars.mean, gasMgCars.sd , gasMgCars.n)

install.packages("rpsychi")
library(rpsychi)

ind.oneway(gasMgCars.mean, gasMgCars.sd , gasMgCars.n, sig.level = 0.01)


grandmean <- (31*25.8+ 31*22.68 + 21.29*14) / (31+31+14)
grandmean

#grandmean <- (25.8+22.68+29.14)/3

SSReg <- ( 31* (25.8- grandmean)^2 + 31*(22.68 - grandmean)^2 + 14*(29.14 - grandmean)^2 )
SSReg


dfReg <- 3-1
dfReg 

MSReg <- SSReg / dfReg
MSReg

MSError <-  (30*(2.56^2) + 30 *(3.67^2) +  13 *(2.76^2) )/ ( (31+31+14-1)-(3-1))
MSError

dfError <- (31+31+14)-(3)
dfError



SSError <- MSError * dfError
SSError

SSTotal <- SSReg + SSError
SSTotal

#FSTATCars <- SSError / SSTotal
#FSTATCars


FSTATCars1 <- MSReg / MSError
FSTATCars1


qf(0.95,4,60)

qf(0.99,8,29)


qf(0.95,3,16)

qf(0.95,3,8)


