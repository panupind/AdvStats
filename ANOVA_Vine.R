VinePrice
summary(VinePrice)
attach(VinePrice)

VinePrice$Year <- factor(VinePrice$Year, labels = c("1996","1997","1998"))


hist(VinePrice[VinePrice$Year==1996,]$Price)
hist(VinePrice[VinePrice$Year==1997,]$Price)
hist(VinePrice[VinePrice$Year==1998,]$Price)

shapiro.test(VinePrice[VinePrice$Year==1996,]$Price)$p.value
shapiro.test(VinePrice[VinePrice$Year==1997,]$Price)$p.value
shapiro.test(VinePrice[VinePrice$Year==1998,]$Price)$p.value


leveneTest(VinePrice$Price~VinePrice$Year)

aovVine <- aov(VinePrice$Price ~ VinePrice$Year)
summary(aovVine)
