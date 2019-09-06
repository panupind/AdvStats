GolfDS

attach(GolfDS)

anovglf <- aov(Distance~Design)
anovglf
summary(anovglf)
print(summary(anovglf),digits = 6)
TukeyHSD(anovglf , conf.level = 0.99)
plot(TukeyHSD(anovglf))
