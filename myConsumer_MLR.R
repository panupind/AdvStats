consumer_MLR
names(consumer_MLR)
attach(consumer_MLR)
mlr <- lm (AmountCharged~Income+HouseholdSize)
summary(mlr)
anova(mlr)
confint(mlr,"Income")
confint(mlr,"HouseholdSize")
predict_data<-data.frame(Income=40 , HouseholdSize =3)
predict(mlr , predict_data , interval = "confidence"  )
predict_mdl <- predict(mlr)
actual_mdl <-consumer_MLR$AmountCharged
BackTrack <- data.frame(predict_mdl , actual_mdl)
BackTrack
plot(actual_mdl, col="Red")
lines(predict_mdl, col="Red")
plot(predict_mdl, col="Blue")
lines(predict_mdl, col="Red")
lines(actual_mdl, col="Blue")

