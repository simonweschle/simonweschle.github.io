
### Class Code from Mar 29

rm(list=ls(all=TRUE))

immidata <- read.csv("~/Dropbox/Teaching/2021_PSC_400/classes/week_7/data/immig.csv")

# h1bvis.supp: 0=decrease a great deal, 1=increase a great deal (larger values=more suport for immigration)
# impl.prejud: 0=low implicit prejudice, 1=high implicit prejudice

# age
m.age <- lm(h1bvis.supp ~ impl.prejud + female + age, data=immidata)
summary(m.age)

summary(immidata$age)

age.vec <- seq(18, 90, 1)

predict.age <- predict(m.age, newdata=data.frame(impl.prejud=mean(immidata$impl.prejud, na.rm=T), female=1, age=age.vec))
plot(age.vec, predict.age, type="l", ylim=c(0,1), xlab="Age", ylab="Predicted Immigration Policy Support", lwd=3)




# non-linear effect (use Grapher)
m.sq <- lm(h1bvis.supp ~ impl.prejud + female + age + I(age^2), data=immidata)
summary(m.sq)

predict.age <- predict(m.sq, newdata=data.frame(impl.prejud=mean(immidata$impl.prejud, na.rm=T), female=1, age=age.vec))
plot(age.vec, predict.age, type="l", ylim=c(0,1), xlab="Age", ylab="Predicted Immigration Policy Support", lwd=3)


# confidence intervals
predict.age.ci <- predict(m.sq, newdata=data.frame(impl.prejud=mean(immidata$impl.prejud, na.rm=T), female=1, age=age.vec), interval="confidence", level = 0.95)

plot(age.vec, predict.age.ci[,1], type="l", ylim=c(0,1), xlab="Age", ylab="Predicted Immigration Policy Support", lwd=3)
points(age.vec, predict.age.ci[,2], type="l", lty="dashed", lwd=2)
points(age.vec, predict.age.ci[,3], type="l", lty="dashed", lwd=2)


# interaction with confidence intervals
m.ia <- lm(h1bvis.supp ~ impl.prejud + female + impl.prejud:female + age, data=immidata)
summary(m.ia)


prejud.vec <- seq(0, 1, 0.1)

predict.prejud.female <- predict(m.ia, newdata=data.frame(impl.prejud=prejud.vec, female=1, age=mean(immidata$age, na.rm=T)), interval="confidence", level = 0.95)
predict.prejud.male <- predict(m.ia, newdata=data.frame(impl.prejud=prejud.vec, female=0, age=mean(immidata$age, na.rm=T)), interval="confidence", level = 0.95)


plot(prejud.vec, predict.prejud.female[,1], type="l", ylim=c(0,1), xlab="Implicit Prejudige", ylab="Predicted Immigration Policy Support", lwd=3, col="red")
points(prejud.vec, predict.prejud.female[,2], type="l", lty="dashed", lwd=2, col="red")
points(prejud.vec, predict.prejud.female[,3], type="l", lty="dashed", lwd=2, col="red")

points(prejud.vec, predict.prejud.male[,1], type="l", lwd=3, col="blue")
points(prejud.vec, predict.prejud.male[,2], type="l", lty="dashed", lwd=2, col="blue")
points(prejud.vec, predict.prejud.male[,3], type="l", lty="dashed", lwd=2, col="blue")




# social pressure experiment
rm(list=ls(all=TRUE))

socialdata <- read.csv("~/Dropbox/Teaching/2021_PSC_400/classes/week_8/data/social.csv")


m.neighb <- lm(primary2006 ~ neighbors, data=socialdata)
summary(m.neighb)

