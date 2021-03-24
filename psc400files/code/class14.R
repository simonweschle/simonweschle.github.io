
### Class Code from Mar 24

rm(list=ls(all=TRUE))

qogdata <- load.csv("~/Dropbox/Teaching/2021_PSC_400/classes/week_7/data/qogdata_reduced.csv")

hist(qogdata$litdiff, main="", xlab="Literacy Rate Male - Female")

m1 <- lm(litdiff ~ p_polity2 + wbgi_gee, data=qogdata)
summary(m1)






## immigration attitudes

rm(list=ls(all=TRUE))

immidata <- read.csv("~/Dropbox/Teaching/2021_PSC_400/classes/week_7/data/immig.csv")

# h1bvis.supp: 0=decrease a great deal, 1=increase a great deal (larger values=more suport for immigration)
# impl.prejud: 0=low implicit prejudice, 1=high implicit prejudice

m.both <- lm(h1bvis.supp ~ impl.prejud + female, data=immidata)
summary(m.both)


# predicted values
predict(m.both, newdata=data.frame(impl.prejud=0.5, female=1))
predict(m.both, newdata=data.frame(impl.prejud=1, female=1))
predict(m.both, newdata=data.frame(impl.prejud=1, female=0))



# plotting the predicted value for different values of prejudice
prejud.vec <- seq(0, 1, 0.1)
predict.prejud.female <- predict(m.both, newdata=data.frame(impl.prejud=prejud.vec, female=1))

plot(prejud.vec, predict.prejud.female, type="l", ylim=c(0,1), xlab="Implicit Prejudige", ylab="Predicted Immigration Policy Support", lwd=3)

predict.prejud.male <- predict(m.both, newdata=data.frame(impl.prejud=prejud.vec, female=0))

points(prejud.vec, predict.prejud.male, type="l", lty="dashed", lwd=3)
text(0.5, 0.45, "Male")
text(0.5, 0.28, "Female")



# heterogeneous treatment effect
m.ia <- lm(h1bvis.supp ~ impl.prejud + female + impl.prejud:female, data=immidata)
summary(m.ia)


predict.prejud.female <- predict(m.ia, newdata=data.frame(impl.prejud=prejud.vec, female=1))
predict.prejud.male <- predict(m.ia, newdata=data.frame(impl.prejud=prejud.vec, female=0))



plot(prejud.vec, predict.prejud.female, type="l", ylim=c(0,1), xlab="Implicit Prejudige", ylab="Predicted Immigration Policy Support", lwd=3)
points(prejud.vec, predict.prejud.male, type="l", lty="dashed", lwd=3)
text(0.5, 0.275, "Female")
text(0.5, 0.46, "Male")



