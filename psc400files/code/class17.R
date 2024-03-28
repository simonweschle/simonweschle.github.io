### PSC 400, Spring 2024
### Week 9, Thursday 3/28



# INTERACTION EFFECT

rm(list=ls(all=TRUE))

immidata <- read.csv("~/Dropbox/Teaching/2024_1_PSC_400/classes/week_7/data/immig.csv")

# h1bvis.supp: 0=decrease a great deal, 1=increase a great deal (larger values=more suport for immigration)
# impl.prejud: 0=low implicit prejudice, 1=high implicit prejudice


model1 <- lm(h1bvis.supp ~ impl.prejud + female, data=immidata)
summary(model1)


# plotting the predicted value for different values of prejudice (no confidence intervals for simplicity)

prejud.vec <- c(0, 0.25, 0.5, 0.75, 1)

predict.prejud.female <- predict(model1, newdata=data.frame(impl.prejud=prejud.vec, female=1))
predict.prejud.male <- predict(model1, newdata=data.frame(impl.prejud=prejud.vec, female=0))

plot(prejud.vec, predict.prejud.female, type="l", ylim=c(0,1), xlab="Implicit Prejudige", ylab="Predicted Immigration Policy Support", lwd=3)
points(prejud.vec, predict.prejud.male, type="l", lty="dashed", lwd=3)
text(0.5, 0.45, "Male")
text(0.5, 0.28, "Female")




model2 <- lm(h1bvis.supp ~ impl.prejud + female + impl.prejud:female, data=immidata)
summary(model2)

predict.prejud.female <- predict(model2, newdata=data.frame(impl.prejud=prejud.vec, female=1))
predict.prejud.male <- predict(model2, newdata=data.frame(impl.prejud=prejud.vec, female=0))

plot(prejud.vec, predict.prejud.female, type="l", ylim=c(0,1), xlab="Implicit Prejudige", ylab="Predicted Immigration Policy Support", lwd=3)
points(prejud.vec, predict.prejud.male, type="l", lty="dashed", lwd=3)
text(0.5, 0.45, "Male")
text(0.5, 0.28, "Female")





# interaction with confidence intervals

predict.prejud.female <- predict(model2, newdata=data.frame(impl.prejud=prejud.vec, female=1, age=mean(immidata$age, na.rm=T)), interval="confidence", level = 0.95)
predict.prejud.male <- predict(model2, newdata=data.frame(impl.prejud=prejud.vec, female=0, age=mean(immidata$age, na.rm=T)), interval="confidence", level = 0.95)


plot(prejud.vec, predict.prejud.female[,1], type="l", ylim=c(0,1), xlab="Implicit Prejudige", ylab="Predicted Immigration Policy Support", lwd=3, col="red")
points(prejud.vec, predict.prejud.female[,2], type="l", lty="dashed", lwd=2, col="red")
points(prejud.vec, predict.prejud.female[,3], type="l", lty="dashed", lwd=2, col="red")

points(prejud.vec, predict.prejud.male[,1], type="l", lwd=3, col="blue")
points(prejud.vec, predict.prejud.male[,2], type="l", lty="dashed", lwd=2, col="blue")
points(prejud.vec, predict.prejud.male[,3], type="l", lty="dashed", lwd=2, col="blue")








## NON-LINEAR EFFECTS

# we focus on age now
m.age <- lm(h1bvis.supp ~ impl.prejud + female + age, data=immidata)
summary(m.age)

summary(immidata$age)

age.vec <- seq(18, 90, 1)

predict.age <- predict(m.age, newdata=data.frame(impl.prejud=mean(immidata$impl.prejud, na.rm=T), female=1, age=age.vec))
plot(age.vec, predict.age, type="l", ylim=c(0,1), xlab="Age", ylab="Predicted Immigration Policy Support", lwd=3)





# non-linear effect
m.sq <- lm(h1bvis.supp ~ impl.prejud + female + age + I(age^2), data=immidata)
summary(m.sq)

predict.age <- predict(m.sq, newdata=data.frame(impl.prejud=mean(immidata$impl.prejud, na.rm=T), female=1, age=age.vec))
plot(age.vec, predict.age, type="l", ylim=c(0,1), xlab="Age", ylab="Predicted Immigration Policy Support", lwd=3)


# confidence intervals
predict.age.ci <- predict(m.sq, newdata=data.frame(impl.prejud=mean(immidata$impl.prejud, na.rm=T), female=1, age=age.vec), interval="confidence", level = 0.95)

plot(age.vec, predict.age.ci[,1], type="l", ylim=c(0,1), xlab="Age", ylab="Predicted Immigration Policy Support", lwd=3)
points(age.vec, predict.age.ci[,2], type="l", lty="dashed", lwd=2)
points(age.vec, predict.age.ci[,3], type="l", lty="dashed", lwd=2)




### CATEGORICAL VARIABLES

rm(list=ls(all=TRUE))

cces <- read.csv("~/Dropbox/Teaching/2024_1_PSC_400/classes/week_9/data/cces19.csv")

prop.table(table(cces$votereg))


# gender
m.gender <- lm(votereg ~ female, data=cces)
summary(m.gender)

predict(m.gender, newdata=data.frame(female=1))
predict(m.gender, newdata=data.frame(female=0))


# partisanship
prop.table(table(cces$partisanship))

m.partisan <- lm(votereg ~ partisanship, data=cces)
summary(m.partisan)

predict(m.partisan, newdata=data.frame(partisanship="Republican"))
predict(m.partisan, newdata=data.frame(partisanship="Independent"))
predict(m.partisan, newdata=data.frame(partisanship="Democrat"))





# using a linear model for a binary dependent variable -- can create big problems!
model1 <- lm(votereg ~ age + female + partisanship + nohighschool + collegeorhigher + nonwhite + married + employed, data=cces)
summary(model1)

summary(cces$age)

agevec <- seq(from=18, to=100, by=1)


predict.age <- predict(model1, newdata=data.frame(age=agevec, female=0, partisanship="Democrat", nohighschool=0, collegeorhigher=1, nonwhite=0, married=1, employed=0), interval="confidence", level = 0.95)

plot(agevec, predict.age[,1], type="l", ylim=c(0.6, 1), xlab="Age", ylab="Predicted Probability of Being Registered to Vote", lwd=3)
points(agevec, predict.age[,2], type="l", lty=2, lwd=3)
points(agevec, predict.age[,3], type="l", lty=2, lwd=3)


plot(agevec, predict.age[,1], type="l", ylim=c(0.6, 1.2), xlab="Age", ylab="Predicted Probability of Being Registered to Vote", lwd=3)
points(agevec, predict.age[,2], type="l", lty=2, lwd=3)
points(agevec, predict.age[,3], type="l", lty=2, lwd=3)




# logit models for binary dependent variables
model.logit <- glm(votereg ~ age + female + partisanship + nohighschool + collegeorhigher + nonwhite + married + employed, data=cces, family = binomial)
summary(model.logit)

predict.age.logit <- predict(model.logit, newdata=data.frame(age=agevec, female=0, partisanship="Democrat", nohighschool=0, collegeorhigher=1, nonwhite=0, married=1, employed=0), type = "response", se.fit=TRUE)


plot(agevec, predict.age.logit$fit, type="l", ylim=c(0.75, 1), xlab="Age", ylab="Predicted Probability of Being Registered to Vote", lwd=2)


