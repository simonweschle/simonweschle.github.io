
### Class Code from Mar 31

# social pressure experiment
rm(list=ls(all=TRUE))

socialdata <- read.csv("~/Dropbox/Teaching/2021_PSC_400/classes/week_8/data/social.csv")


# interaction model
m.ia <- lm(primary2006 ~ neighbors + age + age:neighbors, data=socialdata)
summary(m.ia)


summary(socialdata$age)
agevec <- seq(20, 106, 1)

# effect of age on turnout for those who got treatment and those who did not
predict.control.age <- predict(m.ia, newdata=data.frame(neighbors=0, age=agevec), interval="confidence", level = 0.95)
predict.treat.age <- predict(m.ia, newdata=data.frame(neighbors=1, age=agevec), interval="confidence", level = 0.95)


plot(agevec, predict.treat.age[,1], type="l", ylim=c(0, 0.7), xlab="Age", ylab="Predicted Turnout", lwd=2)
points(agevec, predict.treat.age[,2], type="l", lty="dashed", lwd=1.5)
points(agevec, predict.treat.age[,3], type="l", lty="dashed", lwd=1.5)
text(25, 0.32, "Treatment")

points(agevec, predict.control.age[,1], type="l", lwd=2, col="gray")
points(agevec, predict.control.age[,2], type="l", lty="dashed", lwd=1.5, col="gray")
points(agevec, predict.control.age[,3], type="l", lty="dashed", lwd=1.5, col="gray")
text(25, 0.16, "Control", col="gray")



rm(list=ls(all=TRUE))

cces <- read.csv("~/Dropbox/Teaching/2021_PSC_400/classes/week_8/data/cces19.csv")

prop.table(table(cces$votereg))


# using a linear model for a binary dependent variable -- can create big problems!
m.lin <- lm(votereg ~ age + republican + democrat + female + nohighschool + collegeorhigher + nonwhite + married + employed, data=cces)
summary(m.lin)

summary(cces$age)

agevec <- seq(from=18, to=100, by=1)


predict.age <- predict(m.lin, newdata=data.frame(age=agevec, republican=0, democrat=0, female=0, nohighschool=0, collegeorhigher=1, nonwhite=0, married=1, employed=0))

plot(agevec, predict.age, type="l", xlab="Age", ylab="Predicted Probability of Being Registered to Vote", lwd=2)



