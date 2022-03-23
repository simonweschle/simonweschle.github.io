### PSC 400, Spring 2022
### Week 8, Wednesday 3/23


## HYPOTHESIS TESTING, QUALITY OF GOVERNMENT
rm(list=ls(all=TRUE))

data <- read.csv("~/Dropbox/Teaching/2022_1_PSC_400/classes/week_4/data/qog_bas_cs_jan22.csv")

data$litdiff <- data$wdi_litradm - data$wdi_litradf
hist(data$litdiff)

model1 <- lm(litdiff ~ p_polity2 + wdi_expedu + wbgi_gee, data=data)
summary(model1)


# try it for p_polity2
summary(data$p_polity2)

polity.vec <- seq(from=min(data$p_polity2, na.rm=T), to=max(data$p_polity2, na.rm=T), length.out=50)

predict.polity.ci <- predict(model1, newdata=data.frame(p_polity2=polity.vec, wdi_expedu=mean(data$wdi_expedu, na.rm=T), wbgi_gee=mean(data$wbgi_gee, na.rm=T)), interval="confidence", level = 0.95)

plot(polity.vec, predict.polity.ci[,1], type="l", ylim=c(0, 11), xlab="Polity Score", ylab="Predicted Literacy Gap Men - Women", lwd=3)
points(polity.vec, predict.polity.ci[,2], type="l", lty=2, lwd=3)
points(polity.vec, predict.polity.ci[,3], type="l", lty=2, lwd=3)




rm(list=ls(all=TRUE))

cces <- read.csv("~/Dropbox/Teaching/2022_1_PSC_400/classes/week_8/data/cces19.csv")

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


lwr <- predict.age.logit$fit - 1.96*predict.age.logit$se.fit
upr <- predict.age.logit$fit + 1.96*predict.age.logit$se.fit

points(agevec, lwr, type="l", lty=2, lwd=3)
points(agevec, upr, type="l", lty=2, lwd=3)



# DV: support for Trump impeachment
model.logit <- glm(impeach ~ age + female + partisanship + nohighschool + collegeorhigher + nonwhite + married + employed, data=cces, family = binomial)
summary(model.logit)

predict.age.logit <- predict(model.logit, newdata=data.frame(age=agevec, female=0, partisanship="Independent", nohighschool=0, collegeorhigher=1, nonwhite=0, married=1, employed=0), type = "response", se.fit=TRUE)


plot(agevec, predict.age.logit$fit, type="l", ylim=c(0, 1), xlab="Age", ylab="Predicted Probability of Supporting Impeachment", lwd=2)


lwr <- predict.age.logit$fit - 1.96*predict.age.logit$se.fit
upr <- predict.age.logit$fit + 1.96*predict.age.logit$se.fit

points(agevec, lwr, type="l", lty=2, lwd=3)
points(agevec, upr, type="l", lty=2, lwd=3)




# social pressure experiment
rm(list=ls(all=TRUE))

socialdata <- read.csv("~/Dropbox/Teaching/2022_1_PSC_400/classes/week_8/data/social.csv")


# regular model
m <- lm(primary2006 ~ neighbors + age, data=socialdata)
summary(m)

predict(m, newdata=data.frame(neighbors=1, age=20))
predict(m, newdata=data.frame(neighbors=0, age=20))
predict(m, newdata=data.frame(neighbors=1, age=20)) - predict(m, newdata=data.frame(neighbors=0, age=20))

predict(m, newdata=data.frame(neighbors=1, age=50)) - predict(m, newdata=data.frame(neighbors=0, age=50))

predict(m, newdata=data.frame(neighbors=1, age=80)) - predict(m, newdata=data.frame(neighbors=0, age=80))




# interaction model
m.ia <- lm(primary2006 ~ neighbors + age + neighbors:age, data=socialdata)
summary(m.ia)

predict(m.ia, newdata=data.frame(neighbors=1, age=20))
predict(m.ia, newdata=data.frame(neighbors=0, age=20))
predict(m.ia, newdata=data.frame(neighbors=1, age=20)) - predict(m.ia, newdata=data.frame(neighbors=0, age=20))

predict(m.ia, newdata=data.frame(neighbors=1, age=50)) - predict(m.ia, newdata=data.frame(neighbors=0, age=50))

predict(m.ia, newdata=data.frame(neighbors=1, age=80)) - predict(m.ia, newdata=data.frame(neighbors=0, age=80))