
### Class Code from Apr 5


rm(list=ls(all=TRUE))


cces <- read.csv("~/Dropbox/Teaching/2021_PSC_400/classes/week_9/data/cces19_week9.csv")


# categorical independent variables
m.gender <- lm(votereg ~ female, data=cces)
summary(m.gender)


table(cces$partisanship)

m.partisan <- lm(votereg ~ partisanship, data=cces)
summary(m.partisan)


m.partisan2 <- lm(votereg ~ independent + republican, data=cces)
summary(m.partisan2)


m.partisan3 <- lm(votereg ~ democrat + republican, data=cces)
summary(m.partisan3)




predict(m.partisan, newdata=data.frame(partisanship="Democrat"))
predict(m.partisan2, newdata=data.frame(independent=0, republican=0))
predict(m.partisan3, newdata=data.frame(democrat=1, republican=0))


predict(m.partisan, newdata=data.frame(partisanship="Republican"))
predict(m.partisan2, newdata=data.frame(independent=0, republican=1))
predict(m.partisan3, newdata=data.frame(democrat=0, republican=1))


predict(m.partisan, newdata=data.frame(partisanship="Independent"))
predict(m.partisan2, newdata=data.frame(independent=1, republican=0))
predict(m.partisan3, newdata=data.frame(democrat=0, republican=0))





# linear models can go wrong!
m.lin <- lm(votereg ~ age + partisanship + female + nohighschool + collegeorhigher + nonwhite + married + employed, data=cces)
summary(m.lin)

summary(cces$age)

agevec <- seq(from=18, to=100, by=1)


predict.age <- predict(m.lin, newdata=data.frame(age=agevec, partisanship="Independent", female=0, nohighschool=0, collegeorhigher=1, nonwhite=0, married=1, employed=0))

plot(agevec, predict.age, type="l", xlab="Age", ylab="Predicted Probability of Being Registered to Vote", lwd=2)



# logit models for binary dependent variables
m.logit <- glm(votereg ~ age + partisanship + female + nohighschool + collegeorhigher + nonwhite + married + employed, data=cces, family = binomial)
summary(m.logit)

predict.age.logit <- predict(m.logit, newdata=data.frame(age=agevec, partisanship="Independent", female=0, nohighschool=0, collegeorhigher=1, nonwhite=0, married=1, employed=0), type = "response")


plot(agevec, predict.age.logit, type="l", xlab="Age", ylab="Predicted Probability of Being Registered to Vote", lwd=2)



# support for impeachment 
table(cces$impeach)

m.impeach <- glm(impeach ~ partisanship + age + female + nohighschool + collegeorhigher + nonwhite + married + employed, data=cces, family = binomial)
summary(m.impeach)


predict.age <- predict(m.impeach, newdata=data.frame(partisanship="Independent", age=agevec, female=0, nohighschool=0, collegeorhigher=1, nonwhite=0, married=1, employed=0), type = "response")
plot(agevec, predict.age, type="l", ylim=c(0,1), xlab="Age", ylab="Predicted Probability of Supporting Impeachment", lwd=2)


predict.rep <- predict(m.impeach, newdata=data.frame(partisanship="Republican", age=50, female=0, nohighschool=0, collegeorhigher=1, nonwhite=0, married=1, employed=0), type = "response")
predict.dem <- predict(m.impeach, newdata=data.frame(partisanship="Democrat", age=50, female=0, nohighschool=0, collegeorhigher=1, nonwhite=0, married=1, employed=0), type = "response")
predict.ind <- predict(m.impeach, newdata=data.frame(partisanship="Independent", age=50, female=0, nohighschool=0, collegeorhigher=1, nonwhite=0, married=1, employed=0), type = "response")


