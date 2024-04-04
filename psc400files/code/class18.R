### PSC 400, Spring 2024
### Week 10, Tuesday 4/2



rm(list=ls(all=TRUE))

cces <- read.csv("~/Dropbox/Teaching/2024_1_PSC_400/classes/week_9/data/cces19.csv")

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






### PREDICTION

rm(list=ls(all=TRUE))


## load election results, by state
pres08 <- read.csv("~/Dropbox/Teaching/2022_1_PSC_400/classes/week_9/data/pres08.csv")
## load polling data
polls08 <- read.csv("~/Dropbox/Teaching/2022_1_PSC_400/classes/week_9/data/polls08.csv")



## compute Obama’s margin
polls08$margin <- polls08$Obama - polls08$McCain
pres08$margin <- pres08$Obama - pres08$McCain


# turn into date
polls08$middate <- as.Date(polls08$middate)

## compute the number of days to Election Day
polls08$DaysToElection <- as.Date("2008-11-04") - polls08$middate




### goal: get the margin of the latest polls condicted in a given state

# start with Alabama: subset data to only get polls from AL
state.data <- polls08[polls08$state=="AL",]

# get only the latest poll
latest <- state.data[state.data$DaysToElection==min(state.data$DaysToElection),]

# get the mean margin of those polls
mean(latest$margin)


# what do I need to change if I want to repeat this for every state?


# interlude: loops

# print 1 to 3
for(i in 1:3){
	print(i)
}

# compute 1.5*1, 1.5*2, 1.5*3, ..., 1.5*500 and print 
for(i in 1:500){
	print(1.5*i)
}



