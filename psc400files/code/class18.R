
### Class Code from Apr 7

rm(list=ls(all=TRUE))

cces <- read.csv("~/Dropbox/Teaching/2021_PSC_400/classes/week_9/data/cces19_week9.csv")


# age + partisanship
m.impeach <- glm(impeach ~ age + partisanship, data=cces, family = binomial)
summary(m.impeach)

agevec <- seq(18, 100, 1)


predict(m.impeach, newdata=data.frame(partisanship="Republican", age=50), type = "response")
predict(m.impeach, newdata=data.frame(partisanship="Democrat", age=50), type = "response")
predict(m.impeach, newdata=data.frame(partisanship="Independent", age=50), type = "response")



# interaction age & partisanship
m.impeach.ia <- glm(impeach ~ partisanship + age + partisanship:age, data=cces, family = binomial)
summary(m.impeac.ia)

predict.age.rep <- predict(m.impeach.ia, newdata=data.frame(partisanship="Republican", age=agevec), type = "response")
predict.age.dem <- predict(m.impeach.ia, newdata=data.frame(partisanship="Democrat", age=agevec), type = "response")
predict.age.ind <- predict(m.impeach.ia, newdata=data.frame(partisanship="Independent", age=agevec), type = "response")


plot(agevec, predict.age.rep, type="l", ylim=c(0,1), xlab="Age", ylab="Predicted Probability of Supporting Impeachment", lwd=2, col="red")
points(agevec, predict.age.dem, type="l", lwd=2, col="blue")
points(agevec, predict.age.ind, type="l", lwd=2, col="black")



### loops

rm(list=ls(all=TRUE))

# what is the result of 1*8, 2*8, 3*8, ... 20*8

values <- seq(from=1, to=20, by=1)
values <- 1:20

n <- length(values)
results <- rep(NA, n)

# test with i=1, then run
for(i in 1:n){
	results[i] <- values[i] * 8
}

# include print of loop
for(i in 1:n){
	results[i] <- values[i] * 8
	cat("iteration", i, "\n")
}

for(i in 1:n){
  results[i] <- values[i] * 8
  cat(i, "times 8 is", values[i] * 8, "\n")
}




# Predicting election outcomes

rm(list=ls(all=TRUE))


## load election results, by state
pres08 <- read.csv("~/Dropbox/Teaching/2021_PSC_400/classes/week_9/data/pres08_week9.csv")
## load polling data
polls08 <- read.csv("~/Dropbox/Teaching/2021_PSC_400/classes/week_9/data/polls08_week9.csv")

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




