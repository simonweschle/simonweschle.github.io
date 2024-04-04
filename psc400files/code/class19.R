### PSC 400, Spring 2024
### Week 10, Thursday 4/4



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


# compute 1.5*1, 1.5*2, 1.5*3, ..., 1.5*500 and store in vector outcome
outcome <- rep(NA, 500) 
for(i in 1:500){
	outcome[i] <- 1.5*i
}


# compute 1.5*1 + 1, 1.5*2 + 2, 1.5*3 + 3, ..., 1.5*500 + 500 and store in vector outcome
outcome <- rep(NA, 500) 
for(i in 1:500){
	outcome[i] <- 1.5*i + i
}



# back to prediction

statevec <- unique(polls08$state)
length(statevec)

poll.pred <- rep(NA, 51)
for(i in 1:51){
	# subset data to only get polls from state
	state.data <- polls08[polls08$state==statevec[i],]

	# get only the latest poll
	latest <- state.data[state.data$DaysToElection==min(state.data$DaysToElection),]

	# get the mean margin of those polls and store in poll.pred
	poll.pred[i] <- mean(latest$margin)
}


# put state names and poll margins into one dataset
polldata <- data.frame(statevec, poll.pred)
colnames(polldata) <- c("state", "poll.pred")

pres08 <- merge(pres08, polldata, by="state")


plot(pres08$poll.pred, pres08$margin, type="n")
text(pres08$poll.pred, pres08$margin, pres08$state)
abline(h=0, col="gray")
abline(v=0, col="gray")
abline(0, 1, lty=2)


# did polls call state correctly?
pres08$correct <- ifelse(sign(pres08$poll.pred)==sign(pres08$margin), 1, 0)

table(pres08$correct)





### SPATIAL DATA

rm(list=ls(all=TRUE))

library(maps)

data(us.cities)
head(us.cities)


# plot cities
map(database="usa")
points(x=us.cities$long, y=us.cities$lat, pch=16, cex=0.5)



