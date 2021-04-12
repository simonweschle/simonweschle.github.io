
### Class Code from Apr 12

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

# get the mean margin of those polls and store in poll.pred
mean(latest$margin)



# what do I need to change if I want to repeat this?
# do loop first, then set up poll.pred


### now do this in a loop for all states
# extract unique state names which the loop will iterate through
st.names <- unique(polls08$state)
n <- length(st.names)


# set up vector that will store the latest polling margin
poll.pred <- rep(NA, n)

# as names, assign st.names
names(poll.pred) <- as.character(st.names)

for(i in 1:n){
	# get only state i
	state.data <- polls08[polls08$state==st.names[i],]

	# get only the latest poll
	latest <- state.data[state.data$DaysToElection==min(state.data$DaysToElection),]

	# get the mean margin of those polls and store in poll.pred
	poll.pred[i] <- mean(latest$margin)
}

poll.pred



# include poll.pred into pres08

pres08 <- cbind(pres08, poll.pred)

pres08$error <- pres08$margin - pres08$poll.pred

hist(pres08$error)
mean(pres08$error)


# plot 
plot(pres08$poll.pred, pres08$margin, pch=16, xlab="Poll Result", ylab="Election Result")
abline(h=0)
abline(v=0)
abline(a=0, b=1, lty="dashed")


# another way
plot(pres08$poll.pred, pres08$margin, pch=16, xlab="Poll Result", ylab="Election Result", type="n")
text(pres08$poll.pred, pres08$margin, pres08$state, cex=0.8)
abline(h=0)
abline(v=0)
abline(a=0, b=1, lty="dashed")







## how did the polls develop over time as the election approaches?
rm(list=ls(all=TRUE))


pollsUS08 <- read.csv("~/Dropbox/Teaching/2021_PSC_400/classes/week_10/data/pollsUS08.csv")

# turn into date
pollsUS08$middate <- as.Date(pollsUS08$middate)

## compute the number of days to Election Day
pollsUS08$DaysToElection <- as.Date("2008-11-04") - pollsUS08$middate


# get the mean poll result for Obama and McCain for each day in the 180 days before the election
dayvec <- 1:180
n <- length(dayvec)
obama.polls <- rep(NA, n)
mccain.polls <- rep(NA, n)

for(i in 1:n){
  daydata <- pollsUS08[pollsUS08$DaysToElection==dayvec[i],]
  obama.polls[i] <- mean(daydata$Obama)
  mccain.polls[i] <- mean(daydata$McCain)
}

