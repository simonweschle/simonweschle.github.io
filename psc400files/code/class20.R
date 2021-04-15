
### Class Code from Apr 14



## write a loop that computes 1.5*1 + 1, 1.5*2 + 2, 1.5*3 + 3, ..., 1.5*2000 + 2000 and store in vector
vec <- 1:2000
n <- length(vec)
result <- rep(NA, n)

for(i in 1:n){
	result[i] <- 1.5*vec[i] + vec[i]
}


# now write a loop that computes 1.5*2 + 2, 1.5*4 + 4, 1.5*6 + 6, ..., 1.5*2000 + 2000
vec <- seq(2, 2000, 2)
n <- length(vec)
result <- rep(NA, n)

for(i in 1:n){
	result[i] <- 1.5*vec[i] + vec[i]
}



## how did the polls develop over time as the election approaches?
rm(list=ls(all=TRUE))


pollsUS08 <- read.csv("~/Dropbox/Teaching/2021_PSC_400/classes/week_10/data/pollsUS08.csv")

# turn into date
pollsUS08$middate <- as.Date(pollsUS08$middate)

## compute the number of days to Election Day
pollsUS08$DaysToElection <- pollsUS08$middate - as.Date("2008-11-04") 


# get the mean poll result for Obama and McCain for each day in the 180 days before the election
dayvec <- seq(-180, -1, 1)
n <- length(dayvec)
obama.polls <- rep(NA, n)
mccain.polls <- rep(NA, n)

for(i in 1:n){
  daydata <- pollsUS08[pollsUS08$DaysToElection==dayvec[i],]
  obama.polls[i] <- mean(daydata$Obama)
  mccain.polls[i] <- mean(daydata$McCain)
}


polldata <- data.frame(dayvec, obama.polls, mccain.polls)
colnames(polldata) <- c("DaysToElection", "Obama", "McCain")

plot(polldata$DaysToElection, polldata$Obama, pch=16, ylim=c(35, 55), col="blue", xlab="Days to Election", ylab="Poll Average")
points(polldata$DaysToElection, polldata$McCain, pch=16, col="red")

points(0, 52.93, col="blue", pch=8, cex=2)
points(0, 45.65, col="red", pch=8, cex=2)


loess.obama <- loess.smooth(polldata$DaysToElection, polldata$Obama, span=0.25)
points(loess.obama$x, loess.obama$y, type="l", col="blue", lwd=3)

loess.mccain <- loess.smooth(polldata$DaysToElection, polldata$McCain, span=0.25)
points(loess.mccain$x, loess.mccain$y, type="l", col="red", lwd=3)





### SPATIAL DATA

rm(list=ls(all=TRUE))

library(maps)

data(us.cities)
head(us.cities)


# plot capitals
map(database="usa")
points(x=us.cities$long, y=us.cities$lat, pch=16, cex=0.5)


capitals <- subset(us.cities, capital==2)
map(database="usa")
points(x=capitals$long, y=capitals$lat, col="blue", pch=16)


map(database="state")
points(x=capitals$long, y=capitals$lat, col="blue", pch=16)

map(database="state")
points(x=capitals$long, y=capitals$lat, col="blue", pch=16, cex=capitals$pop/500000)



# plot cities in NY
ny.cities <- subset(us.cities, country.etc=="NY")
pop.ord <- order(ny.cities$pop, decreasing=TRUE)
top5 <- pop.ord[1:5]


map(database="state", regions="New York", col="gray")
points(x=ny.cities$long[top5], y=ny.cities$lat[top5], pch=16)
text(x = ny.cities$long[top5], y = ny.cities$lat[top5], label = ny.cities$name[top5], pos=4)




