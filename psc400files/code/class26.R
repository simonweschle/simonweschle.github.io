
### PSC 400, Spring 2022
### Week 13, Wednesday 4/27



### PS 5 Review

rm(list=ls(all=TRUE))

intrade08 <- read.csv("https://simonweschle.github.io/psc400files/data/intrade08.csv")
pres08 <- read.csv("https://simonweschle.github.io/psc400files/data/pres08.csv")


# Q1
intrade08 <- merge(intrade08, pres08, by="state")

intrade08$day <- as.Date(intrade08$day)
intrade08$daystoelection <- intrade08$day - as.Date("2008-11-04")

intrade08$obamapred <- ifelse(intrade08$PriceD > intrade08$PriceR, 1, 0)

# 120 days before 
usedata <- intrade08[intrade08$daystoelection==-120,]
sum(usedata$EV[usedata$obamapred==1])


# 119 days before 
usedata <- intrade08[intrade08$daystoelection==-119,]
sum(usedata$EV[usedata$obamapred==1])



dayvec <- seq(-120, -1, 1)
n <- length(dayvec)
obamaEV.trade <- rep(NA, n)

for(i in 1:n){
	usedata <- intrade08[intrade08$daystoelection==dayvec[i],]
	obamaEV.trade[i] <- sum(usedata$EV[usedata$obamapred==1])
}


plot(dayvec, obamaEV.trade, type="l")
abline(h=365)



# Q2

polls08 <- read.csv("https://simonweschle.github.io/psc400files/data/polls08_ps5.csv")
polls08 <- merge(polls08, pres08, by="state")


polls08$obamapred <- ifelse(polls08$pollsD > polls08$pollsR, 1, 0)


polls08$middate <- as.Date(polls08$middate)
polls08$daystoelection <- polls08$middate - as.Date("2008-11-04")


# 120 days before 
usedata <- polls08[polls08$daystoelection==-120,]
sum(usedata$EV[usedata$obamapred==1])


# 119 days before 
usedata <- polls08[polls08$daystoelection==-119,]
sum(usedata$EV[usedata$obamapred==1])



dayvec <- seq(-120, -1, 1)
n <- length(dayvec)
obamaEV.polls <- rep(NA, n)

for(i in 1:n){
	usedata <- polls08[polls08$daystoelection==dayvec[i],]
	obamaEV.polls[i] <- sum(usedata$EV[usedata$obamapred==1])
}


plot(dayvec, obamaEV.trade, type="l")
points(dayvec, obamaEV.polls, type="l", col="red")
abline(h=365)



### MORE ON GRAPHS

rm(list=ls(all=TRUE))


data <- read.csv("~/Dropbox/Teaching/2022_1_PSC_400/classes/week_4/data/qog_bas_cs_jan22.csv")


data$regimetype <- NA
data$regimetype[data$p_polity2>=6] <- "Democracy"
data$regimetype[data$p_polity2<=-6] <- "Autocracy"
data$regimetype[data$p_polity2<=5 & data$p_polity2>=-5] <- "Mixed"


# plot regimetype 

barplot(prop.table(table(data$regimetype)))

barplot(prop.table(table(data$regimetype)), col=c("#66c2a5", "#fc8d62", "#8da0cb"))


# plot corruption index
hist(data$bci_bci, freq=F)

d <- density(data$bci_bci, na.rm=T)
plot(d)
polygon(d, col="#66c2a5")


boxplot(data$bci_bci)

install.packages("vioplot")
library(vioplot)

vioplot(data$bci_bci, col="#66c2a5")




# two variables
d.dem <- density(data$bci_bci[data$regimetype=="Democracy"], na.rm=T)
d.aut <- density(data$bci_bci[data$regimetype=="Autocracy"], na.rm=T)
d.mix <- density(data$bci_bci[data$regimetype=="Mixed"], na.rm=T)


plot(d.dem)
points(d.aut, type="l")
points(d.mix, type="l")


plot(d.dem, ylim=c(0, 0.04))
polygon(d.dem, col="#66c2a5")

points(d.aut, type="l")
polygon(d.aut, col="#fc8d62")

points(d.mix, type="l")
polygon(d.mix, col="#8da0cb")


library(grDevices)
adjustcolor("#66c2a5", alpha=0.2)


plot(d.dem, ylim=c(0, 0.04))
polygon(d.dem, col=adjustcolor("#66c2a5", alpha=0.2))

points(d.aut, type="l")
polygon(d.aut, col=adjustcolor("#fc8d62", alpha=0.2))

points(d.mix, type="l")
polygon(d.mix, col=adjustcolor("#8da0cb", alpha=0.2))

legend("topleft", legend=c("Democracy", "Autocracy", "Mixed"), fill=c(adjustcolor("#66c2a5", alpha=0.2), adjustcolor("#fc8d62", alpha=0.2), adjustcolor("#8da0cb")))
# not great

# better:
boxplot(bci_bci ~ regimetype, data=data, col=c("#66c2a5", "#fc8d62", "#8da0cb"))

vioplot(bci_bci ~ regimetype, data=data, col=c("#66c2a5", "#fc8d62", "#8da0cb"))


