
### Class Code from Apr 19

rm(list=ls(all=TRUE))

library(maps)

# plot election results
pres08 <- read.csv("~/Dropbox/Teaching/2021_PSC_400/classes/week_9/data/pres08_week9.csv")

## two-party vote share
pres08$Dem <- pres08$Obama / (pres08$Obama + pres08$McCain)
pres08$Rep <- pres08$McCain / (pres08$Obama + pres08$McCain)

pres08$color <- ifelse(pres08$Dem > pres08$Rep, "blue", "red")


# map of one state
map(database="state", regions="New York", col=pres08$color[pres08$state.name=="New York"], fill=TRUE)


# map of all states
map(database = "state")
map(database="state", regions="New York", col=pres08$color[pres08$state.name=="New York"], fill=TRUE, add=TRUE)

map(database="state", regions="Texas", col=pres08$color[pres08$state.name=="Texas"], fill=TRUE, add=TRUE)


# using a loop
map(database = "state")

statevec <- unique(pres08$state.name)
statevec <- statevec[statevec!="Alaska"]
statevec <- statevec[statevec!="Hawaii"]
statevec <- statevec[statevec!="D.C."]

n <- length(statevec)

for(i in 1:n){
	map(database="state", regions=statevec[i], col=pres08$color[pres08$state.name==statevec[i]], fill=TRUE, add=TRUE)
}




# using mix of red and blue by vote share instaead of binary red/blue
pres08$color.purple <- rgb(red=pres08$Rep, blue=pres08$Dem, green=0)


map(database = "state")

statevec <- unique(pres08$state.name)
statevec <- statevec[statevec!="Alaska"]
statevec <- statevec[statevec!="Hawaii"]
statevec <- statevec[statevec!="D.C."]

n <- length(statevec)

for(i in 1:n){
	map(database="state", regions=statevec[i], col=pres08$color.purple[pres08$state.name==statevec[i]], fill=TRUE, add=TRUE)
}




# Walmart expansion

rm(list=ls(all=TRUE))

library(maps)


walmart <- read.csv("~/Dropbox/Teaching/2021_PSC_400/classes/week_10/data/walmart.csv")


# plot the locations of all walmart stores
map(database="state")
points(walmart$long, walmart$lat, pch=16, cex=0.4)



# let's color them by type: Wal-MartStore=red, SuperCenter=green, DistributionCenter=blue
walmart$color <- NA
walmart$color[walmart$type=="Wal-MartStore"] <- rgb(red=1, green=0, blue=0, alpha=0.5)
walmart$color[walmart$type=="SuperCenter"] <- rgb(red=0, green=1, blue=0, alpha=0.5)
walmart$color[walmart$type=="DistributionCenter"] <- rgb(red=0, green=0, blue=1, alpha=0.5)


map(database="state")
points(walmart$long, walmart$lat, col=walmart$color, pch=16, cex=0.5)

legend(x = -120, y = 32, bty = "n", legend = c("Walmart", "Supercenter", "Distribution center"), col = c("red", "green", "blue"), pch = 19)



# stores open on Jan 1 1970
walmart$opendate <- as.Date(walmart$opendate)
usedata <- walmart[walmart$opendate<"1970-01-01",]

map(database="state")
points(usedata$long, usedata$lat, col=usedata$color, pch=16, cex=0.5)


# play with it different cutoff dates to show the spread of Walmart

setwd("~/Dropbox/Teaching/2021_PSC_400/classes/week_10/data/animation/")

library("animation")

n <- 45
datevec <- seq(as.Date("1963-01-01"), as.Date("2007-01-01"), length.out=n)

saveHTML(
  {
	  for(i in 1:n){
		  usedata <- walmart[walmart$opendate<datevec[i],]
		  year <- substr(datevec[i], 1, 4)
		  map(database="state")
		  title(year)
		  points(usedata$long, usedata$lat, col=usedata$color, pch=16, cex=usedata$pointsize)
	  }
  },
htmlfile="walmart.html", outdir=getwd(), autobrowse=FALSE)





