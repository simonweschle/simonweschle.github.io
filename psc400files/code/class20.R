### PSC 400, Spring 2022
### Week 10, Wednesday 4/6



### SPATIAL DATA

rm(list=ls(all=TRUE))

library(maps)

data(us.cities)


# plot election results
pres08 <- read.csv("~/Dropbox/Teaching/2022_1_PSC_400/classes/week_9/data/pres08.csv")

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


walmart <- read.csv("~/Dropbox/Teaching/2022_1_PSC_400/classes/week_10/data/walmart.csv")


# plot the locations of all walmart stores
map(database="state")
points(walmart$long, walmart$lat, pch=16, cex=0.4)



