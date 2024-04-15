### PSC 400, Spring 2024
### Week 11, Tuesday 4/8



### SPATIAL DATA

rm(list=ls(all=TRUE))

library(maps)

data(us.cities)
head(us.cities)


map(database="usa")
points(x=us.cities$long, y=us.cities$lat, pch=16, cex=0.5)

# plot capitals
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






### ELECTION RESULTS

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





