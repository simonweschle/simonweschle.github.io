### PSC 400, Spring 2024
### Week 11, Thursday 4/11



### SPATIAL DATA

### ELECTION RESULTS

rm(list=ls(all=TRUE))

library(maps)

data(us.cities)


# plot election results
pres08 <- read.csv("~/Dropbox/Teaching/2022_1_PSC_400/classes/week_9/data/pres08.csv")

## two-party vote share
pres08$Dem <- pres08$Obama / (pres08$Obama + pres08$McCain)
pres08$Rep <- pres08$McCain / (pres08$Obama + pres08$McCain)


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




rm(list=ls(all=TRUE))

library(ggmap)
library(tmaptools)


register_stadiamaps(key="be0e5c81-9dee-4951-88aa-061d02da4232")


geocode_OSM("Syracuse, NY")



syracuse <- get_stadiamap(bbox = c(left = -76.20463, bottom = 42.98416, right = -76.07427, top = 43.08612), zoom = 13)
ggmap(syracuse)


syracuse_lines <- get_stadiamap(bbox = c(left = -76.20463, bottom = 42.98416, right = -76.07427, top = 43.08612), zoom = 13, maptype="stamen_terrain_lines")
ggmap(syracuse_lines)


geocode_OSM("Maxwell Hall, Syracuse University")
addpoints <- data.frame(lon=-76.13646, lat=43.03820)

ggmap(syracuse) + geom_point(data = addpoints, aes(x = lon, y = lat), color = "orange", size = 4)
ggmap(syracuse) + geom_point(data = addpoints, aes(x = lon, y = lat), color = "orange", size = 4) + annotate('text', x=addpoints$lon, y=addpoints$lat+0.002, label = 'Eggers Hall', colour = "orange", size = 4)




syracuse_toner <- get_stadiamap(bbox = c(left = -76.20463, bottom = 42.98416, right = -76.07427, top = 43.08612), zoom = 13, maptype="stamen_toner",)
plot(syracuse_toner)






