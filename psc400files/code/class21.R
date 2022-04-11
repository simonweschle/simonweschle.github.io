
### PSC 400, Spring 2022
### Week 11, Monday 4/11


# Walmart expansion

rm(list=ls(all=TRUE))

library(maps)


walmart <- read.csv("~/Dropbox/Teaching/2022_1_PSC_400/classes/week_10/data/walmart.csv")


# plot the locations of all walmart stores
map(database="state")
points(walmart$long, walmart$lat, pch=16, cex=0.4)


# let's color them by type: Wal-MartStore=red, SuperCenter=green, DistributionCenter=blue
walmart$color <- NA
walmart$color[walmart$type=="Wal-MartStore"] <- rgb(red=1, green=0, blue=0, alpha=0.8)
walmart$color[walmart$type=="SuperCenter"] <- rgb(red=0, green=1, blue=0, alpha=0.8)
walmart$color[walmart$type=="DistributionCenter"] <- rgb(red=0, green=0, blue=1, alpha=0.8)


map(database="state")
points(walmart$long, walmart$lat, col=walmart$color, pch=16, cex=0.5)

legend(x = -120, y = 32, bty = "n", legend = c("Walmart", "Supercenter", "Distribution center"), col = c("red", "green", "blue"), pch = 19)



# stores open on Jan 1 1970
walmart$opendate <- as.Date(walmart$opendate)
usedata <- walmart[walmart$opendate<"1970-01-01",]

map(database="state")
points(usedata$long, usedata$lat, col=usedata$color, pch=16, cex=0.5)


# play with it different cutoff dates to show the spread of Walmart

setwd("~/Dropbox/Teaching/2022_1_PSC_400/classes/week_10/data/animation/")

library("animation")

n <- 46
datevec <- seq(as.Date("1962-07-01"), as.Date("2007-07-01"), length.out=n)

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





rm(list=ls(all=TRUE))

library(ggmap)
library(tmaptools)


geocode_OSM("Syracuse, NY")



syracuse <- get_stamenmap(bbox = c(left = -76.20463, bottom = 42.98416, right = -76.07427, top = 43.08612), zoom = 13)
ggmap(syracuse)


syracuse_lines <- get_stamenmap(bbox = c(left = -76.20463, bottom = 42.98416, right = -76.07427, top = 43.08612), zoom = 13, maptype="terrain-lines")
ggmap(syracuse_lines)


geocode_OSM("Eggers Hall, Syracuse University")

ggmap(syracuse) + geom_point(aes(x = -76.13593, y = 43.03803), colour = "red", size = 2) + annotate('text', x=-76.13593, y=43.03803-0.002, label = 'Eggers Hall', colour = I('red'), size = 4)



syracuse_toner <- get_stamenmap(bbox = c(left = -76.20463, bottom = 42.98416, right = -76.07427, top = 43.08612), zoom = 13, maptype="toner",)
plot(syracuse_toner)


syracuse_watercolor <- get_stamenmap(bbox = c(left = -76.20463, bottom = 42.98416, right = -76.07427, top = 43.08612), zoom = 13, maptype="watercolor",)
plot(syracuse_watercolor)


accidents <- read.csv("~/Dropbox/Teaching/2022_1_PSC_400/classes/week_11/data/syracuse_accidents.csv")


ggmap(syracuse_lines) + geom_point(data = accidents, aes(x=Start_Lng, y=Start_Lat)) + theme( legend.position="none")


dens <- stat_density2d(data=accidents, aes(x=Start_Lng, y=Start_Lat, fill=..level.., alpha=..level..), contour=T, n=100, geom="polygon")

ggmap(syracuse_lines) + dens + theme(legend.position="none")

ggmap(syracuse_lines) + dens + theme(legend.position="none") + scale_fill_gradient(low = "white", high = "red", guide = FALSE)



ggmap(syracuse) + dens + theme(legend.position="none")










