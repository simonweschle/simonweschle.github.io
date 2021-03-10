
### Class Code from Mar 10

rm(list=ls(all=TRUE))

congress <- read.csv("~/Dropbox/Teaching/2021_PSC_400/classes/week_5/data/congress.csv")

dem <- congress[congress$party=="Democrat",]
rep <- congress[congress$party=="Republican",]

# focus on 80th (47-49) and 112th Congress (11-13), by party
dem80 <- congress[congress$party=="Democrat" & congress$congress==80,]
dem112 <- congress[congress$party=="Democrat" & congress$congress==112,]

rep80 <- congress[congress$party=="Republican" & congress$congress==80,]
rep112 <- congress[congress$party=="Republican" & congress$congress==112,]



# plot relation between DW dimension 1 and 2 for R and D separately, for 80th and 112th Congress
plot(dem80$dwnom1, dem80$dwnom2, xlim=c(-1.5, 1.5), ylim=c(-1.5, 1.5), pch=16, col="blue", main="80th Congress", xlab="Economic liberalism/conservatism", ylab="Racial liberalism/conservatism")
points(rep80$dwnom1, rep80$dwnom2, col="red", pch=18)
text(x=-0.75, y=1, "Democrats", col="blue")
text(x=1, y=-1, "Republicans", col="red")


plot(dem112$dwnom1, dem112$dwnom2, xlim=c(-1.5, 1.5), ylim=c(-1.5, 1.5), pch=16, col="blue", main="112th Congress", xlab="Economic liberalism/conservatism", ylab="Racial liberalism/conservatism")
points(rep112$dwnom1, rep112$dwnom2, col="red", pch=18)
text(-0.75, 1, "Democrats", col="blue")
text(1, -1, "Republicans", col="red")



# party median by Congress tapply(variable to compute, variable to group by, what to compute)
dem.median <- tapply(dem$dwnom1, dem$congress, median)
rep.median <- tapply(rep$dwnom1, rep$congress, median)

plot(x=names(dem.median), y=dem.median, type="l", ylim=c(-1, 1), col="blue", lwd=3, xlab="Congress", ylab="Economic liberalism/conservatism")
points(x=names(rep.median), y=rep.median, type="l", col="red", lwd=3)
text(110, -0.6, "Democrats", col="blue")
text(110, 0.85, "Republicans", col="red")




## Polarization and income inequality

rm(list=ls(all=TRUE))

data <- read.csv("~/Dropbox/Teaching/2021_PSC_400/classes/week_5/data/polarization_gini.csv")

data$polarization <- data$rep.median - data$dem.median

plot(data$congress, data$polarization, type="l", lwd=3)

plot(data$congress, data$gini, type="l", lwd=3)

plot(data$polarization, data$gini, pch=16, xlab="Polarization", ylab="Income Inequality", cex=1.5)

cor(data$polarization, data$gini)

