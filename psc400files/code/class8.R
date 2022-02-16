### PSC 400, Fall 2022
### Week 4, Wednesday 2/16



## Ideology in US Congress
rm(list=ls(all=TRUE))

congress <- read.csv("~/Dropbox/Teaching/2022_1_PSC_400/classes/week_3/data/congress.csv")


# party median by Congress tapply(variable to compute, variable to group by, what to compute)
dem.median <- tapply(dem$dwnom1, dem$congress, median)
rep.median <- tapply(rep$dwnom1, rep$congress, median)

plot(x=names(dem.median), y=dem.median, type="l", ylim=c(-1, 1), col="blue", lwd=3, xlab="Congress", ylab="Economic liberalism/conservatism")
points(x=names(rep.median), y=rep.median, type="l", col="red", lwd=3)
abline(h=0, col="gray", lwd=2)
text(110, -0.6, "Democrats", col="blue")
text(110, 0.85, "Republicans", col="red")

points(dem$congress, dem$dwnom1, pch=16, cex=0.25, col="blue")
points(rep$congress, rep$dwnom1, pch=16, cex=0.25, col="red")





rm(list=ls(all=TRUE))

# codebook: https://www.qogdata.pol.gu.se/data/codebook_bas_jan22.pdf

data <- read.csv("~/Dropbox/Teaching/2022_1_PSC_400/classes/week_4/data/qog_bas_cs_jan22.csv")



# explore life expectancy
hist(data$wdi_lifexp, xlab="Life Expectancy at Birth", main="Life Expectancy at Birth")
hist(data$wdi_lifexp, xlab="Life Expectancy at Birth", main="Life Expectancy at Birth", freq=F)

plot(density(data$wdi_lifexp, na.rm=T), xlab="Life Expectancy at Birth", main="Life Expectancy at Birth")

boxplot(data$wdi_lifexp, ylab="Life Expectancy at Birth", main="Life Expectancy at Birth")

summary(data$wdi_lifexp)




# difference between male and female life expectancy
data$wdi_lifexp_diff <- data$wdi_lifexpf - data$wdi_lifexpm
hist(data$wdi_lifexp_diff, freq=F, xlab="Female - Male", main="Difference between Female and Male Life Expectancy at Birth")







