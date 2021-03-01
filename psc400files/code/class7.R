
### Class Code from Mar 1


### graphs

rm(list=ls(all=TRUE))

minwage <- read.csv("/Users/simonweschle/Dropbox/Teaching/2021_PSC_400/classes/week_2/data/minwage.csv")


# Histogram
hist(minwage$wageBefore)

# Histogram with custom bins and a vertical line for where the mean wage is
hist(minwage$wageBefore, breaks=seq(4.25, 5.75, 0.10), main="Wage before Minimum Wage Increase", xlab="Wage")
abline(v=mean(minwage$wageBefore), col="red", lwd=3)
text(x=4.75, y=120, "Median", col="red")

# Density plot
plot(density(minwage$wageBefore))
points(density(minwage$wageAfter), type="l", col="red")

plot(density(minwage$wageBefore), ylim=c(0,4.5), xlim=c(4, 6.5))
points(density(minwage$wageAfter), type="l", col="red")

abline(v=mean(minwage$wageBefore), lwd=3)
abline(v=mean(minwage$wageAfter), lwd=3, col="red")








rm(list=ls(all=TRUE))

# codebook: https://www.qogdata.pol.gu.se/data/codebook_bas_jan21.pdf

data <- read.csv("~/Dropbox/Teaching/2021_PSC_400/classes/week_3/data/qog.csv")



