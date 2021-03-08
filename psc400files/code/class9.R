
### Class Code from Mar 8


rm(list=ls(all=TRUE))

data <- read.csv("~/Dropbox/Teaching/2021_PSC_400/classes/week_3/data/qog.csv")



# regimetype
table(data$ht_regtype1)

# first way: ifelse
data$dem <- ifelse(data$ht_regtype1==100, "Democracy", "Not Democracy")
data$dem <- as.factor(data$dem)
table(data$dem)

# another way, I told you it works equally well
data$dem2 <- "Not Democracy"
data$dem2[data$ht_regtype1==100] <- "Democracy"
data$dem2 <- as.factor(data$dem2)
table(data$dem2)

# but results are different (ALWAYS CHECK!). problem: second way counts NAs as Not Democracy since that's what I told it to do.
data$dem3 <- NA
data$dem3[data$ht_regtype1==100] <- "Democracy"
data$dem3[data$ht_regtype1<100] <- "Not Democracy"
data$dem3 <- as.factor(data$dem3)
table(data$dem3)
	# takeaway: if you build a variable that way, start with NA and then add each condition




#  create a new variable of electoral system
data$gol_est_simple <- NA
data$gol_est_simple[data$gol_est_spec==1] <- "Majoritarian"
data$gol_est_simple[data$gol_est_spec==2] <- "Majoritarian"
data$gol_est_simple[data$gol_est_spec==3] <- "Majoritarian"
data$gol_est_simple[data$gol_est_spec==4] <- "Majoritarian"
data$gol_est_simple[data$gol_est_spec==5] <- "Majoritarian"
data$gol_est_simple[data$gol_est_spec==6] <- "Majoritarian"
data$gol_est_simple[data$gol_est_spec==7] <- "Majoritarian"
data$gol_est_simple[data$gol_est_spec==8] <- "Majoritarian"
data$gol_est_simple[data$gol_est_spec==9] <- "Proportional"
data$gol_est_simple[data$gol_est_spec==10] <- "Proportional"
data$gol_est_simple[data$gol_est_spec==11] <- "Mixed"
data$gol_est_simple[data$gol_est_spec==12] <- "Mixed"
data$gol_est_simple <- as.factor(data$gol_est_simple)


# do the same thing with less code:
data$gol_est_simple <- NA
data$gol_est_simple[data$gol_est_spec==1 | data$gol_est_spec==2 | data$gol_est_spec==3 | data$gol_est_spec==4 | data$gol_est_spec==5 | data$gol_est_spec==6 | data$gol_est_spec==7 | data$gol_est_spec==8] <- "Majoritarian"
data$gol_est_simple[data$gol_est_spec==9 | data$gol_est_spec==10] <- "Proportional"
data$gol_est_simple[data$gol_est_spec==11 | data$gol_est_spec==12] <- "Mixed"
data$gol_est_simple <- as.factor(data$gol_est_simple)



# do the same thing with even less code:
data$gol_est_simple <- NA
data$gol_est_simple[data$gol_est_spec<=8] <- "Majoritarian"
data$gol_est_simple[data$gol_est_spec>=9 & data$gol_est_spec<=10] <- "Proportional"
data$gol_est_simple[data$gol_est_spec>=11] <- "Mixed"
data$gol_est_simple <- as.factor(data$gol_est_simple)


# do it with just one line of code
library(car)

data$gol_est_simple <- recode(data$gol_est_spec, " c(1, 2, 3, 4, 5, 6, 7, 8) = 'Majoritarian'; c(9, 10) = 'Proportional'; c(11, 12) = 'Mixed'  ")






rm(list=ls(all=TRUE))

congress <- read.csv("~/Dropbox/Teaching/2021_PSC_400/classes/week_5/data/congress.csv")

hist(congress$dwnom1)
hist(congress$dwnom2)

# create separate datasets for Dem and Rep. Two ways of doing that
dem <- congress[congress$party=="Democrat",]
rep <- subset(congress, subset=(party=="Republican"))


hist(dem$dwnom1, xlim=c(-1.5, 1.5))
hist(rep$dwnom1, xlim=c(-1.5, 1.5))


# plot first and second dimension for Dems and Reps
plot(x=dem$dwnom1, y=dem$dwnom2, xlim=c(-1.5, 1.5), ylim=c(-1.5, 1.5), pch=16, col="blue", cex=0.3, xlab="Economic liberalism/conservatism", ylab="Racial liberalism/conservatism")
points(x=rep$dwnom1, y=rep$dwnom2, pch=16, col="red", cex=0.3)


