### PSC 400, Spring 2022
### Week 7, Monday 3/7




### UNCERTAINTY
rm(list=ls(all=TRUE))

# population approval rating
pop <- rbinom(10000000, 1, 0.45)

length(pop)
mean(pop)

# sample 1
sam1 <- sample(pop, 1000)
mean(sam1)

# sample 2
sam2 <- sample(pop, 1000)
mean(sam2)

# sample 3
sam3 <- sample(pop, 1000)
mean(sam3)

samplemeans <- NULL
for(i in 1:1000){
	sam <- sample(pop, 1000)
	samplemeans <- c(samplemeans, mean(sam))
}

samplemeans

mean(samplemeans)

hist(samplemeans)

plot(density(samplemeans))
abline(v=mean(samplemeans), col="red")
abline(v=mean(pop), col="darkgreen")


## CONFIDENCE INTERVALS FOR SAMPLE MEANS

# confidence interval for sam1
ci.low <- mean(sam1) - 1.96*(sqrt(var(sam1)/length(sam1)))
ci.low
ci.high <- mean(sam1) + 1.96*(sqrt(var(sam1)/length(sam1)))
ci.high

# does confidence interval contain true population mean?
ifelse(mean(pop)>ci.low & mean(pop)<ci.high, 1, 0)


# confidence interval for sam2
ci.low <- mean(sam2) - 1.96*(sqrt(var(sam2)/length(sam2)))
ci.low
ci.high <- mean(sam2) + 1.96*(sqrt(var(sam2)/length(sam2)))
ci.high


# or an easier way
t.test(sam2)$conf.int


# does confidence interval contain true population mean?
ifelse(mean(pop)>ci.low & mean(pop)<ci.high, 1, 0)


# do this 1000 times
containstrue <- NULL
for(i in 1:1000){
	sam <- sample(pop, 1000)
	ci.low <- mean(sam) - 1.96*(sqrt(var(sam)/length(sam)))
	ci.high <- mean(sam) + 1.96*(sqrt(var(sam)/length(sam)))
	inorout <-ifelse(mean(pop)>ci.low & mean(pop)<ci.high, 1, 0)
	containstrue <- c(containstrue, inorout)
}

table(containstrue)
mean(containstrue)



## CONFIDENCE INTERVAL BREXIT
rm(list=ls(all=TRUE))

bes <- read.csv("~/Dropbox/Teaching/2022_1_PSC_400/classes/week_3/data/BES.csv")

bes1 <- na.omit(bes)


mean(bes1$leave)

ci.low <- mean(bes1$leave) - 1.96*(sqrt(var(bes1$leave)/nrow(bes1)))
ci.low
ci.high <- mean(bes1$leave) + 1.96*(sqrt(var(bes1$leave)/nrow(bes1)))
ci.high

t.test(bes1$leave)$conf.int




## CONFIDENCE INTERVALS FOR DIFFERENCE IN MEANS
rm(list=ls(all=TRUE))

star <- read.csv("~/Dropbox/Teaching/2022_1_PSC_400/classes/week_2/data/STAR.csv")


# create treatment variable
star$small <- ifelse(star$classtype=="small", 1, 0)


# divide into treatment and control datasets
star_treat <- star[star$small==1,]
star_contr <- star[star$small==0,]

# difference in means, reading
mean(star_treat$reading) - mean(star_contr$reading)


# for confidence interval, need size of treatment and control groups
n_treat <- nrow(star_treat)
n_contr <- nrow(star_contr)


ci.low <- (mean(star_treat$reading) - mean(star_contr$reading)) - 1.96*sqrt((var(star_treat$reading)/n_treat) + (var(star_contr$reading)/n_contr))
ci.high <- (mean(star_treat$reading) - mean(star_contr$reading)) + 1.96*sqrt((var(star_treat$reading)/n_treat) + (var(star_contr$reading)/n_contr))


# a simpler way
t.test(reading ~ small, data=star)

t.test(reading ~ small, data=star)$estimate
t.test(reading ~ small, data=star)$conf.int
	# careful, this provides estimates for control - treatment



