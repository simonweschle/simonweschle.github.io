
rm(list=ls(all=TRUE))

# set seed for random number generator (necessary to get same results every time you run code)
set.seed(123)

# simulate a population of 10 million people
competence.true <- runif(10000000, 0, 10)
support.true <- runif(10000000, 0, 10)

truedata <- data.frame(competence.true, support.true)

cor(truedata$support.true, truedata$competence.true)

reg1 <- lm(support.true ~ competence.true, data=truedata)
summary(reg1)


# randomly pick 1000 people for survey and invetigate effect of competence on support
subdata <- truedata[sample(10000000, 1000),]
reg2 <- lm(support.true ~ competence.true, data=subdata)
summary(reg2)

# do it again
subdata <- truedata[sample(10000000, 1000),]
reg2 <- lm(support.true ~ competence.true, data=subdata)
summary(reg2)

# and again
subdata <- truedata[sample(10000000, 1000),]
reg2 <- lm(support.true ~ competence.true, data=subdata)
summary(reg2)




# do this 1000 times (using a loop) and store coefficient of competence for each
coefvec <- NULL
for(i in 1:1000){
	subdata <- truedata[sample(10000000, 1000),]
	reg2 <- lm(support.true ~ competence.true, data=subdata)
	coefvec <- c(coefvec, coef(reg2)[2])
}


plot(coefvec, pch=16)

plot(density(coefvec), lwd=3, xlab="Density of Effects from 1000 Samples")
abline(v=coef(reg1)[2], col="red", lwd=3)
text(0, 0, "True Effect in Population", col="red", pos=4)



# do another 1000 times (using a loop), this time also storing whether significant at 5 percent level (1 if yes, 0 if no) as well as at 1 percent level
coefvec <- sigvec5 <- sigvec1 <- NULL
for(i in 1:1000){
	subdata <- truedata[sample(10000000, 1000),]
	reg2 <- lm(support.true ~ competence.true, data=subdata)
	coefvec <- c(coefvec, coef(reg2)[2])
	sigvec5 <- c(sigvec5, ifelse(summary(reg2)$coefficients[2,4] <= 0.05, 1, 0))
	sigvec1 <- c(sigvec1, ifelse(summary(reg2)$coefficients[2,4] <= 0.01, 1, 0))
}


plot(coefvec, pch=16, col=ifelse(sigvec5==1, "red", "black"))
table(sigvec5)


plot(density(coefvec), lwd=3, xlab="Density of Effects from 1000 Samples")
abline(v=min(coefvec[coefvec>0 & sigvec5==1]), lty="dashed")
abline(v=max(coefvec[coefvec<0 & sigvec5==1]), lty="dashed")
text(0.1, 13, "significant", col="red")
text(-0.1, 13, "significant", col="red")
text(0, 13, "not significant")


table(sigvec1)



# confidence interval
subdata <- truedata[sample(10000000, 1000),]
reg2 <- lm(support.true ~ competence.true, data=subdata)
summary(reg2)


confint(reg2)
# what does this mean?

# do another 1000 times (using a loop), storing each time whether the confidence interval contains the true effect or not
coefvec <- civec <- NULL
for(i in 1:1000){
	subdata <- truedata[sample(10000000, 1000),]
	reg2 <- lm(support.true ~ competence.true, data=subdata)
	civec <- c(civec, ifelse(coef(reg1)[2] >= confint(reg2)[2,1] & coef(reg1)[2] <= confint(reg2)[2,2], 1, 0))
}


table(civec)






