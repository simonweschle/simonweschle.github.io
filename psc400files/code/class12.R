
### Class Code from Mar 17


## Election results in 08 and 12

rm(list=ls(all=TRUE))

pres12 <- read.csv("~/Dropbox/Teaching/2021_PSC_400/classes/week_6/data/pres12.csv")

pres08 <- read.csv("~/Dropbox/Teaching/2021_PSC_400/classes/week_6/data/pres08.csv")

colnames(pres08) <- c("state.name", "state", "Obama08", "McCain08", "EV08")

colnames(pres12) <- c("state", "Obama12", "Romney12", "EV12")

pres <- merge(pres08, pres12, by="state")

# DV: Obama12, IV: Obama08


reg1 <- lm(Obama12 ~ Obama08, data=pres)
summary(reg1)
# interpretation?


confint(reg1)
confint(reg1, level=0.99)


plot(pres$Obama08, pres$Obama12, pch=16, xlab="Obama Vote Share 2008", ylab="Obama Vote Share 2012")
abline(reg1, col="red", lwd=1.5)
abline(a=0, b=1, lty="dashed", lwd=1.5)



plot(pres$Obama08, pres$Obama12, pch=16, xlab="Obama Vote Share 2008", ylab="Obama Vote Share 2012")
abline(reg1, col="red", lwd=1.5)
# some states are above and some are below the red line. can investigate this a bit more


predict(reg1)
fitted(reg1)


# can put these predicted values in the data
pres$Obama12.predicted <- predict(reg1)


# difference between actual and predicted
pres$Obama12.difference <- pres$Obama12 - pres$Obama12.predicted


max(pres$Obama12.difference)
min(pres$Obama12.difference)


which(pres$Obama12.difference==min(pres$Obama12.difference))

summary(reg1)
summary(reg1)$r.squared


summary(reg1)$coefficient
coef(reg1)

confint(reg1)

