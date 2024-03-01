### PSC 400, Spring 2024
### Week 6, Tuesday 2/27


### GDP and PRIOR GDP
rm(list=ls(all=TRUE))

countries <- read.csv("~/Dropbox/Teaching/2024_1_PSC_400/classes/week_6/data/countries.csv")

plot(countries$prior_gdp, countries$gdp)




### Election results in 08 and 12

rm(list=ls(all=TRUE))

pres12 <- read.csv("~/Dropbox/Teaching/2024_1_PSC_400/classes/week_6/data/pres12.csv")
colnames(pres12) <- c("state", "Obama12", "Romney12", "EV12")


pres08 <- read.csv("~/Dropbox/Teaching/2024_1_PSC_400/classes/week_6/data/pres08.csv")
colnames(pres08) <- c("state.name", "state", "Obama08", "McCain08", "EV08")

# merge them by state
pres <- merge(pres08, pres12, by="state")



# Exercise
plot(pres$Obama08, pres$Obama12, pch=16, xlab="Obama Vote Share 2008", ylab="Obama Vote Share 2012")

reg1 <- lm(Obama12 ~ Obama08, data=pres)
reg1

abline(reg1, col="red", lwd=2)

-4.960 + 50*1.052


summary(reg1)
summary(reg1)$r.squared




# predict 2012 vote share for every state using the regression line
predict(reg1)
pres$Obama12_predicted <- predict(reg1)


# plot predicted vs actual vote share
plot(pres$Obama12_predicted, pres$Obama12, pch=16, xlab="Predicted Obama Vote Share", ylab="Actual Obama Vote Share")
abline(0,1, col="red", lwd=2) # observations on this line means predicted vote share is equal to actual vote share, above means he outperformed prediciton



# now we don't have the dots but instead we have the state name in the graph
plot(pres$Obama12_predicted, pres$Obama12, type="n", xlab="Predicted Obama Vote Share", ylab="Actual Obama Vote Share")
abline(0,1, col="red", lwd=1.5)
text(x=pres$Obama12_predicted, y=pres$Obama12, labels=pres$state, cex=0.6)












