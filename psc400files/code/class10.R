### PSC 400, Fall 2022
### Week 5, Wednesday 2/23


### GDP and NIGHT TIME EMISSIONS
rm(list=ls(all=TRUE))

countries <- read.csv("~/Dropbox/Teaching/2022_1_PSC_400/classes/week_5/data/countries.csv")


countries$gdp_change <- ((countries$gdp - countries$prior_gdp)/countries$prior_gdp)*100
countries$light_change <- ((countries$light - countries$prior_light)/countries$prior_light)*100

hist(countries$gdp_change)
hist(countries$gdp_change, breaks=30)

hist(countries$light_change, breaks=30)

plot(countries$light_change, countries$gdp_change, pch=16, xlab="Light Emission Change", ylab="GDP Change")


cor(countries$light_change, countries$gdp_change)


model1 <- lm(gdp_change ~ light_change, data=countries)
model1

# light change is 20%
49.8202 + 20 * 0.2546

abline(model1, col="red", lwd=2)


summary(model1)$r.squared



### Election results in 08 and 12

rm(list=ls(all=TRUE))

pres12 <- read.csv("~/Dropbox/Teaching/2022_1_PSC_400/classes/week_5/data/pres12.csv")
colnames(pres12) <- c("state", "Obama12", "Romney12", "EV12")


pres08 <- read.csv("~/Dropbox/Teaching/2022_1_PSC_400/classes/week_5/data/pres08.csv")
colnames(pres08) <- c("state.name", "state", "Obama08", "McCain08", "EV08")

# merge them by state
pres <- merge(pres08, pres12, by="state")


plot(pres$Obama08, pres$Obama12, pch=16, xlab="Obama Vote Share 2008", ylab="Obama Vote Share 2012")


reg1 <- lm(Obama12 ~ Obama08, data=pres)
reg1

abline(reg1, col="red", lwd=2)

-4.960 + 50*1.052


predict(reg1)
pres$Obama12_predicted <- predict(reg1)


plot(pres$Obama12_predicted, pres$Obama12, pch=16, xlab="Predicted Obama Vote Share", ylab="Actual Obama Vote Share")
abline(0,1, col="red", lwd=2)




plot(pres$Obama12_predicted, pres$Obama12, type="n", xlab="Predicted Obama Vote Share", ylab="Actual Obama Vote Share")
abline(0,1, col="red", lwd=1.5)
text(x=pres$Obama12_predicted, y=pres$Obama12, labels=pres$state, cex=0.6)


summary(reg1)
summary(reg1)$r.squared




