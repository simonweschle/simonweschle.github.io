### PSC 400, Spring 2024
### Week 9, Tuesday 3/26


## HYPOTHESIS TESTING, QUALITY OF GOVERNMENT
rm(list=ls(all=TRUE))

data <- read.csv("~/Dropbox/Teaching/2024_1_PSC_400/classes/week_4/data/qog_bas_cs_jan24.csv")

data$litdiff <- data$wdi_litradm - data$wdi_litradf
hist(data$litdiff)

model1 <- lm(litdiff ~ p_polity2 + wdi_expedu + wbgi_gee, data=data)
summary(model1)
confint(model1)


summary(data$p_polity2)

# creates a vector of values for p_polity2 from min to max
polity.vec <- seq(from=-10, to=10, by=1)


# predictions
predict.polity <- predict(model1, newdata=data.frame(p_polity2=polity.vec, wdi_expedu=mean(data$wdi_expedu, na.rm=T), wbgi_gee=mean(data$wbgi_gee, na.rm=T)))


# plot predicted values
plot(polity.vec, predict.polity, type="l", ylim=c(0,10), xlab="Polity Score", ylab="Predicted Literacy Gap Men - Women", lwd=3)


# now plot with confidence interval
predict.polity.ci <- predict(model1, newdata=data.frame(p_polity2=polity.vec, wdi_expedu=mean(data$wdi_expedu, na.rm=T), wbgi_gee=mean(data$wbgi_gee, na.rm=T)), interval="confidence", level = 0.95)

plot(polity.vec, predict.polity.ci[,1], type="l", ylim=c(0,10), xlab="Polity Score", ylab="Predicted Literacy Gap Men - Women", lwd=3)
# lower boundary of CI
points(polity.vec, predict.polity.ci[,2], type="l", lty=2, lwd=3)
# upper boundary of CI
points(polity.vec, predict.polity.ci[,3], type="l", lty=2, lwd=3)




# INTERACTION EFFECT

rm(list=ls(all=TRUE))

socialdata <- read.csv("~/Dropbox/Teaching/2024_1_PSC_400/classes/week_9/data/social.csv")


# regular model
model <- lm(primary2006 ~ neighbors + age, data=socialdata)
summary(model)


predict(model, newdata=data.frame(neighbors=1, age=20)) - predict(model, newdata=data.frame(neighbors=0, age=20))
predict(model, newdata=data.frame(neighbors=1, age=80)) - predict(model, newdata=data.frame(neighbors=0, age=80))



# interaction model
model.ia <- lm(primary2006 ~ neighbors + age + neighbors:age, data=socialdata)
summary(model.ia)


predict(model.ia, newdata=data.frame(neighbors=1, age=20)) - predict(model.ia, newdata=data.frame(neighbors=0, age=20))
predict(model.ia, newdata=data.frame(neighbors=1, age=80)) - predict(model.ia, newdata=data.frame(neighbors=0, age=80))




