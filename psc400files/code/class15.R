### PSC 400, Spring 2022
### Week 8, Monday 3/21


## HYPOTHESIS TESTING, QUALITY OF GOVERNMENT
rm(list=ls(all=TRUE))

data <- read.csv("~/Dropbox/Teaching/2022_1_PSC_400/classes/week_4/data/qog_bas_cs_jan22.csv")

data$litdiff <- data$wdi_litradm - data$wdi_litradf
hist(data$litdiff)

model1 <- lm(litdiff ~ p_polity2 + wdi_expedu + wbgi_gee, data=data)
summary(model1)
confint(model1)



# plot the effect of government effectiveness (wbgi_gee)
summary(data$wbgi_gee)

# creates a vector of values for wbgi_gee from min to max
effective.vec <- seq(from=min(data$wbgi_gee, na.rm=T), to=max(data$wbgi_gee, na.rm=T), length.out=50)

# predictions
predict.effective <- predict(model1, newdata=data.frame(p_polity2=mean(data$p_polity2, na.rm=T), wdi_expedu=mean(data$wdi_expedu, na.rm=T), wbgi_gee=effective.vec))

# plot predicted values
plot(effective.vec, predict.effective, type="l", ylim=c(-10,22), xlab="Government Effectiveness", ylab="Predicted Literacy Gap Men - Women", lwd=3)


# now plot with confidence interval
predict.effective.ci <- predict(model1, newdata=data.frame(p_polity2=mean(data$p_polity2, na.rm=T), wdi_expedu=mean(data$wdi_expedu, na.rm=T), wbgi_gee=effective.vec), interval="confidence", level = 0.95)

plot(effective.vec, predict.effective.ci[,1], type="l", ylim=c(-16,28), xlab="Government Effectiveness", ylab="Predicted Literacy Gap Men - Women", lwd=3)
# lower boundary of CI
points(effective.vec, predict.effective.ci[,2], type="l", lty=2, lwd=3)
# upper boundary of CI
points(effective.vec, predict.effective.ci[,3], type="l", lty=2, lwd=3)


# try it for p_polity2
summary(data$p_polity2)
