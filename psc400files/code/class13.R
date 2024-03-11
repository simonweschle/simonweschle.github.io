### PSC 400, Spring 2024
### Week 7, Thursday 3/7




### UKRAINE AGGREGATE
rm(list=ls(all=TRUE))

data <- read.csv("~/Dropbox/Teaching/2022_1_PSC_400/classes/week_6/data/UA_precincts.csv")

dim(data)

head(data)


# create variable that is change in voting behavior
data$pro_russian_change <- data$pro_russian - data$prior_pro_russian

hist(data$pro_russian_change)


## difference in means 
mean(data$pro_russian_change)

mean(data$pro_russian_change[data$russian_tv==1])
mean(data$pro_russian_change[data$russian_tv==0])

mean(data$pro_russian_change[data$russian_tv==1]) - mean(data$pro_russian_change[data$russian_tv==0])


## using a regression with one independent variable
model1 <- lm(pro_russian_change ~ russian_tv, data=data)
model1



## living within 25km of border as a potential confounder
model2 <- lm(pro_russian_change ~ russian_tv + within_25km, data=data)
model2


## R2
summary(model1)$r.squared

summary(model2)$r.squared




## IMMIGRATION ATTITUDES

rm(list=ls(all=TRUE))

immidata <- read.csv("~/Dropbox/Teaching/2024_1_PSC_400/classes/week_7/data/immig.csv")

# h1bvis.supp: 0=decrease a great deal, 1=increase a great deal (larger values=more suport for immigration)
# impl.prejud: 0=low implicit prejudice, 1=high implicit prejudice

model1 <- lm(h1bvis.supp ~ impl.prejud, data=immidata)
model1


model2 <- lm(h1bvis.supp ~ impl.prejud + female, data=immidata)
model2


# plotting the predicted value for different values of prejudice

# for simpolicity, I'm creating the vector first
prejud.vec <- c(0, 0.25, 0.5, 0.75, 1)
predict.prejud.female <- predict(model2, newdata=data.frame(impl.prejud=prejud.vec, female=1))

plot(prejud.vec, predict.prejud.female, type="l", ylim=c(0,1), xlab="Implicit Prejudige", ylab="Predicted Immigration Policy Support", lwd=3)

predict.prejud.male <- predict(model2, newdata=data.frame(impl.prejud=prejud.vec, female=0))

points(prejud.vec, predict.prejud.male, type="l", lty="dashed", lwd=3)
text(0.5, 0.45, "Male")
text(0.5, 0.28, "Female")




model3 <- lm(h1bvis.supp ~ impl.prejud + female + employed + age, data=immidata)
model3




## QOG
rm(list=ls(all=TRUE))

data <- read.csv("~/Dropbox/Teaching/2024_1_PSC_400/classes/week_4/data/qog_bas_cs_jan24.csv")


data$wdi_litrad_diff <- data$wdi_litradm - data$wdi_litradf
hist(data$wdi_litrad_diff, breaks=30)


mod1 <- lm(wdi_litrad_diff ~ p_polity2, data=data)
mod1


mod2 <- lm(wdi_litrad_diff ~ p_polity2 + wdi_expedu + wbgi_gee, data=data)
mod2



polity.vec <- seq(-10, 10, 1)
predict.diff <- predict(mod2, newdata=data.frame(p_polity2=polity.vec, wdi_expedu=mean(data$wdi_expedu, na.rm=T), wbgi_gee=mean(data$wbgi_gee, na.rm=T)))

plot(polity.vec, predict.diff, type="l", ylim=c(0, max(predict.diff)), xlab="Polity Score", ylab="Predicted Literacy Rate Difference (Men - Women)", lwd=3)




