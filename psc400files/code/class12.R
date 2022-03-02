### PSC 400, Spring 2022
### Week 6, Wednesday 3/2




### UKRAINE
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

immidata <- read.csv("~/Dropbox/Teaching/2022_1_PSC_400/classes/week_6/data/immig.csv")

# h1bvis.supp: 0=decrease a great deal, 1=increase a great deal (larger values=more suport for immigration)
# impl.prejud: 0=low implicit prejudice, 1=high implicit prejudice

model1 <- lm(h1bvis.supp ~ impl.prejud, data=immidata)
model1


model2 <- lm(h1bvis.supp ~ impl.prejud + female, data=immidata)
model2


# predicted value for impl.prejud=0, female=1?
0.50897 - 0.20960*0 - 0.06858*1
predict(model2, newdata=data.frame(impl.prejud=0, female=1))

# predicted values
predict(model2, newdata=data.frame(impl.prejud=0.5, female=1))
predict(model2, newdata=data.frame(impl.prejud=1, female=1))
predict(model2, newdata=data.frame(impl.prejud=1, female=0))


# plotting the predicted value for different values of prejudice

# for simpolicity, I'm creating the vector first
prejud.vec <- c(0, 0.25, 0.5, 0.75, 1)
predict.prejud.female <- predict(model2, newdata=data.frame(impl.prejud=prejud.vec, female=1))

plot(prejud.vec, predict.prejud.female, type="l", ylim=c(0,1), xlab="Implicit Prejudige", ylab="Predicted Immigration Policy Support", lwd=3)

predict.prejud.male <- predict(model2, newdata=data.frame(impl.prejud=prejud.vec, female=0))

points(prejud.vec, predict.prejud.male, type="l", lty="dashed", lwd=3)
text(0.5, 0.45, "Male")
text(0.5, 0.28, "Female")

