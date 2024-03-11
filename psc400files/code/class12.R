### PSC 400, Spring 2024
### Week 7, Tuesday 3/5



### UKRAINE
rm(list=ls(all=TRUE))

data <- read.csv("~/Dropbox/Teaching/2024_1_PSC_400/classes/week_6/data/UA_survey.csv")

dim(data)

head(data)


## using a regression with one independent variable
model1 <- lm(pro_russian_vote ~ russian_tv, data=data)
model1

summary(model1)$r.squared


## living within 25km of border as a potential confounder

table(data$within_25km)

model2 <- lm(pro_russian_vote ~ russian_tv + within_25km, data=data)
model2

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


# predicted value for impl.prejud=0, female=1?
0.50897 - 0.20960*0 - 0.06858*1
predict(model2, newdata=data.frame(impl.prejud=0, female=1))

# predicted values
predict(model2, newdata=data.frame(impl.prejud=0.5, female=1))
predict(model2, newdata=data.frame(impl.prejud=1, female=1))
predict(model2, newdata=data.frame(impl.prejud=1, female=0))



model3 <- lm(h1bvis.supp ~ impl.prejud + female + employed + age, data=immidata)
model3




