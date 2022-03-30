### PSC 400, Spring 2022
### Week 9, Monday 3/28


# social pressure experiment
rm(list=ls(all=TRUE))

socialdata <- read.csv("~/Dropbox/Teaching/2022_1_PSC_400/classes/week_8/data/social.csv")


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


