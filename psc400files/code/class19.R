### PSC 400, Spring 2022
### Week 10, Monday 4/4



### PRACTICE PROBLEMS SOLUTION

rm(list=ls(all=TRUE))

data <- read.csv("~/Dropbox/Teaching/2022_1_PSC_400/classes/week_9/data/mothers.csv")


# use only 2009 data
data <- data[data$year==2009,]


# summarize numChildren
summary(data$numChildren)

prop.table(table(data$numChildren))

# create binary variable: any children
data$children_bin <- ifelse(data$numChildren > 0, 1, 0)


# graphically compare wage of women with and without children
boxplot(wage ~ children_bin, data=data)


# create variable: log wage
data$logwage <- log(data$wage)

# graphically compare logwage of women with and without children
boxplot(logwage ~ children_bin, data=data)


# what is logwage difference between women with and without children? Incl. 95% CI
model1 <- lm(logwage ~ children_bin, data=data)
summary(model1)
confint(model1)

# how does logwage depend on number of children (incl 95%CI)
model2 <- lm(logwage ~ numChildren, data=data)
summary(model2)
confint(model2)


# now add controls you think are important (that may infleunce both logwage and having number of children)
model3 <- lm(logwage ~ numChildren + age + urban + marstat + educ + school + experience + tenure + fullTime + unionized, data=data)
summary(model3)
confint(model3)
	# careful if you include e.g. region: use as.factor(region) or as.character(region)


# plot predicted log wage by number of children
summary(data$numChildren)

childvec <- seq(1,6,1)
pred.wage <- predict(model3, newdata=data.frame(numChildren=childvec, age=mean(data$age, na.rm=T), urban=1, marstat="No romantic union", educ="2.High school", school=FALSE, experience=mean(data$experience, na.rm=T), tenure=mean(data$tenure, na.rm=T), fullTime=TRUE, unionized=0), interval="confidence", level=0.95)

plot(childvec, pred.wage[,1], ylim=c(6,8), type="l", lwd=3)
points(childvec, pred.wage[,2], type="l", lwd=3, lty=2)
points(childvec, pred.wage[,3], type="l", lwd=3, lty=2)



# effect of having any children
model4 <- lm(logwage ~ children_bin + age + urban + marstat + educ + school + experience + tenure + fullTime + unionized, data=data)
summary(model4)
confint(model4)

childvec <- c(0, 1)
pred.wage <- predict(model4, newdata=data.frame(children_bin=childvec, age=mean(data$age, na.rm=T), urban=1, marstat="No romantic union", educ="2.High school", school=FALSE, experience=mean(data$experience, na.rm=T), tenure=mean(data$tenure, na.rm=T), fullTime=TRUE, unionized=0), interval="confidence", level=0.95)

# interaction: Is the effect of having any children on log wages different for women in a unionized versus a non-unionized job?
model5 <- lm(logwage ~ children_bin + children_bin:unionized + age + urban + marstat + educ + school + experience + tenure + fullTime + unionized, data=data)
summary(model5)






### SPATIAL DATA

rm(list=ls(all=TRUE))

library(maps)

data(us.cities)
head(us.cities)


# plot capitals
map(database="usa")
points(x=us.cities$long, y=us.cities$lat, pch=16, cex=0.5)


capitals <- subset(us.cities, capital==2)
map(database="usa")
points(x=capitals$long, y=capitals$lat, col="blue", pch=16)


map(database="state")
points(x=capitals$long, y=capitals$lat, col="blue", pch=16)

map(database="state")
points(x=capitals$long, y=capitals$lat, col="blue", pch=16, cex=capitals$pop/500000)



# plot cities in NY
ny.cities <- subset(us.cities, country.etc=="NY")
pop.ord <- order(ny.cities$pop, decreasing=TRUE)
top5 <- pop.ord[1:5]


map(database="state", regions="New York", col="gray")
points(x=ny.cities$long[top5], y=ny.cities$lat[top5], pch=16)
text(x = ny.cities$long[top5], y = ny.cities$lat[top5], label = ny.cities$name[top5], pos=4)





