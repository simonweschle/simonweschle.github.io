### PSC 400, Spring 2024
### Week 8, Thursday 3/21


## CONFIDENCE INTERVALS UKRAINE
rm(list=ls(all=TRUE))

data <- read.csv("~/Dropbox/Teaching/2022_1_PSC_400/classes/week_6/data/UA_survey.csv")

mean(data$pro_russian_vote[data$russian_tv==1]) - mean(data$pro_russian_vote[data$russian_tv==0])


t.test(pro_russian_vote ~ russian_tv, data=data)
t.test(pro_russian_vote ~ russian_tv, data=data)$estimate
t.test(pro_russian_vote ~ russian_tv, data=data)$conf.int




## HYPOTHESIS TESTING, DIFFERENCE IN MEANS

rm(list=ls(all=TRUE))

data <- read.csv("~/Dropbox/Teaching/2022_1_PSC_400/classes/week_6/data/UA_survey.csv")

mean(data$pro_russian_vote[data$russian_tv==1]) - mean(data$pro_russian_vote[data$russian_tv==0])

t.test(pro_russian_vote ~ russian_tv, data=data)$p.value

# significant at 5% level = 95% CI does not include zero


rm(list=ls(all=TRUE))

star <- read.csv("~/Dropbox/Teaching/2022_1_PSC_400/classes/week_2/data/STAR.csv")

# create treatment variable
star$small <- ifelse(star$classtype=="small", 1, 0)

t.test(reading ~ small, data=star)



## HYPOTHESIS TESTING, REGRESSION COEFFICIENTS

rm(list=ls(all=TRUE))

data <- read.csv("~/Dropbox/Teaching/2022_1_PSC_400/classes/week_6/data/UA_precincts.csv")

data$pro_russian_change <- data$pro_russian - data$prior_pro_russian


model1 <- lm(pro_russian_change ~ russian_tv, data=data)
model1
summary(model1)


model2 <- lm(pro_russian_change ~ russian_tv + within_25km, data=data)
model2
summary(model2)



