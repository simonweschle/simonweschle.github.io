### PSC 400, Spring 2022
### Week 6, Monday 2/28



### QUALITY OF GOVERNMENT
rm(list=ls(all=TRUE))

# codebook: https://www.qogdata.pol.gu.se/data/codebook_bas_jan22.pdf

data <- read.csv("~/Dropbox/Teaching/2022_1_PSC_400/classes/week_4/data/qog_bas_cs_jan22.csv")


plot(data$p_polity2, data$ti_cpi, pch=16, xlab="Polity Score", ylab="Corruption Perceptions Index")

m1 <- lm(ti_cpi ~ p_polity2, data=data)
m1

abline(m1, col="red", lwd=1.5)

summary(m1)$r.squared



### UKRAINE
rm(list=ls(all=TRUE))

data <- read.csv("~/Dropbox/Teaching/2022_1_PSC_400/classes/week_6/data/UA_survey.csv")

dim(data)

head(data)


## difference in means of pro-russian vote between those taht watch Russian TV and those that don't
mean(data$pro_russian_vote)

mean(data$pro_russian_vote[data$russian_tv==1])
mean(data$pro_russian_vote[data$russian_tv==0])

mean(data$pro_russian_vote[data$russian_tv==1]) - mean(data$pro_russian_vote[data$russian_tv==0])


## using a regression with one independent variable
model1 <- lm(pro_russian_vote ~ russian_tv, data=data)
model1



## living within 25km of border as a potential confounder

table(data$within_25km)

# bivariate relation between Russian TV and closeness to border
mean(data$russian_tv[data$within_25km==1])
mean(data$russian_tv[data$within_25km==0])


# bivariate relation between pro-Russian vote and closeness to border
mean(data$pro_russian_vote[data$within_25km==1])
mean(data$pro_russian_vote[data$within_25km==0])


model2 <- lm(pro_russian_vote ~ russian_tv + within_25km, data=data)
model2


