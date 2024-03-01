### PSC 400, Spring 2022
### Week 6, Monday 2/28


### GDP and NIGHT TIME EMISSIONS
rm(list=ls(all=TRUE))

countries <- read.csv("~/Dropbox/Teaching/2024_1_PSC_400/classes/week_6/data/countries.csv")


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




### QUALITY OF GOVERNMENT
rm(list=ls(all=TRUE))


data <- read.csv("~/Dropbox/Teaching/2024_1_PSC_400/classes/week_4/data/qog_bas_cs_jan24.csv")


plot(data$p_polity2, data$ti_cpi, pch=16, xlab="Polity Score", ylab="Corruption Perceptions Index")

m1 <- lm(ti_cpi ~ p_polity2, data=data)
m1

abline(m1, col="red", lwd=1.5)

summary(m1)$r.squared



### UKRAINE
rm(list=ls(all=TRUE))

data <- read.csv("~/Dropbox/Teaching/2024_1_PSC_400/classes/week_6/data/UA_survey.csv")

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





