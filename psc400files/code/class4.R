### PSC 400, Spring 2024
### Week 2, Wednesday 2/1





### EXPERIMENT ON RACIAL DISCRIMINATION

rm(list=ls(all=TRUE))

data <- read.csv("~/Dropbox/Teaching/2022_1_PSC_400/classes/week_2/data/resume.csv")

dim(data)

head(data)

summary(data)


mean(data$call)


# what is the difference-in-means in callback rate between black and white respondents?
data$white <- ifelse(data$race=="white", 1, 0)

mean(data$call[data$white==1])
mean(data$call[data$white==0])
mean(data$call[data$white==1]) - mean(data$call[data$white==0])

## equivalent
mean(data$call[data$race=="white"])
mean(data$call[data$race=="black"])
mean(data$call[data$race=="white"]) - mean(data$call[data$race=="black"])


# what is the difference-in-means in callback rate between *male* black and white respondents?

# first way: only keep data with male applicants
data.male <- subset(data, subset=(sex=="male"))

dim(data.male)

mean(data.male$call[data.male$race=="white"])
mean(data.male$call[data.male$race=="black"])
mean(data.male$call[data.male$race=="white"]) - mean(data.male$call[data.male$race=="black"])

# second way: use original data, but with two conditoins
mean(data$call[data$race=="white" & data$sex=="male"])
mean(data$call[data$race=="black" & data$sex=="male"])
mean(data$call[data$race=="white" & data$sex=="male"]) - mean(data$call[data$race=="black" & data$sex=="male"])




# what is the difference-in-means in callback rate between *female* black and white respondents?
mean(data$call[data$race=="white" & data$sex=="female"])
mean(data$call[data$race=="black" & data$sex=="female"])
mean(data$call[data$race=="white" & data$sex=="female"]) - mean(data$call[data$race=="black" & data$sex=="female"])




# gender difference
mean(data$call[data$sex=="female"]) - mean(data$call[data$sex=="male"])

mean(data$call[data$sex=="female" & data$race=="white"]) - mean(data$call[data$sex=="male" & data$race=="white"])
mean(data$call[data$sex=="female" & data$race=="black"]) - mean(data$call[data$sex=="male" & data$race=="black"])


