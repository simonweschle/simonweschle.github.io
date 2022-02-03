### PSC 400, Fall 2022
### Week 2, Wednesday 2/2



### EFFECT OF CLASS SIZE ON OUTCOMES

rm(list=ls(all=TRUE))

star <- read.csv("~/Dropbox/Teaching/2022_1_PSC_400/classes/week_2/data/STAR.csv")


# create a new variable that is 1 if class type is small, and 0 otherwise
star$classtype

star$classtype=="small"


# ifelse(condition, value if condition is TRUE, value if condition is FALSE)
star$small <- ifelse(star$classtype=="small", 1, 0)

table(star$small)

mean(star$small)


# average reading scores overall
mean(star$reading)

# average reading scores for treatment group
mean(star$reading[star$small==1])


# average reading scores for control group
mean(star$reading[star$small==0])

# differnece in means estimator
mean(star$reading[star$small==1]) - mean(star$reading[star$small==0])

# difference in means math
mean(star$math[star$small==1]) - mean(star$math[star$small==0])


# difference in means graduation (outcome is binary, so interpret as percentage )
mean(star$graduated[star$small==1]) - mean(star$graduated[star$small==0])



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



# helpful command

# get the mean callback rate of applicants that are black *OR* female
mean(data$call[data$race=="white" | data$sex=="female"])


