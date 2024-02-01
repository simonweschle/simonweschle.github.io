### PSC 400, Spring 2024
### Week 2, Tuesday 1/30



### EFFECT OF CLASS SIZE ON OUTCOMES

rm(list=ls(all=TRUE))

star <- read.csv("~/Dropbox/Teaching/2024_1_PSC_400/classes/week_1/data/STAR.csv")


## check out the data
head(star)

summary(star$reading)
summary(star$math)
summary(star$graduated)

summary(star$classtype) # doesn't work: character variable
table(star$classtype)
table(star$graduated)




# create a new variable that is 1 if class type is small, and 0 otherwise
# iuse felse(condition, value if condition is TRUE, value if condition is FALSE)
star$small <- ifelse(star$classtype=="small", 1, 0)

table(star$small)

mean(star$small)


## Causal effect of small class size

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


# difference in means graduation (outcome is binary, so interpret as percentage)
mean(star$graduated[star$small==1]) - mean(star$graduated[star$small==0])




