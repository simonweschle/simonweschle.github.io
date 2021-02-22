
### Class Code from Feb 22


### Continuing Reume Experiment Analysis

rm(list=ls(all=TRUE))

# load data from a csv file
resume <- read.csv("/Users/simonweschle/Dropbox/Teaching/2021_PSC_400/classes/week_2/data/resume.csv")


# goal: callback rates for combination of race and sex
resume$black.women <- ifelse(resume$race=="black" & resume$sex=="female", 1, 0)
resume$black.men <- ifelse(resume$race=="black" & resume$sex=="male", 1, 0)
resume$white.women <- ifelse(resume$race=="white" & resume$sex=="female", 1, 0)
resume$white.men <- ifelse(resume$race=="white" & resume$sex=="male", 1, 0)

mean(resume$call[resume$black.women==1])
mean(resume$call[resume$black.men==1])
mean(resume$call[resume$white.women==1])
mean(resume$call[resume$white.men==1])



# create a factor variable (for categorical variables)
resume$type <- NA
resume$type[resume$race=="black" & resume$sex=="female"] <- "BlackFemale"
resume$type[resume$race=="black" & resume$sex=="male"] <- "BlackMale"
resume$type[resume$race=="white" & resume$sex=="female"] <- "WhiteFemale"
resume$type[resume$race=="white" & resume$sex=="male"] <- "WhiteMale"

# turn it into a factor variable
class(resume$type)
resume$type <- as.factor(resume$type)
class(resume$type)

table(resume$type)
prop.table(table(resume$type))

# use tapply to get mean for every type
  # tapply(object to which function is applied to, object that defines the groups, what tappy should compute)
results <- tapply(resume$call, resume$type, mean)
results

# plot them
barplot(results)
barplot(results, main="Callback Rates by Sex/Gender")




### often can't do RCT, e.g. minimum wage increase. but still important to study

### comparison of two states: NJ and PA, NJ increase min wage frmo 4.25 to 5.05, PA left it unchanged

rm(list=ls(all=TRUE))

minwage <- read.csv("/Users/simonweschle/Dropbox/Teaching/2021_PSC_400/classes/week_2/data/minwage.csv")


# what is the average wage in each location before?
tapply(minwage$wageBefore, minwage$location, mean)


# what is the average wage in PA and NJ before?
table(minwage$location)

summary(minwage$wageBefore[minwage$location=="PA"])
summary(minwage$wageBefore[minwage$location!="PA"])

# what is the average wage in PA and NJ after?
summary(minwage$wageAfter[minwage$location=="PA"])
summary(minwage$wageAfter[minwage$location!="PA"])


# what is the share of restaurants in NJ that did not pay 5.05 before implementation?
minwage$lessminwageBefore <- ifelse(minwage$wageBefore<=5.05, 1, 0)  ## talk about >= etc.
mean(minwage$lessminwageBefore[minwage$location!="PA"])


# AT HOME: what is proportion of full-time employees before the min wage increase in each state?

# AT HOME: what is proportion of full-time employees after the min wage increase in each state?


