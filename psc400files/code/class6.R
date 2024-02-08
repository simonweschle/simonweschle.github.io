### PSC 400, Spring 2024
### Week 3, Thursday 2/8


## analyzing Brexit
rm(list=ls(all=TRUE))

setwd("~/Dropbox/Teaching/2024_1_PSC_400/")

bes <- read.csv("classes/week_3/data/BES.csv")


table(bes$education)
table(bes$education, exclude=NULL)

prop.table(table(bes$education))


table(bes$leave)
table(bes$leave, exclude=NULL)

prop.table(table(bes$leave))

mean(bes$leave)
mean(bes$leave, na.rm=TRUE)


# delete all observations that have *any* missing observations (in *any* variable in the dataset--you may only be interested in some of the variables)
bes1 <- na.omit(bes)
dim(bes1)
dim(bes)



## histogram of age
hist(bes1$age)

hist(bes1$age, xlab="Age", main="Age Distribution of Respondents")



# barplot of vote intention (not in book)
hist(bes1$leave)  # not good

leavetab <- prop.table(table(bes$leave))
barplot(leavetab) # better

barplot(leavetab, ylab="Share of Respondents", xlab="Vote Intention")

barplot(leavetab, ylab="Share of Respondents", xlab="Vote Intention", names.arg=c("Remain", "Leave"))











### now we want to look at the relation between two variables

## vote intention by education

## two-way frequency tables
table(bes1$leave, bes1$education)

## two-way table of proportions
prop.table(table(bes1$leave, bes1$education))
	# sum of all proportions is 1

prop.table(table(bes1$leave, bes1$education), margin=1)
	# proportions in each row sum to 1

prop.table(table(bes1$leave, bes1$education), margin=2)
	# proportions in each column sum to 1


## vote intention by age

# descriptive statistics: supporters vs. opponents
mean(bes1$age[bes1$leave==0])
mean(bes1$age[bes1$leave==1])

median(bes1$age[bes1$leave==0])
median(bes1$age[bes1$leave==1])

sd(bes1$age[bes1$leave==0])
sd(bes1$age[bes1$leave==1])


# boxplot of age by vote intention (not in book)
boxplot(bes1$age[bes1$leave==0], bes1$age[bes1$leave==1])
boxplot(age ~ leave, data=bes1)

boxplot(age ~ leave, data=bes1, ylab="Age", xlab="Vote Intention", names=c("Remain", "Leave"))



## histogram of age by vote intention
hist(bes1$age[bes1$leave==0], xlab="Age", main="Age Distribution of Brexit Opponents")
hist(bes1$age[bes1$leave==1], xlab="Age", main="Age Distribution of Brexit Supporters")

# put the y-axes on same scale
hist(bes1$age[bes1$leave==0], xlab="Age", main="Age Distribution of Brexit Opponents", ylim=c(0, 2000))
hist(bes1$age[bes1$leave==1], xlab="Age", main="Age Distribution of Brexit Supporters", ylim=c(0, 2000))

# create densities of age by support/oppose Brexit
hist(bes1$age[bes1$leave==0], xlab="Age", main="Age Distribution of Brexit Opponents", ylim=c(0, 0.04), freq=FALSE)
hist(bes1$age[bes1$leave==1], xlab="Age", main="Age Distribution of Brexit Supporters", ylim=c(0, 0.04), freq=FALSE)














## Relation between Brexit and Education
rm(list=ls(all=TRUE))

dis <- read.csv("~/Dropbox/Teaching/2022_1_PSC_400/classes/week_3/data/UK_districts.csv")

head(dis)

dim(dis)

dis1 <- na.omit(dis)
dim(dis1)


plot(dis1$high_education, dis1$leave)

plot(dis1$high_education, dis1$leave, main="Education Levels and Support for Brexit", xlab="Share of District Residents with Higher Education Degree", ylab="Share of Leave Votes")

plot(dis1$high_education, dis1$leave, main="Education Levels and Support for Brexit", xlab="Share of District Residents with Higher Education Degree", ylab="Share of Leave Votes", pch=16, col="gray")

# add lines of means
abline(h=mean(dis1$leave), lty="dashed", col="red", lwd=2)
abline(v=mean(dis1$high_education), lty="dashed", col="red", lwd=2)


# correlation
cor(dis1$high_education, dis1$leave)





