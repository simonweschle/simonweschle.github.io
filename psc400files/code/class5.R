### PSC 400, Spring 2024
### Week 3, Tuesday 2/6




### Continuation frmo last week. Often can't do RCT, e.g. minimum wage increase. but still important to study

### comparison of two states: NJ and PA, NJ increase min wage frmo 4.25 to 5.05, PA left it unchanged

rm(list=ls(all=TRUE))

setwd("~/Dropbox/Teaching/2024_1_PSC_400/")

minwage <- read.csv("classes/week_2/data/minwage.csv")


# what is the average wage in PA and NJ before?
table(minwage$location)

summary(minwage$wageBefore[minwage$location=="PA"])
summary(minwage$wageBefore[minwage$location!="PA"])
# equivalent:
summary(minwage$wageBefore[minwage$location=="centralNJ" | minwage$location=="northNJ" | minwage$location=="shoreNJ" | minwage$location=="southNJ"])



# what is the average wage in PA and NJ after?
summary(minwage$wageAfter[minwage$location=="PA"])
summary(minwage$wageAfter[minwage$location!="PA"])


# what is proportion of full-time employees before the min wage increase in each state?
minwage$propBefore <- minwage$fullBefore/(minwage$fullBefore + minwage$partBefore)

mean(minwage$propBefore[minwage$location=="PA"])
mean(minwage$propBefore[minwage$location!="PA"])

# what is proportion of full-time employees after the min wage increase in each state?
minwage$propAfter <- minwage$fullAfter/(minwage$fullAfter + minwage$partAfter)

mean(minwage$propAfter[minwage$location=="PA"])
mean(minwage$propAfter[minwage$location!="PA"])









### Accessung data frames, vectors, data points

rm(list=ls(all=TRUE))

setwd("~/Dropbox/Teaching/2024_1_PSC_400/")

data <- read.csv("classes/week_1/data/turnout.csv")


# whole data frame
data

# one variable = one vector = one column
data$year
data$ANES

data[,1]
data[,5]

# one observation = one row
data[1,]

# one data point
data$year[1]
data[1,1]
data[1, "year"]

data$ANES[3]
data[3,5]
data[3, "ANES"]



# creating data points, vectors, data frames
vector1 <- c(5, 3, 4, 9)
vector2 <- c("A", "B", "C", "D")

newdata <- data.frame(vector1, vector2)
colnames(newdata) <- c("numbers", "letters")

newdata













## analyzing Brexit
rm(list=ls(all=TRUE))

setwd("~/Dropbox/Teaching/2024_1_PSC_400/")

bes <- read.csv("classes/week_3/data/BES.csv")

head(bes)

dim(bes)

table(bes$vote)

freq_table <- table(bes$vote)
freq_table

prop.table(freq_table)

prop.table(table(bes$vote))


