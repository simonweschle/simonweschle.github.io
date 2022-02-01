### PSC 400, Fall 2022
### Week 2, Monday 1/31



### ANALYZING STAR.CSV

rm(list=ls(all=TRUE))

setwd("~/Dropbox/Teaching/2022_1_PSC_400/classes/week_2/data")

data <- read.csv("STAR.csv")

dim(data)

nrow(data)

ncol(data)

head(data)


# accessing a variable within a dataframe
data$reading
head(data$reading)


# what is the variable type? numeric, non-binary

# with numeric variables, we can summarize them in several ways

# what is mean/average reading score?
mean(data$reading)

# what is median rreading score
median(data$reading)

# what is the lowest reported reading score?
min(data$reading)

# what is the highest reported reading score?
max(data$reading)

# or both together
range(data$reading)


# more info all at once
summary(data$reading)



# classtype: character
head(data$classtype)

table(data$classtype)




# graduated: numeric, binary
head(data$graduated)

table(data$graduated)

summary(data$graduated)



## learned: dim, nrow, ncol, head, mean, median, min, max, range, summary, table


### CREATING NEW VARIABLES

rm(list=ls(all=TRUE))

setwd("~/Dropbox/Teaching/2022_1_PSC_400/classes/week_1/data")

data <- read.csv("turnout.csv")


# summarize reported turnout
data$ANES
summary(data$ANES)

# create variable with actual turnout
data$turnout <- (data$total/data$VAP)*100


# difference between reported and actual turnout
data$turnout.diff <- data$ANES - data$turnout


summary(data$turnout.diff)
