### PSC 400, Fall 2022
### Week 1, Thursday 1/25

### set up code file


### Analyzing a new data set: TURNOUT.CSV


# download dataset from webpage and put it in your PSC400 folder


# we clear everything in the workspace since this is new project
rm(list=ls(all=TRUE))

# set the working directory to your PSC400 folder
setwd("~/Dropbox/Teaching/2024_1_PSC_400/classes/week_1/data")

# load the data
data <- read.csv("turnout.csv")

# view the dataset
View(data)

# see the first few rows (this is a function)
head(data)

# see the last few rows
tail(data)

# this is a dataframe
# rows=observation: information collected from a particular individual or entity in study (here: elections)
# column=variables: values of a certain characteristic for multiple individuals or entiries in study
  # how do I know what the different variables are? Codebook (slide)


# this gives you the names of the variables
names(data)

# identify number of observations in dataset
dim(data)



# accessing a variable within a dataframe
data$total
head(data$total)


# what is the variable type? numeric, non-binary

# with numeric variables, we can summarize them in several ways

# what is mean/average total?
mean(data$total)

# what is median total
median(data$total)

# what is the lowest total?
min(data$total)

# what is the total?
max(data$total)

# or both together
range(data$total)


# more info all at once
summary(data$total)

# table (not that useful for numerical variables)
summary(data$year)


## learned: dim, nrow, ncol, head, mean, median, min, max, range, summary



### CREATING NEW VARIABLES


# summarize reported turnout
data$ANES
summary(data$ANES)

# create variable with actual turnout
data$turnout <- (data$total/data$VAP)*100


# difference between reported and actual turnout
data$turnout.diff <- data$ANES - data$turnout


summary(data$turnout.diff)






