### PSC 400, Fall 2022
### Week 1, Wednesday 1/26


# clear everything in the workspace. put this at the beginning of ALL your code files
rm(list=ls(all=TRUE))


## R as a calculator
4 + 5

4 ^ 6


## creating an object
# name <- content (box analogy)

result <- 5 + 3 
result

# overwriting an object
result <- 5 - 3

# can use object in further calculations
result*5
result/2

# objects can be text
instructor <- "Prof. Weschle"

instructor <- Prof. Weschle

result2 <- "5 + 3"


## functions
# input -> function performs action with input -> output

sqrt(4)
sqrt(result)



### Analyzing a new data set: TURNOUT.CSV


# download dataset from webpage and put it in your PSC400 folder


# we clear everything in the workspace since this is new project
rm(list=ls(all=TRUE))

# set the working directory to your PSC400 folder
setwd("~/Dropbox/Teaching/2022_1_PSC_400/classes/week_1/data")


# load the data
data <- read.csv("turnout.csv")

# view the dataset

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