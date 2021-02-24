
# clear workspace
rm(list=ls(all=TRUE))


# LOADING/SAVING DATA

# load csv
data <- read.csv("your/path/to/data.csv")

# load RData
load("your/path/to/data.RData")


# save data to csv and RData
write.csv(data, file="your/path/to/data.csv")
save(data, file="your/path/to/data.RData")




# ACCESSING SUBSETS OF VARIABLES

# access all observations in vector data$v1 where data$v2 (factor variable) takes the value "blub"
data$v1[data$v2=="blub"]

# access all observations in vector data$v1 where data$v2 (factor variable) *does not* take the value "blub"
data$v1[data$v2!="blub"]

# access all observations in vector data$v1 where data$v3 (integer variable) is equal to or larger than 5
data$v1[data$v3>=5]
	# 	>		greater than
	# 	>=		greater or equal than
	# 	<		smaller than
	# 	<=		smaller or equal than


# get mean of all observations in vector data$v1 where data$v2 (factor variable) takes the value "blub"
mean(data$v1[data$v2=="blub"])
	# can use !=, >= etc. as above




# ACCESSING SUBSETS OF THE WHOLE DATA FRAME
# create newdata with all observations where data$v2 (factor variable) takes the value "blub"
newdata <- data[data$v2=="blub",]
	# can use !=, >= etc. as above






# CREATE NEW VARIABLES

# create data$v4, which is data$v2 times data$v3 (both integer variables)
data$v4 <- data$v2 * data$v3
	# can also use + - * / ^

# create data$v4, which is 1 if v2 takes the value "blub" and 0 otherwise
data$v4 <- ifelse(data$v2=="blub", 1, 0)

# create data$v4, which is 1 if v2 takes the value "blub" and v3 the value "blob", and 0 otherwise
data$v4 <- ifelse(data$v2=="blub" & data$v3=="blob", 1, 0)

# create data$v4, which is 1 if v2 takes the value "blub" or v3 the value "blob", and 0 otherwise
data$v4 <- ifelse(data$v2=="blub" | data$v3=="blob", 1, 0)



# create a factor variable data$type that combines data$sex (male/female) and data$race (black/white)
data$type <- NA
data$type[data$race=="black" & data$sex=="female"] <- "BlackFemale"
data$type[data$race=="black" & data$sex=="male"] <- "BlackMale"
data$type[data$race=="white" & data$sex=="female"] <- "WhiteFemale"
data$type[data$race=="white" & data$sex=="male"] <- "WhiteMale"
data$type <- as.factor(data$type)




# GET MEAN etc. I'm only providing the more complicated ways to do this, not mean(data$v1) etc,

# get mean of data$v1 (integer variable) separately for each category in data$v2 (a factor variable, e.g. male/female)
tapply(data$v1, data$v2, mean)




