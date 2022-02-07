### PSC 400, Fall 2022
### Week 3, Monday 2/7



## accessung data frames, vectors, data points
rm(list=ls(all=TRUE))

data <- read.csv("~/Dropbox/Teaching/2022_1_PSC_400/classes/week_1/data/turnout.csv")


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

bes <- read.csv("~/Dropbox/Teaching/2022_1_PSC_400/classes/week_3/data/BES.csv")

head(bes)

dim(bes)

table(bes$vote)

freq_table <- table(bes$vote)
freq_table

prop.table(freq_table)

prop.table(table(bes$vote))


table(bes$education)
table(bes$education, exclude=NULL)

prop.table(table(bes$education))
prop.table(table(bes$education, exclude=NULL))


table(bes$leave)
table(bes$leave, exclude=NULL)

prop.table(table(bes$leave))
prop.table(table(bes$leave, exclude=NULL))

mean(bes$leave)
mean(bes$leave, na.rm=TRUE)


# delete all observations that have *any* missing observations (in *any* variable in the dataset--you may only be interested in some of the variables)
bes1 <- na.omit(bes)
dim(bes1)
dim(bes)




### now we want to look at the relation between two variables

## two-way frequency tables
table(bes1$leave, bes1$education)

## two-way table of proportions
prop.table(table(bes1$leave, bes1$education))
	# sum of all proportions is 1

prop.table(table(bes1$leave, bes1$education), margin=1)
	# proportions in each row sum to 1

prop.table(table(bes1$leave, bes1$education), margin=2)
	# proportions in each column sum to 1

