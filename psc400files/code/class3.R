
### Class Code from Feb 15


# clear everything in the workspace
rm(list=ls(all=TRUE))

# load data from a csv file
UNpop <- read.csv("/Users/simonweschle/Dropbox/Teaching/2021_PSC_400/classes/week_1/data/UNpop.csv")
	

# looking at the data set
head(UNpop)

tail(UNpop)



# characteristics of data set
dim(UNpop)

nrow(UNpop)

ncol(UNpop)

names(UNpop)

summary(UNpop)





# accessing the variables in the data
UNpop$year # using the variable name
UNpop[,1] # first column
UNpop[,"year"]

UNpop$world.pop
UNpop[,2]
UNpop[,"world.pop"]

# access specific observations (=rows)
UNpop[1,]

# access a specific observation, but only one variable
UNpop[1,2]
UNpop[1,"world.pop"]
UNpop$world.pop[1]
# note: when you are accessing an observation in a data frame, you need data[row,column]
# when you are accessing an observation in a vector, you need vector[position in vector]





# plot pop over time
plot(x=UNpop$year, y=UNpop$world.pop)

plot(UNpop$year, UNpop$world.pop)

plot(UNpop$year, UNpop$world.pop, type="l")


# create a new variable: pop relative to 1950
UNpop$world.pop.rate <- UNpop$world.pop/UNpop$world.pop[1]

# save data. Never ever change anything in your original data
write.csv(UNpop, file="/Users/simonweschle/Dropbox/Teaching/2021_PSC_400/classes/week_1/data/UNpop_new.csv")

save(UNpop, file="/Users/simonweschle/Dropbox/Teaching/2021_PSC_400/classes/week_1/data/UNpop_new.RData")




### EXERCISE 

rm(list=ls(all=TRUE))

turnout <- read.csv("/Users/simonweschle/Dropbox/Teaching/2021_PSC_400/classes/week_1/data/turnout.csv")

dim(turnout)

summary(turnout)

range(turnout$year)



