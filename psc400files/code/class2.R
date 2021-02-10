
### Class Code from Feb 10

# clear everything in the workspace. put this at the beginning of ALL your code files
rm(list=ls(all=TRUE))



## World Population as a vector
world.pop <- c(2525779, 3026003, 3691173, 4449049, 5320817, 6127700, 6916183)

## population as function of 2010 population
pop.rate <- world.pop / world.pop[7]

# various functions
length(world.pop)
min(world.pop)
max(world.pop)
range(world.pop)
median(world.pop)
mean(world.pop)
sum(world.pop)/length(world.pop)

# create a vector for the years (3 equivalent years)
years1 <- c(1950, 1960, 1970, 1980, 1990, 2000, 2010)
years2 <- seq(from=1950, to=2010, by=10)
years3 <- seq(1950, 2010, 10)


# assign the years as names to the population vector
names(world.pop)

names(world.pop) <- years1

world.pop

# replace a value
world.pop[1] <- 2525780
world.pop



# clear everything in the workspace again
rm(list=ls(all=TRUE))

# load data from a csv file
data <- read.csv("/Users/simonweschle/Dropbox/Teaching/2021_PSC_400/classes/week_1/UNpop.csv")

