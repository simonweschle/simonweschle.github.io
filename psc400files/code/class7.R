### PSC 400, Fall 2022
### Week 4, Monday 2/14


## Review Exercise 1 Recap

## Characters and strings

# First, save the text "social science" to the variable course
course <- "social science"

# Next, save the text "learning R" to the same variable course
course <- "learning R"

# Finally, print the value of course to the console. What do you think it will say?
course



## Copying and reassigning variables

# execute the line below to create the object result
result <- 8 - 2

# Assign the value of result to result2
result2 <- result

# Overwrite the value of result with 10 - 2
result <- 10-2

# print result
result


### Working with Data

# Load the data frame UNpop from the file UNpop.csv
UNpop <- read.csv("~/Dropbox/Teaching/2022_1_PSC_400/classes/week_2/data/UNpop.csv")

## The data provides the world population (in thousands) per year 

# Print the UNpop data frame
UNpop

# Print the variable names of UNpop
names(UNpop)

## Print the dimensions of UNpop
dim(UNpop)

# Print the number of observations (rows) in this data frame
nrow(UNpop)

# Print the number of variables (columns) in this data frame
ncol(UNpop)

# print out the world.pop variable using $
UNpop$world.pop

# calculate the mean world population over this time period
mean(UNpop$world.pop)

# print out the world.pop variable for the years 1980 and later (greater or equal to in R: >=; greater than: >)
UNpop$world.pop[UNpop$years>=1980]

# calculate the mean world population for the years 1980 and later
mean(UNpop$world.pop[UNpop$years>=1980])



## Relation between Brexit and Education
rm(list=ls(all=TRUE))

dis <- read.csv("~/Dropbox/Teaching/2022_1_PSC_400/classes/week_3/data/UK_districts.csv")

# correlation
cor(dis1$high_education, dis1$leave)






## Ideology in US Congress
rm(list=ls(all=TRUE))

congress <- read.csv("~/Dropbox/Teaching/2022_1_PSC_400/classes/week_3/data/congress.csv")


# create separate datasets for Dem and Rep. Two ways of doing that
dem <- congress[congress$party=="Democrat",]
rep <- subset(congress, subset=(party=="Republican"))


# plot first and second dimension for Dems and Reps
plot(x=dem$dwnom1, y=dem$dwnom2, xlim=c(-1.5, 1.5), ylim=c(-1.5, 1.5), pch=16, col="blue", cex=0.3, xlab="Economic liberalism/conservatism", ylab="Racial liberalism/conservatism")
points(x=rep$dwnom1, y=rep$dwnom2, pch=16, col="red", cex=0.3)

cor(dem$dwnom1, dem$dwnom2)
cor(rep$dwnom1, rep$dwnom2)



# focus on 80th (47-49) and 112th Congress (11-13), by party
dem80 <- congress[congress$party=="Democrat" & congress$congress==80,]
rep80 <- congress[congress$party=="Republican" & congress$congress==80,]


# plot relation between DW dimension 1 and 2 for R and D separately, for 80th and 112th Congress
plot(dem80$dwnom1, dem80$dwnom2, xlim=c(-1.5, 1.5), ylim=c(-1.5, 1.5), pch=16, col="blue", main="80th Congress", xlab="Economic liberalism/conservatism", ylab="Racial liberalism/conservatism")
points(rep80$dwnom1, rep80$dwnom2, col="red", pch=18)
text(x=-0.75, y=1, "Democrats", col="blue")
text(x=1, y=-1, "Republicans", col="red")

cor(dem80$dwnom1, dem80$dwnom2)
cor(rep80$dwnom1, rep80$dwnom2)


dem112 <- congress[congress$party=="Democrat" & congress$congress==112,]
rep112 <- congress[congress$party=="Republican" & congress$congress==112,]


plot(dem112$dwnom1, dem112$dwnom2, xlim=c(-1.5, 1.5), ylim=c(-1.5, 1.5), pch=16, col="blue", main="112th Congress", xlab="Economic liberalism/conservatism", ylab="Racial liberalism/conservatism")
points(rep112$dwnom1, rep112$dwnom2, col="red", pch=18)
text(-0.75, 1, "Democrats", col="blue")
text(1, -1, "Republicans", col="red")

cor(dem112$dwnom1, dem112$dwnom2)
cor(rep112$dwnom1, rep112$dwnom2)



