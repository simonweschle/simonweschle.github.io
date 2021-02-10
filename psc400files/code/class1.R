
### Class Code from Feb 8


# some basic math operations
4 + 5

sqrt(4)

4 ^ 6


# creating an object
result <- 5 + 3 
result

# overwriting an object
result <- 5 - 3

# objects can be text
instructor <- "Prof. Weschle"

result2 <- "5 + 3"



## World Population as a vector
world.pop <- c(2525779, 3026003, 3691173, 4449049, 5320817, 6127700, 6916183)

# accessing elements in a vector
world.pop[1]
world.pop[6]
world.pop[c(2,5)]
world.pop[c(5, 2)]
world.pop[-1]


# create a new object that gives world population in million
pop.million <- world.pop / 1000


# functions: min, max, mean
min(pop.million)
max(pop.million)
mean(pop.million)


