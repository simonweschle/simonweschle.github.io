
### Class Code from Feb 17


### Exercise from last time 

rm(list=ls(all=TRUE))

turnout <- read.csv("/Users/simonweschle/Dropbox/Teaching/2021_PSC_400/classes/week_1/data/turnout.csv")


turnout$turnout <- (turnout$total/turnout$VEP) * 100

turnout$turnout.diff <- turnout$ANES - turnout$turnout
mean(turnout$turnout.diff)
range(turnout$turnout.diff)


plot(turnout$year, turnout$turnout.diff, type="l", ylim=c(0, max(turnout$turnout.diff)))



# RESUME EXPERIMENT
rm(list=ls(all=TRUE))

# load data from a csv file
resume <- read.csv("/Users/simonweschle/Dropbox/Teaching/2021_PSC_400/classes/week_2/data/resume.csv")

# table of the results
table(race=resume$race, call=resume$call)


# can compute callback rate from table, but better/easier to do this directly from the data

# overall callback rate
mean(resume$call)

# callback rates separately
mean(resume$call[resume$race=="black"]) # == is equal to
mean(resume$call[resume$race=="white"])


# alternatively, create 2 dataframes, separated by race
resume.black <- resume[resume$race=="black",] 
dim(resume.black)

resume.white <- resume[resume$race=="white",]
dim(resume.white)

mean(resume.black$call)
mean(resume.white$call)



# how do we get callback rate for men and women?
mean(resume$call[resume$sex=="male"])

mean(resume$call[resume$sex=="female"])
mean(resume$call[resume$sex!="male"]) # != is not equal to



# how do we get callback rate for black/white man/women?
mean(resume$call[resume$race=="black" & resume$sex=="male"])

mean(resume$call[resume$race=="black" & resume$sex=="female"])

mean(resume$call[resume$race=="white" & resume$sex=="male"])

mean(resume$call[resume$race=="white" & resume$sex=="female"])



# create a new dummy variable: 1 if black and female
resume$black.women <- ifelse(resume$race=="black" & resume$sex=="female", 1, 0)
table(resume$black.women)
mean(resume$call[resume$black.women==1])



# create a new dummy variable: 1 if black *or* female
resume$black.or.women <- ifelse(resume$race=="black" | resume$sex=="female", 1, 0)
table(resume$black.or.women)
mean(resume$call[resume$black.or.women==1])




