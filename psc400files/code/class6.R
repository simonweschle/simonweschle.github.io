
### Class Code from Feb 24


### comparison of two states: NJ and PA, NJ increase min wage frmo 4.25 to 5.05, PA left it unchanged

rm(list=ls(all=TRUE))

minwage <- read.csv("/Users/simonweschle/Dropbox/Teaching/2021_PSC_400/classes/week_2/data/minwage.csv")


# what is proportion of full-time employees before the min wage increase in each state?
minwage$propBefore <- minwage$fullBefore/(minwage$fullBefore + minwage$partBefore)

mean(minwage$propBefore[minwage$location=="PA"])
mean(minwage$propBefore[minwage$location!="PA"])

# what is proportion of full-time employees after the min wage increase in each state?
minwage$propAfter <- minwage$fullAfter/(minwage$fullAfter + minwage$partAfter)

mean(minwage$propAfter[minwage$location=="PA"])
mean(minwage$propAfter[minwage$location!="PA"])





### SUMMARIZING SINGLE VARIABLES

## factor variables

table(minwage$location)

tab.loc <- table(minwage$location)
pie(tab.loc)
# equivalent:
pie(table(minwage$location))



# better: 
barplot(tab.loc)
barplot(table(minwage$location)) # equivalent


tab.loc.prop <- prop.table(tab.loc)
tab.loc.prop

prop.table(table(minwage$location)) # equivalent



barplot(tab.loc.prop)

barplot(tab.loc.prop, main="Location of Restaurants", xlab="Location", ylab="Share")

barplot(tab.loc.prop, main="Location of Restaurants", xlab="Location", ylab="Share", col="red")




## numeric variables
quantile(minwage$wageBefore)

quantile(minwage$wageBefore, c(0.33, 0.66))


# lower/upper quartile, within 1.5 quaritles
boxplot(minwage$wageBefore)


hist(minwage$wageBefore)


