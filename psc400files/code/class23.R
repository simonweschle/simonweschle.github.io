
### Class Code from Apr 28


## R packages for next week
rm(list=ls(all=TRUE))

install.packages("stringr")
install.packages("rvest")
install.packages("magrittr")


# HW Review
rm(list=ls(all=TRUE))

intrade08 <- read.csv("https://simonweschle.github.io/psc400files/data/intrade08.csv")
pres08 <- read.csv("https://simonweschle.github.io/psc400files/data/pres08.csv")


# Q1
intrade08 <- merge(intrade08, pres08, by="state")

intrade08$day <- as.Date(intrade08$day)
intrade08$daystoelection <- intrade08$day - as.Date("2008-11-04")


intrade08$obamapred <- ifelse(intrade08$PriceD > intrade08$PriceR, 1, 0)
intrade08$obamaEV <- ifelse(intrade08$obamapred==1, intrade08$EV, 0)


dayvec <- seq(-120, -1, 1)
n <- length(dayvec)
obamaEV.trade <- rep(NA, n)

for(i in 1:n){
	usedata <- intrade08[intrade08$daystoelection==dayvec[i],]
	obamaEV.trade[i] <- sum(usedata$obamaEV)
}


plot(dayvec, obamaEV.trade, type="l", lwd=3)
abline(h=365)




# Text analysis
rm(list=ls(all=TRUE))

library(tm)
library(SnowballC)
library(wordcloud)


# load the data
corpus.raw <- Corpus(DirSource(directory = "~/Dropbox/Teaching/2021_PSC_400/classes/week_12/data/federalist", pattern = "fp"))


### need to pre-process text

## make lower case
corpus.prep <- tm_map(corpus.raw, content_transformer(tolower))

## remove white space
corpus.prep <- tm_map(corpus.prep, stripWhitespace)

## remove punctuation
corpus.prep <- tm_map(corpus.prep, removePunctuation)

## remove numbers
corpus.prep <- tm_map(corpus.prep, removeNumbers)



## Predicting authorships
dtm <- DocumentTermMatrix(corpus.prep)
dtm <- as.matrix(dtm)

# term frequency per 1000 words
tfm <- dtm / rowSums(dtm) * 1000 

## words of interest
words <- c("although", "always", "commonly", "consequently", "considerable", "enough", "there", "upon", "while", "whilst")
## select only these words
tfm <- tfm[, words]
head(tfm)


hamilton <- c(1, 6:9, 11:13, 15:17, 21:36, 59:61, 65:85)
madison <- c(10, 14, 37:48, 58)


tfm.hamilton <- tfm[hamilton,]
tfm.madison <- tfm[madison,]

colMeans(tfm.hamilton)
colMeans(tfm.madison)

# Hamilton: likes there, upon
# Madison: likes whilst, consequently

# create a vector of authors
author <- rep(NA, nrow(dtm1))
author[hamilton] <- 1 # 1 if Hamilton
author[madison] <- -1 # -1 if Madison

# create data frame for regression
author.data <- data.frame(author=author, tfm)
author.data <- author.data[is.na(author.data$author)==F,]



hm.fit <- lm(author ~ upon + there + consequently + whilst, data=author.data)
summary(hm.fit)


# fitted values
author.data$hm.fit <- predict(hm.fit)
author.data$hm.pred <- ifelse(author.data$hm.fit>0, 1, -1)


table(author=author.data$author, pred.author=author.data$hm.pred)

plot(hamilton, author.data$hm.fit[author.data$author==1], xlim=c(1, 85), ylim=c(-2, 2), xlab="Federalist Papers", ylab="Predicted Values", pch=16, col="black")
points(madison, author.data$hm.fit[author.data$author==-1], pch=16, col="gray")
abline(h=0, lty="dashed")


# predict values of disputed papers

# 11 essays with disputed authorship
disputed <- c(49, 50:57, 62, 63) 
tf.disputed <- as.data.frame(tfm[disputed,])


disputed.pred <- predict(hm.fit, newdata=tf.disputed)
points(disputed, disputed.pred, pch=16, col="red")

