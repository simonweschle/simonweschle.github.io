### PSC 400, Spring 2024
### Week 12, Tuesday 4/16


## R packages for Thursday
rm(list=ls(all=TRUE))

install.packages("stringr")
install.packages("rvest")
install.packages("magrittr")

# in addition, install Chrome and this: https://bit.ly/3t1dc9c




### SPATIAL DATA


rm(list=ls(all=TRUE))

library(ggmap)
library(tmaptools)


register_stadiamaps(key="be0e5c81-9dee-4951-88aa-061d02da4232")


syracuse_lines <- get_stadiamap(bbox = c(left = -76.20463, bottom = 42.98416, right = -76.07427, top = 43.08612), zoom = 13, maptype="stamen_terrain_lines")
ggmap(syracuse_lines)


accidents <- read.csv("~/Dropbox/Teaching/2022_1_PSC_400/classes/week_11/data/syracuse_accidents.csv")


ggmap(syracuse_lines) + geom_point(data = accidents, aes(x=Start_Lng, y=Start_Lat)) 


dens <- stat_density2d(data=accidents, aes(x=Start_Lng, y=Start_Lat, fill=..level.., alpha=..level..), contour=T, n=100, geom="polygon")

ggmap(syracuse_lines) + dens + theme(legend.position="none")

ggmap(syracuse_lines) + dens + theme(legend.position="none") + scale_fill_gradient(low = "white", high = "red", guide = FALSE)






rm(list=ls(all=TRUE))

# install.packages("tm")
# install.packages("SnowballC")
# install.packages("wordcloud")

library(tm)
library(SnowballC)
library(wordcloud)


# example
corpus.raw <- Corpus(VectorSource(c("John likes to watch movies. Mary likes movies too.", "Mary also likes to watch football games.")))
content(corpus.raw[[2]])


### need to pre-process text

## make lower case
corpus.prep <- tm_map(corpus.raw, content_transformer(tolower))

## remove white space
corpus.prep <- tm_map(corpus.prep, stripWhitespace)

## remove punctuation
corpus.prep <- tm_map(corpus.prep, removePunctuation)

## remove numbers
corpus.prep <- tm_map(corpus.prep, removeNumbers)

## remove stop words
stopwords("english")
corpus <- tm_map(corpus.prep, removeWords, stopwords("english"))

## stem remaining words
corpus <- tm_map(corpus, stemDocument)

content(corpus[[2]])

dtm <- DocumentTermMatrix(corpus)
dtm

dtm.mat <- as.matrix(dtm)
dtm.mat





