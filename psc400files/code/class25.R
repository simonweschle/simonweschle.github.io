
### PSC 400, Spring 2022
### Week 13, Monday 4/25


rm(list=ls(all=TRUE))

library(tm)
library(SnowballC)
library(wordcloud)


### FEDERALIST PAPERS

# load the data
corpus.raw <- Corpus((DirSource(directory = "~/Dropbox/Teaching/2022_1_PSC_400/classes/week_11/data/federalist", pattern = "fp")))


### need to pre-process text

## make lower case
corpus <- tm_map(corpus.raw, content_transformer(tolower))

## remove white space
corpus <- tm_map(corpus, stripWhitespace)

## remove punctuation
corpus <- tm_map(corpus, removePunctuation)

## remove numbers
corpus <- tm_map(corpus, removeNumbers)

## remove stop words
corpus <- tm_map(corpus, removeWords, stopwords("english"))

## stem remaining words
corpus <- tm_map(corpus, stemDocument)



### Document-Term Matrix: Word frequencies

dtm <- DocumentTermMatrix(corpus)
dtm.mat <- as.matrix(dtm)

dtm.tfidf <- weightTfIdf(dtm)
dtm.tfidf.mat <- as.matrix(dtm.tfidf) 


# wordclouds
# 12th letter
wordcloud(colnames(dtm.mat), dtm.mat[12, ], max.words = 20)

# all letters
wordcloud(colnames(dtm.mat), colSums(dtm.mat), max.words = 20)


# identify clusters of similar topics
hamilton <- c(1, 6:9, 11:13, 15:17, 21:36, 59:61, 65:85)
dtm.tfidf.hamilton <- dtm.tfidf.mat[hamilton, ]



km.out <- kmeans(dtm.tfidf.hamilton, centers = 4)

km.out$cluster
table(km.out$cluster)


sort(km.out$centers[1, ], decreasing = TRUE)[1:10]
km.out$cluster[km.out$cluster==1]

sort(km.out$centers[2, ], decreasing = TRUE)[1:10]
km.out$cluster[km.out$cluster==2]

sort(km.out$centers[3, ], decreasing = TRUE)[1:10]
km.out$cluster[km.out$cluster==3]

sort(km.out$centers[4, ], decreasing = TRUE)[1:10]
km.out$cluster[km.out$cluster==4]














## NY state assembly

rm(list=ls(all=TRUE))

library(tm)
library(SnowballC)
library(wordcloud)

load("~/Dropbox/Teaching/2022_1_PSC_400/classes/week_13/data/nyassembly.rda")


corpus.raw <- Corpus(VectorSource(assembly$biography))



### pre-process text

## make lower case
corpus <- tm_map(corpus.raw, content_transformer(tolower))

## remove white space
corpus <- tm_map(corpus, stripWhitespace)

## remove punctuation
corpus <- tm_map(corpus, removePunctuation)

## remove numbers
corpus <- tm_map(corpus, removeNumbers)

## remove stop words
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removeWords, stopwords("spanish"))

## stem remaining words
corpus <- tm_map(corpus, stemDocument)



dtm <- DocumentTermMatrix(corpus)
dtm.mat <- as.matrix(dtm)




# one representative
wordcloud(colnames(dtm.mat), dtm.mat[1,], max.words = 20)

# all representatives
wordcloud(colnames(dtm.mat), colSums(dtm.mat), max.words = 20)


# most common words
sort(colSums(dtm.mat), decreasing=T)[1:20]




