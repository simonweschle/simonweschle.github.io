
### Class Code from Apr 26

rm(list=ls(all=TRUE))

# install.packages("tm")
# install.packages("SnowballC")
# install.packages("wordcloud")

library(tm)
library(SnowballC)
library(wordcloud)


# load the data
corpus.raw <- Corpus(DirSource(directory = "~/Dropbox/Teaching/2021_PSC_400/classes/week_12/data/federalist", pattern = "fp"))

corpus.raw
content(corpus.raw[[1]])




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


content(corpus[[1]])



### Document-Term Matrix: Word frequencies

dtm <- DocumentTermMatrix(corpus)
dtm

dtm.mat <- as.matrix(dtm)

dtm.mat[1:10,1:10]




### Topic Discovery

wordcloud(colnames(dtm.mat), dtm.mat[12, ], max.words = 20)
	# economy-related: # "The utility of the Union in respect to revenue"
wordcloud(colnames(dtm.mat), dtm.mat[24, ], max.words = 20)
	# security-related: "The powers necessary to the common defense further considered"



## a certain term’s high frequency within a document means little if that term often appears across the documents of the corpus. To address this issue, we should downweight the terms that occur frequently across documents.
# term frequency–inverse document frequency: tf-idf
# The tf–idf value increases proportionally to the number of times a word appears in the document and is offset by the number of documents in the corpus that contain the word, which helps to adjust for the fact that some words appear more frequently in general

dtm.tfidf <- weightTfIdf(dtm)

# convert to matrix
dtm.tfidf.mat <- as.matrix(dtm.tfidf) 

dtm.tfidf.mat[1:10,1:10]

sort(dtm.tfidf.mat[12,], decreasing=TRUE)[1:10]
sort(dtm.tfidf.mat[24,], decreasing=TRUE)[1:10]


wordcloud(colnames(dtm.tfidf.mat), dtm.tfidf.mat[12, ], max.words = 20)
wordcloud(colnames(dtm.tfidf.mat), dtm.tfidf.mat[24, ], max.words = 20)


# identify clusters of similar topics

hamilton <- c(1, 6:9, 11:13, 15:17, 21:36, 59:61, 65:85)

dtm.tfidf.hamilton <- dtm.tfidf.mat[hamilton, ]

km.out <- kmeans(dtm.tfidf.hamilton, centers = 4)


sort(km.out$centers[1, ], decreasing = TRUE)[1:10]
km.out$cluster
km.out$cluster[km.out$cluster==1]


sort(km.out$centers[2, ], decreasing = TRUE)[1:10]
km.out$cluster[km.out$cluster==2]

sort(km.out$centers[3, ], decreasing = TRUE)[1:10]
km.out$cluster[km.out$cluster==3]

sort(km.out$centers[4, ], decreasing = TRUE)[1:10]
km.out$cluster[km.out$cluster==4]

# https://en.wikipedia.org/wiki/The_Federalist_Papers



