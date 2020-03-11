library(tm)
library(RTextTools)
library(e1071)
library(dplyr)
library(caret)
# Library for parallel processing
library(doMC)
registerDoMC(cores=detectCores()) 

df<- read.csv("movie-pang02.csv", stringsAsFactors = FALSE)
glimpse(df)
set.seed(1)
df <- df[sample(nrow(df)), ]
df <- df[sample(nrow(df)), ]
glimpse(df)

df$class <- as.factor(df$class)

corpus <- Corpus(VectorSource(df$text))
# Inspect the corpus
corpus
## <<VCorpus>>
## Metadata:  corpus specific: 0, document level (indexed): 0
## Content:  documents: 2000
inspect(corpus[1:3])


corpus.clean <- corpus %>%
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords(kind="en")) %>%
  tm_map(stripWhitespace)
dtm <- DocumentTermMatrix(corpus.clean)
# Inspect the dtm
inspect(dtm[40:50, 10:15])


df.train <- df[1:1500,]
df.test <- df[1501:2000,]

dtm.train <- dtm[1:1500,]
dtm.test <- dtm[1501:2000,]

corpus.clean.train <- corpus.clean[1:1500]
corpus.clean.test <- corpus.clean[1501:2000]


dim(dtm.train)


fivefreq <- findFreqTerms(dtm.train, 5)
length((fivefreq))
## [1] 12144

# Use only 5 most frequent words (fivefreq) to build the DTM

dtm.train.nb <- DocumentTermMatrix(corpus.clean.train, control=list(dictionary = fivefreq))

dim(dtm.train.nb)
## [1]  1500 12144

dtm.test.nb <- DocumentTermMatrix(corpus.clean.test, control=list(dictionary = fivefreq))

dim(dtm.train.nb)
## [1]  1500 12144

convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}
trainNB <- apply(dtm.train.nb, 2, convert_count)
testNB <- apply(dtm.test.nb, 2, convert_count)

system.time( classifier <- naiveBayes(trainNB, df.train$class, laplace = 1) )
system.time( pred <- predict(classifier, newdata=testNB) )
table("Predictions"= pred,  "Actual" = df.test$class )
conf.mat <- confusionMatrix(pred, df.test$class)

conf.matconf.mat$byClass
conf.mat$overall
conf.mat$overall['Accuracy']
