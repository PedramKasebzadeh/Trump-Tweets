# Naive bayes 
library(e1071)
library(tm)
library(RTextTools)
library(dplyr)
library(caret)
# Library for parallel processing
library(doMC)
registerDoMC(cores=detectCores())
#importing the data 
labeled = read.csv('export_dataframe.csv')
# choosing relevnt columns 
labeled = labeled[,c(3,2)]
colnames(labeled)= c("Text", "Sentiment")
# giving values to characters 
labeled$Sentiment = ifelse(labeled$Sentiment == "neg", -1, 1 )

labeled$Text = as.character(labeled$Text)
# randomizing the data 
randomsample = sample(1:11914,11914,replace = F) 
labeled2 =  labeled[randomsample,]
labeled2$Sentiment = as.factor(labeled2$Sentiment)

# tokenizing, creating a Bag of words, cleaning the text , removing stop words 
corpus <- Corpus(VectorSource(labeled2$Text))
corpus.clean <- corpus %>%
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords(kind="en")) %>%
  tm_map(stripWhitespace)
# Document Term Matrix for each bag of words 
dtm <- DocumentTermMatrix(corpus.clean)


# train and test data 
df.train = labeled2[1:3575,]
df.test = labeled2[3576:11914,]

dtm.train <- dtm[1:3575,]
dtm.test <- dtm[3576:11914,]

corpus.clean.train <- corpus.clean[1:3575]
corpus.clean.test <- corpus.clean[3576:11914]


# Feature Selection , removing words which were used less than 5 times 
fivefreq <- findFreqTerms(dtm.train, 5)
length((fivefreq))
## [1] 12144

# Use only 5 most frequent words (fivefreq) to build the DTM

dtm.train.nb <- DocumentTermMatrix(corpus.clean.train, control=list(dictionary = fivefreq))

dim(dtm.train.nb)
## [1]  1500 12144
dtm.test.nb <- DocumentTermMatrix(corpus.clean.test, control=list(dictionary = fivefreq))
dim(dtm.train.nb)

# converting  word frequencies to yes and no labels
convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}

# train and test data sets 
trainNB = apply(dtm.train.nb, 2, convert_count)
testNB = apply(dtm.test.nb, 2, convert_count)

# training the classifier 
classifier = naiveBayes(trainNB, df.train$Sentiment) 
# Testing the classifier 
pred = predict(classifier, newdata=testNB) 

# Table of true and false posetive and negatives 
table("Predictions"= pred,  "Actual" = df.test$Sentiment )

# confusion matrix
conf.mat <- confusionMatrix(pred, df.test$Sentiment)

conf.mat


conf.mat$byClass


conf.mat$overall

#  Accuracy
conf.mat$overall['Accuracy']



https://rpubs.com/cen0te/naivebayes-sentimentpolarity
https://rpubs.com/meisenbach/272229
https://www.offerzen.com/blog/how-i-built-a-trump-related-tweet-sentiment-analysis-tool-with-elasticsearch-and-kibana



Lecture slides from the Stanford NLP Coursera course by Dan Jurafsky and Christopher Manning: https://web.stanford.edu/~jurafsky/NLPCourseraSlides.html

Machine Learning with R by Brett Lantz: https://www.packtpub.com/big-data-and-business-intelligence/machine-learning-r

https://www.tidytextmining.com/sentiment.html

https://cran.r-project.org/web/packages/text2vec/vignettes/text-vectorization.html

https://monkeylearn.com/blog/sentiment-analysis-using-r/
  
  https://monkeylearn.com/sentiment-analysis/
  
  https://www.kaggle.com/rtatman/tutorial-sentiment-analysis-in-r
