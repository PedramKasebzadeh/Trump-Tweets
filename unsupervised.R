library(sentimentr)
library(tm)

filtered= read.csv("filtered.csv",quote = "",
                   row.names = NULL,stringsAsFactors=FALSE)
filtered= filtered[,c(4,2)]
colnames(filtered) <- c('Text','Date')
#filtered$Date =as.Date(as.POSIXct(filtered$Date, format = "%m-%d-%Y %H:%M:%S"))


# idea from https://towardsdatascience.com/unsupervised-sentiment-analysis-a38bf1906483

#for test 
data = filtered[1:300,]

courps = iconv(data$Text)#
courps = Corpus(VectorSource(courps))

# Cleaning the text 
courps = tm_map(courps,tolower)
courps = tm_map(courps,removePunctuation)
courps = tm_map(courps,removeNumbers)
courps = tm_map(courps,removeWords,stopwords('english'))
removeURL = function(x) gsub('http[[:alnum:]]*', '',x)
cleanset = tm_map(courps,content_transformer(removeURL))
cleanset = tm_map(cleanset,stripWhitespace)

tdm = TermDocumentMatrix(cleanset)#control = list(minwordlength=c(1,Inf)))
#t = removeSparseTerms(tdm,sparse = 0.98)
m = as.matrix(tdm)
frq = rowSums(m)
frq = subset(frq,frq >=25)

barplot(frq,las=2,col=rainbow(25))


# idea from https://gist.github.com/primaryobjects/8038d345aae48ae48988906b0525d175

library(wordVectors)
library(magrittr)
prep_word2vec(origin="cookbooks",destination="cookbooks.txt",lowercase=T,bundle_ngrams=2)

word2vec = prep_word2vec(origin=data$Text, destination='temp.prep',
                         lowercase=T, bundle_ngrams=2)

model <- train_word2vec(prepFileName, binaryFileName, vectors=200, threads=4, window=12, iter=5, negative_samples=0)

#library(devtools)
#install_github("dselivanov/text2vec")
library(text2vec)
library(data.table)
library(magrittr)
data("movie_review")
setDT(data)
setkey(movie_review, id)
set.seed(2017L)
all_ids = movie_review$id
train_ids = sample(all_ids, 4000)
test_ids = setdiff(all_ids, train_ids)
train = movie_review[J(train_ids)]
test = movie_review[J(test_ids)]

library(glmnet)
library(stringr)


# https://www.r-bloggers.com/glove-vs-word2vec-revisited/
it2 <- itoken(data$Text, 
              preprocess_function = function(x) 
                str_split(x, fixed("t")) %>% 
                # select only the body of the article
                sapply(.subset2, 2), 
              tokenizer = function(x) str_split(x, fixed(" ")))
vocab <- create_vocabulary(it2)
str(vocab)
TOKEN_LIMIT = 30000L
TOKEN_DOC_PROPORTION = 0.3
pruned_vocab <- prune_vocabulary(vocabulary = vocab, 
                                 doc_proportion_max = TOKEN_DOC_PROPORTION,doc_count_max = TOKEN_LIMIT)
write.table(x = data.frame("word" = pruned_vocab$term, "id" = 0:(1781)), 
           # file = "/path/to/destination/dir/pruned_vocab.csv",
            quote = F, sep = ',', row.names = F, col.names = F)
WINDOW = 10L
# create iterator over files in directory
it <- idir('~/Downloads/datasets/splits/')
# create iterator over tokens
it2 <- itoken(data$Text, 
              preprocess_function = function(x) 
                str_split(x, fixed("t")) %>% 
                # select only the body of the article
                sapply(.subset2, 2), 
              tokenizer = function(x) str_split(x, fixed(" ")))
# create_vocab_corpus can construct documen-term matrix and term-cooccurence matrix simultaneously
# here we are not interesting in documen-term matrix, so set `grow_dtm = FALSE`
corpus <- create_vocab_corpus(it2, vocabulary = pruned_vocab, grow_dtm = FALSE, skip_grams_window = WINDOW)
# in this call, we wrap std::unordered_map into R's dgTMatrix
tcm <- get_tcm(corpus)