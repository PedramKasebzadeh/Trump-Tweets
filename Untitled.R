# testing the whole movie reviws with my method 


bing_sentiment = function(tweets){
  tweet_tibble = tibble(text=tweets) %>%
    mutate(
      #remove http 
      stripped_text = gsub("http\\S+","", text),
      stripped_text = gsub("http.*","", text),
      stripped_text <- gsub("https.*","", text)
    ) %>%
    # remove stop words 
    unnest_tokens(word, stripped_text) %>%
    anti_join(stop_words) %>%
    # bing sentiment 
    inner_join(get_sentiments('bing')) %>%
    dplyr::count(word,sentiment,sort = TRUE) %>%
    ungroup() %>%
    
    # a column for -1 for negative and +1 for posiive 
    mutate(
      score = case_when(
        sentiment== "positive"~ n*(1),
        sentiment== "negative"~ n*(-1))
    ) 
  #total score 
  sent_score = sum(tweet_tibble$score)
  
  unlist(sent_score)
}


finalfunction2 = function(data){
  data$bing_score=unlist(lapply(data,function(x){bing_sentiment(x)}))
  data$nrc_score = sapply(data, function(x)get_sentiment(x,method = 'nrc'))
  data$afin_score = sapply(data, function(x)get_sentiment(x,method = 'afinn'))
  return(data)
}


# Sampling over labled data 

testingbase = finalfunction2(testNB) # runing the function with sampled data
testing=testingbase
testing$bing_score = ifelse(testing$bing_score > 0 
                            , 1 ,ifelse(testing$bing_score<0,-1,0)) #converting the scale
testing$nrc_score = ifelse(testing$nrc_score > 0 ,
                           1 ,ifelse(testing$nrc_score<0,-1,0) )#converting the scale
testing$afin_score = ifelse(testing$afin_score > 0 
                            , 1 ,ifelse(testing$bing_score<0,-1,0))#converting the scale

###   Bing
testing = testing[-(which(testing$bing_score==0)),] #removing 0s since labled data didn't have any
#computing accuracy 
bingacc = table(testing$bing_score,testing$Sentiment)
bingacc = sum(diag(bingacc))/sum(bingacc)
bingacc
#cor.test(testing$bing_score,testing$Sentiment)


### NRC
testing = testing[-(which(testing$nrc_score==0)),]#removing 0s since labled data didn't have any
#computing accuracy 
nrcacc = table(testing$nrc_score,testing$Sentiment)
nrcacc = sum(diag(nrcacc))/sum(nrcacc)
nrcacc
#cor.test(testing$nrc_score,testing$Sentiment)

### AFINN
testing = testing[-(which(testing$afin_score==0)),]#removing 0s since labled data didn't have any
#computing accuracy 
Afinnacc = table(testing$afin_score,testing$Sentiment)
Afinnacc = sum(diag(Afinnacc))/sum(Afinnacc)
Afinnacc
#cor.test(testing$afin_score,testing$Sentiment)