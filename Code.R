####################################################################################
####################################################################################
############################## Libraries  ##########################################
####################################################################################
####################################################################################

rm(list=setdiff(ls(),c("Oil","full","filtered",'Hi')))



#library(tidytext)
#library(textdata)
library(ggpubr)
library(ggplot2)
library(dplyr)
library(zoo)
library(rtweet)
library(twitteR)
library(xlsx)

# text mining library
library(tidytext)
library(syuzhet)


library(SentimentAnalysis) # by Stefan Feuerriegel(https://cran.r-project.org/web/packages/SentimentAnalysis/vignettes/SentimentAnalysis.html)








####################################################################################
####################################################################################
####################################################################################
####################################################################################
##############################  datasets  ##########################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################



####################################################################################
##############################  Oil   ##############################################
####################################################################################

Oil = read.csv('oil2.csv',header = TRUE,stringsAsFactors = FALSE)
Oil$Day =as.Date(Oil$Day,format="%m/%d/%y")
colnames(Oil) <- c("Date","Price")
orderd = Oil[order(Oil$Date),]
oil =orderd[which(orderd$Date=="2009-05-01") :nrow(orderd),] 

####################################################################################
##############################  Full   #############################################
####################################################################################

full= read.csv("Full-dataset.csv",
               quote = "",row.names = NULL,stringsAsFactors=FALSE)
colnames(full) <- c('Text','Date')
full$Date =as.Date(as.POSIXct(full$Date, format = "%m-%d-%Y %H:%M:%S"))


####################################################################################
##############################  Filtered  ##########################################
####################################################################################

filtered= read.csv("filtered.csv",quote = "",
                   row.names = NULL,stringsAsFactors=FALSE)
colnames(filtered) <- c('Text','Date')
filtered$Date =as.Date(as.POSIXct(filtered$Date, format = "%m-%d-%Y %H:%M:%S"))

####################################################################################
####################################################################################
############################## Sentiment analysis function #########################
####################################################################################
####################################################################################
####################################################################################

# A function for doing the sentiment analysis over difrent data sets 

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


####################################################################################
################################## NRC Function####################################
####################################################################################

tweets_sentiment2 = function(tweets){
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
    # nrc sentiment 
    inner_join(get_sentiments('nrc')) %>%
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


####################################################################################
####################################################################################
############################# Output algoritms #####################################
####################################################################################
####################################################################################

finalfunction = function(data){
  data$bing_score=unlist(lapply(data$Text,function(x){bing_sentiment(x)}))
  data$nrc_score = sapply(data$Text, function(x)get_sentiment(x,method = 'nrc'))
  data$afin_score = sapply(data$Text, function(x)get_sentiment(x,method = 'afinn'))
  return(data)
}

####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
############################# Filtere data #########################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################


# merging with oil 
filtered = merge(Oil,filtered,by='Date',all.y = TRUE)
filtered=na.locf(filtered)
rownames(filtered) <- NULL
filtered = filtered[order(filtered$Date),]

ggplot(filtered)+
  geom_line(aes(x=Date,y=Price))

# credit to : https://www.earthdatascience.org/courses/earth-analytics/get-data-using-apis/text-mining-twitter-data-intro-r/
# Lowercase
filtered$Text = tolower(filtered$Text)
# Remove everything that is not a number or letter (may want to keep more 
# stuff in your actual analyses). 
filtered$Text <- stringr::str_replace_all(filtered$Text,"[^a-zA-Z\\s]", " ")
# Shrink down to just one white space
filtered$Text <- stringr::str_replace_all(filtered$Text,"[\\s]+", " ")

# credit to: https://towardsdatascience.com/twitter-sentiment-analysis-and-visualization-using-r-22e1f70f6967

# remove http  elements 
#filtered$stripped_text = gsub("http\\S+","",  filtered$Text)
#filtered$stripped_text = gsub("http.*","",  filtered$stripped_text)
#filtered$stripped_text <- gsub("https.*","", filtered$stripped_text)

#filtered_stem <- filtered %>%
##  select(stripped_text) %>%
#  unnest_tokens(word, stripped_text)

#cleaned_filtered <- filtered_stem %>%
#  anti_join(stop_words)




#get_sentiments("bing") %>% filter(sentiment=="positive")

#bing_filtered = cleaned_filtered%>%
#  inner_join(get_sentiments('bing')) %>%
#  dplyr::count(word,sentiment,sort = TRUE) %>%
#  ungroup()

#bing_filtered %>%
#  group_by(sentiment) %>%
#  top_n(10) %>%
#  ungroup() %>%
#  mutate(word=reorder(word,n)) %>%
#  ggplot(aes(word,n,fill=sentiment))+
#  geom_col(show.legend = FALSE)+
#  facet_wrap(~sentiment,scales='free_y')+
 # coord_flip()

filtered_save=filtered
filtered=finalfunction(filtered)
#hist(unlist(full_points),breaks = 20)
#plot(ts(unlist(full_points)))
#filtered_Oil = (filtered$Price - min(filtered$Price)) * (7 - (-7)) / (max(filtered$Price) - min(filtered$Price)) + (-7)
#lines(full_Oil,col="red")

bingsum=aggregate(filtered$bing_score, by=list(Date=filtered$Date), FUN=sum)
nrcsum=aggregate(filtered$nrc_score, by=list(Date=filtered$Date), FUN=sum)
afinnsum=aggregate(filtered$afin_score, by=list(Date=filtered$Date), FUN=sum)
filtereddf = cbind("Date"=bingsum$Date,"bing_score"=bingsum$x,"nrc_score"=nrcsum,"Afinn_score"=afinnsum)
filtereddf=filtereddf[,-c(3,5)]



#oilfinal_full = filter(full_Oil, Date %in% filtereddf$Date)
filtereddf = merge(oil,filtereddf, 
               by= "Date",all.y = TRUE)
filtereddf=na.locf(filtereddf)
filtereddf$Price = (filtereddf$Price - min(filtereddf$Price)) * (7 - (-7)) / (max(filtereddf$Price) - min(filtereddf$Price)) + (-7)
filtereddf$Price2 =  (filtereddf$Price - min(filtereddf$Price)) * (52 - (-30)) / (max(filtereddf$Price) - min(filtereddf$Price)) + (-30)

ggplot(filtereddf)+
  geom_line(aes(x=Date,y=filtereddf$Price,color = "Scaled price"),alpha=1)+
  geom_line(aes(x=Date,y=filtereddf$bing_score,color="Bing score"),alpha=0.50)+
  geom_line(aes(x=Date,y=filtereddf$Price2,color="Price in 2nd range"),alpha=0.65)+
  geom_line(aes(x=Date,y=filtereddf$nrc_score.x,color="NRC score"),alpha=0.60)+
  geom_line(aes(x=Date,y=scale(filtereddf$Afinn_score.x,0),color="Afinn score"),alpha=0.55)


cor.test(filtereddf$Price3,filtereddf$Afinn_score.x)

####################################################################################
########################  NRC     ##################################################
####################################################################################


#Joining, by = "word"
#Do you want to download:
#  Name: NRC Word-Emotion Association Lexicon 
#URL: http://saifmohammad.com/WebPages/lexicons.html 
#License: License required for commercial use. Please contact Saif M. Mohammad (saif.mohammad@nrc-cnrc.gc.ca). 
#Size: 22.8 MB (cleaned 424 KB) 
#Download mechanism: http 
#Citation info:
  
#  This dataset was published in Saif M. Mohammad and Peter Turney. (2013), ``Crowdsourcing a Word-Emotion Association Lexicon.'' Computational Intelligence, 29(3): 436-465.

#article{mohammad13,
#  author = {Mohammad, Saif M. and Turney, Peter D.},
#  title = {Crowdsourcing a Word-Emotion Association Lexicon},
#  journal = {Computational Intelligence},
#  volume = {29},
#  number = {3},
#  pages = {436-465},
#  doi = {10.1111/j.1467-8640.2012.00460.x},
#  url = {https://onlinelibrary.wiley.com/doi/abs/10.1111/j.1467-8640.2012.00460.x},
#  eprint = {https://onlinelibrary.wiley.com/doi/pdf/10.1111/j.1467-8640.2012.00460.x},
#  year = {2013}
#}
#If you use this lexicon, then please cite it. #

#1: Yes
#2: No

####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
################################## Full dataset ####################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################




# merging with oil 
full = merge(Oil,full,by='Date',all.y = TRUE)
full=na.locf(full)
rownames(full) <- NULL
full = full[order(full$Date),]

#ggplot(full)+
#  geom_line(aes(x=Date,y=Price))

plot1=ggplot(Oil)+
  geom_line(aes(x=Oil$Date,y=Oil$Price),color="Blue")+
  xlab("Date")+
  ylab("Oil Price")+
  ggtitle('over the whole period')


orderd = Oil[order(Oil$Date),]
oil =orderd[which(orderd$Date=="2009-05-01") :nrow(orderd),] 

plot2=ggplot(oil)+
  geom_line(aes(x=oil$Date,y=oil$Price),color="red")+
  xlab("May 2009 to February 2020")+
  ylab("Oil Price")+
  ggtitle('over the related period')



ggarrange(plot1,plot2,ncol=1,label.x = 'Oil Price')

fulldata = full 

# credit to : https://www.earthdatascience.org/courses/earth-analytics/get-data-using-apis/text-mining-twitter-data-intro-r/
# Lowercase
fulldata$Text = tolower(fulldata$Text)
# Remove everything that is not a number or letter (may want to keep more 
# stuff in your actual analyses). 
fulldata$Text <- stringr::str_replace_all(fulldata$Text,"[^a-zA-Z\\s]", " ")
# Shrink down to just one white space
fulldata$Text <- stringr::str_replace_all(fulldata$Text,"[\\s]+", " ")

# credit to: https://towardsdatascience.com/twitter-sentiment-analysis-and-visualization-using-r-22e1f70f6967

# remove http  elements 
fulldata$Text = gsub("http\\S+","",  fulldata$Text)
fulldata$Text = gsub("http.*","",  fulldata$Text)
fulldata$Text <- gsub("https.*","", fulldata$Text)

fulldata_stem <- fulldata %>%
  select(Text) %>%
  unnest_tokens(word, Text)

cleaned_fulldata <- fulldata_stem %>%
  anti_join(stop_words)


#get_sentiments("bing") %>% filter(sentiment=="positive")

bing_fulldata = cleaned_fulldata%>%
  inner_join(get_sentiments('bing')) %>%
  count(word,sentiment,sort = TRUE) %>%
  ungroup()

bing_fulldata %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word,n)) %>%
  ggplot(aes(word,n,fill=sentiment))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~sentiment,scales='free_y')+
  ylab("Count")+
  coord_flip()


#write.csv(fulldata, " \\Users\\pedram\\Desktop\\fulldata.xlsx")
#write.csv(fulldata,'\\Users\\pedram\\Desktop\\Text-mining-.nosync\\TrumpTweets\\fulldata.csv')



fulldata_save=fulldata
fulldata=finalfunction(fulldata)
#hist(unlist(full_points),breaks = 20)
#plot(ts(unlist(full_points)))
full_Oil = (fulldata$Price - min(fulldata$Price)) * (7 - (-7)) / (max(fulldata$Price) - min(fulldata$Price)) + (-7)
#lines(full_Oil,col="red")

bingsum=aggregate(fulldata$bing_score, by=list(Date=fulldata$Date), FUN=sum)
nrcsum=aggregate(fulldata$nrc_score, by=list(Date=fulldata$Date), FUN=sum)
afinnsum=aggregate(fulldata$afin_score, by=list(Date=fulldata$Date), FUN=sum)
fulldf = cbind("Date"=bingsum$Date,"bing_score"=bingsum$x,"nrc_score"=nrcsum,"Afinn_score"=afinnsum)
fulldf=fulldf[,-c(3,5)]



#oilfinal_full = filter(full_Oil, Date %in% fulldf$Date)
fulldf = merge(oil,fulldf, 
                     by= "Date",all.y = TRUE)
fulldf=na.locf(fulldf)
fulldf$Price = (fulldf$Price - min(fulldf$Price)) * (7 - (-7)) / (max(fulldf$Price) - min(fulldf$Price)) + (-7)
fulldf$Price2 =  (fulldf$Price - min(fulldf$Price)) * (52 - (-30)) / (max(fulldf$Price) - min(fulldf$Price)) + (-30)

ggplot(fulldf)+
  geom_line(aes(x=Date,y=fulldf$Price,color = "Scaled price"))+
  geom_line(aes(x=Date,y=fulldf$bing_score,color="Bing score"))+
  geom_line(aes(x=Date,y=fulldf$Price2,color="Price in 2nd range"))+
geom_line(aes(x=Date,y=fulldf$nrc_score.x,color="NRC score"))+
geom_line(aes(x=Date,y=fulldf$Afinn_score.x,color="Afinn score"))


#write.csv(fulldf,'fulldf.csv')

cor.test(fulldf$Price,fulldf$Afinn_score.x)


#######################################################################################
#######################################################################################
#######################################################################################
############################## Evaluation #############################################
#######################################################################################
#######################################################################################
#######################################################################################



#######################################################################################
############################## labeled data ###########################################
#######################################################################################


labeled = read.csv('export_dataframe.csv')
labeled = labeled[,c(3,2)]
colnames(labeled)= c("Text", "Sentiment")
labeled$Sentiment = ifelse(labeled$Sentiment == "neg", -1, 1 )
labeled$Text = as.character(labeled$Text)


# Sampling over labled data 
randomsample = sample(1:11914,400,replace = F) #400 samples 

testingbase = finalfunction(labeled[randomsample,]) # runing the function with sampled data
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


### SENTIMENTANALYSIS
labeled[randomsample,][1]
eval = analyzeSentiment(labeled$Text[randomsample])
eval_res = convertToBinaryResponse(eval)$SentimentQDAP
eval_res = ifelse(eval_res== "positive",1,-1)
#computing accuracy 
table(eval_res,labeled$Sentiment[randomsample])
Stefanacc = table(eval_res,labeled$Sentiment[randomsample])
Stefanacc = sum(diag(Stefanacc))/sum(Stefanacc)
Stefanacc
#cor.test(eval_res,labeled$Sentiment[randomsample])
