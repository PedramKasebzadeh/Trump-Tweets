####################################################################################
############################## Libraries  ##########################################
####################################################################################
####################################################################################
library(tidytext)
#library(textdata)
library(ggpubr)
library(ggplot2)
library(dplyr)
library(zoo)
library(rtweet)
library(twitteR)
#library(xlsx)

# text mining library
#library(tidytext)
library(syuzhet)



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

Oil = read.csv('Oil-Price.csv',header = TRUE,stringsAsFactors = FALSE)
Oil$Day =as.Date(Oil$Day,format="%m/%d/%y")
colnames(Oil) <- c("Date","Price")
orderd = Oil[order(Oil$Date),]
oil =orderd[which(orderd$Date=="2009-05-01") :nrow(orderd),] 

####################################################################################
##############################  Full   #############################################
####################################################################################

Trump= read.csv("Full-dataset.csv",
               quote = "",row.names = NULL,stringsAsFactors=FALSE)
colnames(Trump) <- c('Text','Date')
Trump$Date =as.Date(as.POSIXct(Trump$Date, format = "%m-%d-%Y %H:%M:%S"))


# Specifing the date for the perio Trump was president 
ind=which(Trump$Date=="2017-01-20")
Trump= Trump[ind[1]:nrow(Trump),]


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
################################## Trump dataset ####################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################




# merging with oil 
Trump = merge(Oil,Trump,by='Date',all.y = TRUE)
Trump=na.locf(Trump)
rownames(Trump) <- NULL
Trump = Trump[order(Trump$Date),]

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

# Saving the data in a secondary variable 
fulldata = Trump 

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


# tokenizing 
fulldata_stem <- fulldata %>%
  select(Text) %>%
  unnest_tokens(word, Text)

cleaned_fulldata <- fulldata_stem %>%
  anti_join(stop_words)


# Ploting word distribution in bing 
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


write.csv(fulldf,'fulldf.csv')

cor.test(fulldf$Price,fulldf$Afinn_score.x,method = "kendall", alternative = "greater",
         exact = FALSE)


#######################################################################################
#######################################################################################
#######################################################################################
############################## Evaluation #############################################
#######################################################################################
#######################################################################################
#######################################################################################

#The Evaluation is done in Python using Kmeans 


# using the full data set with related columns 
python = read.csv("fulldata.csv")
python = python[,c(2,4,5,6,7)]



# removing neutral sentiments 
python=python[(!(python$bing_score==0)),] 
python=python[(!(python$nrc_score==0)),]
python=python[(!(python$afin_score==0)),] 

# scailing the scores 
python$bing_score = ifelse(python$bing_score>0,1,ifelse(python$bing_score<0,-1,0))
python$nrc_score = ifelse(python$nrc_score>0,1,ifelse(python$nrc_score<0,-1,0))
python$afin_score = ifelse(python$afin_score>0,1,ifelse(python$afin_score<0,-1,0))

# Saving output 
write.csv(python,"evaluationdata.csv")
