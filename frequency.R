rm(list=ls())
library(readxl)
library(quanteda)
library(dplyr)
library(ggplot2)
library(readtext)
xiake_trade <- read_excel("/Users/martin/Desktop/lab/xiake_trade.xlsx")

#String of Experimert News
exp <- toString(xiake_trade[1,'text'])

#Tokenize the article 
ch_stop <- stopwords("zh", source = "misc")
ch_toks <- exp %>% 
  tokens(remove_punct = TRUE) %>%
  tokens_remove(pattern = ch_stop)

#Create a DataFrame for Tokenized World
ch_dfm <- dfm(ch_toks)

#Top Word Frequency
topfeatures(ch_dfm)

#Word Frequency
a <- textstat_frequency(ch_dfm)

#A for loop to count certain world frequency
vec <- vector()

for (i in 1:nrow(xiake_trade))
{news <- toString(xiake_trade[i,'text'])
 ch_stop <- stopwords("zh", source = "misc")
 news_toks <- news %>% 
  tokens(remove_punct = TRUE) %>%
  tokens_remove(pattern = ch_stop)
 news_dfm <- dfm(news_toks)
 a <- textstat_frequency(news_dfm)
 g <- filter(a,feature == '霸权')
 if(nrow(g)==0){vec[i]=0} else{vec[i]=g[1,'frequency']}}

#Plot Frequency across time 
vec <- as.data.frame(vec, stringsAsFactors=FALSE);
freq <- cbind(vec,xiake_trade$date_published)
colnames(freq) <- c("frequency","date")
ggplot(freq, aes(x=date,y = frequency)) +geom_bar(stat = "identity",color='royalblue')

#Import a dictionary 
sen <- read_excel('/Users/martin/Desktop/lab/sentiment/Sentiment.xlsx')
neg <- as.list(sen[,1 ])
pos <- as.list(sen[,2 ])
di <- rbind(neg,pos)
names(di)<-c('positive','negative')
dic1 <- dictionary(di)

#Experiment with the dictionary 
View(dic1)
b<-dfm_lookup(ch_dfm,dic1)
View(b)

