rm(list=ls())
library(quanteda)
library(ggplot2)
library(dplyr)

#Load the data 
china_daily <- read.csv("/Users/martin/Desktop/lab/china_daily_columnist_relevant.csv")

#Sort the data by date 
china_daily <- mutate(china_daily,date_published = as.Date(date_published, "%m/%d/%Y"))
china_daily <- arrange(china_daily,date_published)

#Look_up in dictionary
score <- vector()
for (i in 1:nrow(china_daily))
  {ana <- toString(china_daily[i,'text'])
  toks_news <- tokens(ana, remove_punct = TRUE)
  toks_news_cd <- tokens_lookup(toks_news, dictionary =  data_dictionary_LSD2015[1:2])
  dfmat_news_cd <- dfm(toks_news_cd)
  f<-convert(dfmat_news_cd, to = "data.frame")
  score[i] = f[1,3]-f[1,2]}

#Plot the score over time 
score <- as.data.frame(score, stringsAsFactors=FALSE);
sentiment <- cbind(score,china_daily$date_published)
colnames(sentiment) <- c("sentiment","date")
ggplot(sentiment, aes(x=date,y = sentiment)) +geom_line(color='gold2')