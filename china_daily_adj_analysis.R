rm(list=ls(all=TRUE))

library(rvest)
library(devtools)
library(rJava)
library(RDRPOSTagger)
library(tokenizers)
library(dplyr)
library(stringr)
library(xml2)
library(openxlsx)
library(janitor)
library(openxlsx)
library(ggplot2)
library(qdap)


setwd("C:/Users/irene/Documents/GitHub/chinese_media")
file.edit('.Rprofile')
#enter the following code into Rprofile: Sys.setlocale(category = "LC_ALL", locale = "chs")

#gathering nk and trade data, converting dates and ordering by date
nk_data <- read.xlsx("C:/Users/irene/Documents/GitHub/chinese_media/realdata/china_daily/china_daily_nk.xlsx")
nk_data$date_published <- excel_numeric_to_date(nk_data$date_published)
nk_data <- nk_data[order(as.Date(nk_data$date_published, format="%Y/%m/%d")),]

us_data <- read.xlsx("C:/Users/irene/Documents/GitHub/chinese_media/realdata/china_daily/china_daily_trade.xlsx")
us_data$date_published <- excel_numeric_to_date(us_data$date_published)
us_data <- us_data[order(as.Date(us_data$date_published, format="%Y/%m/%d")),]


#this must be done to analyse POS
unipostag_types <- c("ADJ" = "adjective", "ADP" = "adposition", 
                     "ADV" = "adverb", "AUX" = "auxiliary", "CONJ" = "coordinating conjunction", 
                     "DET" = "determiner", "INTJ" = "interjection", "NOUN" = "noun", "NUM" = "numeral", 
                     "PART" = "particle", "PRON" = "pronoun", "PROPN" = "proper noun", "PUNCT" = "punctuation", 
                     "SCONJ" = "subordinating conjunction", "SYM" = "symbol", "VERB" = "verb", "X" = "other")


#NORTH KOREA

#for every row in the data, i am tokenizing the sentences and tagging each word's POS. Then I am pulling
#all of the adjectives and counting how many there are, then adding this to a list of all the adj values
#for each article. this takes a while, but also means you can pull out other parts of speech if you want

list <- c()
for(i in 1:nrow(nk_data)){
  article <- nk_data[i,3]
  sentences <- tokenize_sentences(article, simplify = TRUE)
  unipostags <- rdr_pos(unipostagger, sentences)
  #unipostags$word.type <- unipostag_types[unipostags$word.type]
  nrow(unipostags[unipostags$pos == "ADJ",] )
  adj_values <- c(nrow(unipostags[unipostags$pos == "ADJ",] ))
  list <- c(list, adj_values)
  i <- i + 1
}

#for every article in the data, I am counting how many words are in it. then adding each value to a list of
#all of the wordcounts
wordcountlistnk <- c()
for(i in 1:nrow(nk_data)){
  article2 <- nk_data[i,3]
  wordcount <- wc(article2)
  wordcountlistnk <- c(wordcountlistnk, wordcount)
  i <- i + 1
}

#adding these lists as columns to the data frame
testframe2 <- nk_data
testframe3 <- cbind(testframe2,list)
testframe4 <- cbind(testframe3, wordcountlistnk)

colnames(testframe4)[colnames(testframe4)=="wordcountlistnk"] <- "total_words"

#now dividing number of adjs by total words to find adj/total word ratio, making a list to add to the df
percentlistnk <- c()
for(i in 1:nrow(testframe4)){
  article3 <- testframe4[i,3]
  percent <- testframe4[i,6]/testframe4[i,7]
  percentlistnk <- c(percentlistnk, percent)
  i <- i + 1
}

testframe5 <- cbind(testframe4, percentlistnk)

#it's a good idea to save the df here because this process takes a long time!
save(testframe5, file = "C:/Users/irene/Documents/GitHub/chinese_media/posnk.Rdata")
colnames(testframe5)[colnames(testframe5)=="adj percentage"] <- "adj_percentage"
colnames(testframe5)[colnames(testframe5)=="total words"] <- "total_words"

#making a df with just date and adj
adjfreq <- data.frame("date_published" = testframe3$date_published, "adj" = testframe3$adj)

ggplot(aes(x = date_published, y = adj), data = adjfreq) + geom_point() +
  geom_smooth(span = 0.1) + ggtitle("Total Adjective Use in China Daily Articles about North Korea") +
  annotate("text", x = as.Date("2017-11-20"), y = 100, label = "Trump re-lists NK as State Sponsor of Terrorism", 
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-03-08"), y = 100, label = "Trump accepts Kim meeting", 
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-05-24"), y = 100, label = "Trump cancels", 
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-06-12"), y = 100, label = "Singapore summit", 
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-12-20"), y = 100, label = "NK won't denuclearize",
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2017-09-03"), y = 100, label = "NK H-Bomb Test",
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2017-06-04"), y = 100, label = "NK ICBM Test",
           angle = 90, size = 2)

#making a df with just date and adj ratio
adjpercent <- data.frame("date_published" = testframe3$date_published, "adj_percent" = testframe5$adj_percentage)

ggplot(aes(x = date_published, y = adj_percent), data = adjpercent) + geom_point() + geom_smooth(span = 0.1) +
  ggtitle("Adjective Use in China Daily Articles about North Korea as ADJ/Total Words") + 
  annotate("text", x = as.Date("2017-11-20"), y = 0.115, label = "Trump re-lists NK as State Sponsor of Terrorism", 
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-03-08"), y = 0.125, label = "Trump accepts Kim meeting", 
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-05-24"), y = 0.13, label = "Trump cancels", 
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-06-12"), y = 0.12, label = "Singapore summit", 
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-12-20"), y = 0.12, label = "NK won't denuclearize",
           angle = 90, size = 2)+
  annotate("text", x = as.Date("2017-06-04"), y = 0.12, label = "NK ICBM Test",
           angle = 90, size = 2)

#Chinatrade

#doing the same as above
list2 <- c()
for(i in 1:nrow(us_data)){
  article <- us_data[i,3]
  sentences <- tokenize_sentences(article, simplify = TRUE)
  unipostags <- rdr_pos(unipostagger, sentences)
  #unipostags$word.type <- unipostag_types[unipostags$word.type]
  nrow(unipostags[unipostags$pos == "ADJ",] )
  adj_values <- c(nrow(unipostags[unipostags$pos == "ADJ",] ))
  list2 <- c(list2, adj_values)
  i <- i + 1
}

wordcountlistus <- c()
for(i in 1:nrow(us_data)){
  article2 <- us_data[i,3]
  wordcount <- wc(article2)
  wordcountlistus <- c(wordcountlistus, wordcount)
  i <- i + 1
}


ustestframe <- us_data
ustestframe2 <- cbind(ustestframe,list2)

colnames(ustestframe2)[colnames(ustestframe2)=="list2"] <- "adj"

ustestframe3 <- cbind(ustestframe2, wordcountlistus)
colnames(ustestframe3)[colnames(ustestframe3)=="wordcountlistus"] <- "total_words"

percentlistus <- c()
for(i in 1:nrow(ustestframe3)){
  article <- ustestframe3[i,3]
  percent <- ustestframe3[i,6]/ustestframe3[i,7]
  percentlistus <- c(percentlistus, percent)
  i <- i + 1
}

ustestframe3 <- cbind(ustestframe3, percentlistus)
colnames(ustestframe3)[colnames(ustestframe3)=="percentlistus"] <- "adj_percentage"
save(ustestframe3, file = "C:/Users/irene/Documents/GitHub/chinese_media/posus.Rdata")

#now making dfs for adj and adj percent
adjfrequs <- data.frame("date_published" = ustestframe3$date_published, "adj_percentage" = ustestframe3$adj_percentage)
adjfrequsone <- data.frame("date_published" = ustestframe3$date_published, "adj" = ustestframe3$adj)

ggplot(aes(x = date_published, y = adj), data = adjfrequsone) + geom_point() + 
  geom_smooth(span = 0.1) + ggtitle("Total Adjective Use in China Daily Articles about US Trade Relations") +
  annotate("text", x = as.Date("2018-05-20"), y = 200, label = "Agreement to end trade war", 
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-07-06"), y = 200, label = "First China-specific tariffs", 
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-08-03"), y = 200, label = "China releases List 3", 
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-08-23"), y = 200, label = "US implements List 2", 
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-09-24"), y = 200, label = "US implements List 3",
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-05-03"), y = 200, label = "Trade talks",
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-04-16"), y = 200, label = "US boycotts ZTE",
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-03-22"), y = 200, label = "US files WTO case",
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-12-02"), y = 200, label = "Temporary truce",
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-02-07"), y = 200, label = "US tariffs on solar panels",
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-12-14"), y = 200, label = "China resumes buying soybeans",
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-06-04"), y = 200, label = "Trade talks",
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-10-25"), y = 200, label = "US and China resume contact",
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2019-02-22"), y = 200, label = "US and China extend talks",
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2019-01-07"), y = 200, label = "Trade talks resume",
           angle = 90, size = 2)


ggplot(aes(x = date_published, y = adj_percentage), data = adjfrequs) + geom_point() +
  geom_smooth(span = 0.1) + ggtitle("Adjective Use in China Daily Articles about US Trade Relations as ADJ/Total Words") +
  annotate("text", x = as.Date("2018-05-20"), y = 0.15, label = "Agreement to end trade war", 
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-07-06"), y = 0.15, label = "First China-specific tariffs", 
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-08-03"), y = 0.15, label = "China releases List 3", 
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-08-23"), y = 0.15, label = "US implements List 2", 
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-09-24"), y = 0.15, label = "US implements List 3",
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-05-03"), y = 0.15, label = "Trade talks",
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-04-16"), y = 0.15, label = "US boycotts ZTE",
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-03-22"), y = 0.15, label = "US files WTO case",
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-12-02"), y = 0.15, label = "Temporary truce",
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-02-07"), y = 0.15, label = "US tariffs on solar panels",
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-12-14"), y = 0.15, label = "China resumes buying soybeans",
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-06-04"), y = 0.15, label = "Trade talks",
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-10-25"), y = 0.15, label = "US and China resume contact",
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2019-02-22"), y = 0.15, label = "US and China extend talks",
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2019-01-07"), y = 0.15, label = "Trade talks resume",
           angle = 90, size = 2)


###now let's just do raw word count

totalus <- data.frame("date_published" = ustestframe3$date_published, "word_count" = ustestframe3$total_words)
totalnk <- data.frame("date_published" = testframe5$date_published, "word_count" = testframe5$total_words)

ggplot(aes(x = date_published, y = word_count), data = totalus) + geom_point() +
  geom_smooth(span = 0.1) + ggtitle("Word Count in China Daily Articles about US Trade Relations") +
  ylim(0,2050) +
  annotate("text", x = as.Date("2018-05-20"), y = 1800, label = "Agreement to end trade war", 
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-07-06"), y = 1800, label = "First China-specific tariffs", 
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-08-03"), y = 1800, label = "China releases List 3", 
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-08-23"), y = 1800, label = "US implements List 2", 
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-09-24"), y = 1800, label = "US implements List 3",
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-05-03"), y = 1800, label = "Trade talks",
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-04-16"), y = 1800, label = "US boycotts ZTE",
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-03-22"), y = 1800, label = "US files WTO case",
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-12-02"), y = 1800, label = "Temporary truce",
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-02-07"), y = 1800, label = "US tariffs on solar panels",
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-12-14"), y = 1800, label = "China resumes buying soybeans",
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-06-04"), y = 1800, label = "Trade talks",
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-10-25"), y = 1800, label = "US and China resume contact",
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2019-02-22"), y = 1800, label = "US and China extend talks",
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2019-01-07"), y = 1800, label = "Trade talks resume",
           angle = 90, size = 2)


ggplot(aes(x = date_published, y = word_count), data = totalnk) + geom_point() +
  geom_smooth(span = 0.1) + ggtitle("Word Count in China Daily Articles about North Korea") +
  ylim(0,1200) +
  annotate("text", x = as.Date("2017-11-20"), y = 1000, label = "Trump re-lists NK as State Sponsor of Terrorism", 
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-03-08"), y = 1100, label = "Trump accepts Kim meeting", 
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-05-24"), y = 1100, label = "Trump cancels", 
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-06-12"), y = 1150, label = "Singapore summit", 
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-12-20"), y = 1100, label = "NK won't denuclearize",
           angle = 90, size = 2)+
  annotate("text", x = as.Date("2017-06-04"), y = 1100, label = "NK ICBM Test",
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-09-25"), y = 1100, label = "US reports NK stops testing",
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-06-01"), y = 1100, label = "Trump reverses cancellation",
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2017-08-08"), y = 1050, label = "Trump fire and fury comments",
           angle = 90, size = 2) 


