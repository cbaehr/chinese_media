rm(list=ls(all=TRUE))

#install.packages("chinese.misc")
library(chinese.misc)
library(rvest)
library(devtools)
library(tokenizers)
library(dplyr)
library(stringr)
library(xml2)
library(openxlsx)
library(janitor)
library(RCurl)
library(ggplot2)


setwd("C:/Users/irene/Documents/GitHub/chinese_media")

file.edit('.Rprofile')

#reading the people daily nk file
nk_chinese <- read.xlsx("C:/Users/irene/Documents/GitHub/chinese_media/realdata/people_daily/people_daily_nk.xlsx")

#converting it to a date format legible for R
nk_chinese$date <- excel_numeric_to_date(nk_chinese$date)

#ordering files by date
nk_chinese <- nk_chinese[order(as.Date(nk_chinese$date, format="%Y/%m/%d")),]

#here I am just testing the process to make sure it works
#testo <- nk_chinese[1,5]
#testo <- as.character2(testo)

#testa <- seg_file(testo, from = "v")
#testb <- strsplit(testa, " ")

#testadj <- get_tag_word(testb, tag = "a")
#testc <- as.data.frame(testadj)
#testd <- as.data.frame(testb)
#valueadj <- nrow(testc)

#NORTH KOREA
#for every article in the data frame, I am making it into a character vector, segmenting it, splitting these
#segments, tagging them, pulling out the adjectives, making them into a df, and counting the number of the
#rows in the df. Then I add that number to a list of all the adjs that each article has. 
list <- c()
for(i in 1:nrow(nk_chinese)){
  article <- nk_chinese[i,5]
  characters <- as.character2(article)
  segmented <- seg_file(characters, from = "v")
  split <- strsplit(segmented, " ")
  testadj <- get_tag_word(split, tag = "a")
  testc <- as.data.frame(testadj)
  valueadj <- nrow(testc)
  list <- c(list, valueadj)
  i <- i + 1
}

#doing the same but not pulling out adjectives, just counting words
list2 <- c()
for(i in 1:nrow(nk_chinese)){
  article <- nk_chinese[i,5]
  characters <- as.character2(article)
  segmented <- seg_file(characters, from = "v")
  split <- strsplit(segmented, " ")
  testc <- as.data.frame(split)
  wordcount <- nrow(testc)
  list2 <- c(list2, wordcount)
  i <- i + 1
}

#now adding these lists to the old data frame as columns                                       
testframe <- nk_chinese
testframe2 <- cbind(testframe,list)
colnames(testframe2)[colnames(testframe2)=="list"] <- "adj"

testframe3 <- cbind(testframe2, list2)
colnames(testframe3)[colnames(testframe3)=="list2"] <- "total_words"

#now adding a column which is adj/total words by first making a list, then adding that list to the df
percentlistnk <- c()
for(i in 1:nrow(testframe3)){
  percent <- testframe3[i,7]/testframe3[i,8]
  percentlistnk <- c(percentlistnk, percent)
  i <- i + 1
}

finalframe <- cbind(testframe3, percentlistnk)
colnames(finalframe)[colnames(finalframe)=="percentlistnk"] <- "percent_adj"

#save the data here because it takes a long time to process and it's faster to just load the saved data frame for graphing
save(finalframe, file = "C:/Users/irene/Documents/GitHub/chinese_media/pospeopledailynk.Rdata")


file.edit('.Rprofile')
#enter these commands: Sys.setlocale(category = "LC_ALL", locale = "chs")

#Sys.setlocale("LC_TIME", "English")


#making data frames with date and adj, date and adj percent, date and total words
adjfreq <- data.frame("date_published" = testframe2$date, "adj" = testframe2$adj)
adjpercent <- data.frame("date_published" = finalframe$date, "adj_percent" = finalframe$percent_adj)
totalframe <- data.frame("date_published" = finalframe$date, "total_words" = finalframe$total_words)


#creating plots with date on x axis, tokens on y axis, major events written in.
ggplot(aes(x = date_published, y = adj, ymin = 0), data = adjfreq) + geom_point() +
  geom_smooth(span = 0.4) + ggtitle("Total Adjective Use in People's Daily Articles about North Korea 2018-2019") +
  annotate("text", x = as.Date("2018-12-20"), y = 85, label = "NK won't denuclearize",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-09-25"), y = 70, label = "NK stops testing, US maintains sanctions",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2019-02-27"), y = 70, label = "Hanoi Summit",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-11-01"), y = 70, label = "NK demands lifting of sanctions",
           angle = 90, size = 3) 

ggplot(aes(x = date_published, y = adj_percent, ymax = 0.07), data = adjpercent) + geom_point() +
  geom_smooth(span = 0.4) + ggtitle("Adjective Use in People's Daily Articles about North Korea 2018-2019 as ADJ/Total Words") +
  annotate("text", x = as.Date("2018-12-20"), y = .05, label = "NK won't denuclearize",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-09-25"), y = .05, label = "NK stops testing, US maintains sanctions",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2019-02-27"), y = .05, label = "Hanoi Summit",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-11-01"), y = .05, label = "NK demands lifting of sanctions",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-09-18"), y = .05, label = "Inter-Korean Summit",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-08-03"), y = .05, label = "Letter from Kim to Trump",
           angle = 90, size = 3) 

ggplot(aes(x = date_published, y = total_words), data = totalframe) + geom_point() +
  geom_smooth(span = 0.4) + ggtitle("Word Count in People's Daily Articles about North Korea 2018-2019") +
  annotate("text", x = as.Date("2018-12-20"), y = 2500, label = "NK won't denuclearize",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-09-25"), y = 2500, label = "NK stops testing, US maintains sanctions",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2019-02-27"), y = 2500, label = "Hanoi Summit",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-11-01"), y = 2500, label = "NK demands lifting of sanctions",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-09-18"), y = 2500, label = "Inter-Korean Summit",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-08-03"), y = 2500, label = "Letter from Kim to Trump",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-12-13"), y = 2500, label = "Inter-Korean Groundbreaking Rail Ceremony",
           angle = 90, size = 3) 


##US TRADE this is the same as above
us_chinese <- read.xlsx("C:/Users/irene/Documents/GitHub/chinese_media/realdata/people_daily/people_daily_trade.xlsx")
us_chinese$date <- excel_numeric_to_date(us_chinese$date)
us_chinese <- us_chinese[order(as.Date(us_chinese$date, format="%Y/%m/%d")),]

adj <- c()
for(i in 1:nrow(us_chinese)){
  article <- us_chinese[i,5]
  characters <- as.character2(article)
  segmented <- seg_file(characters, from = "v")
  split <- strsplit(segmented, " ")
  testadj <- get_tag_word(split, tag = "a")
  testc <- as.data.frame(testadj)
  valueadj <- nrow(testc)
  adj <- c(adj, valueadj)
  i <- i + 1
}


total_words <- c()
for(i in 1:nrow(us_chinese)){
  article <- us_chinese[i,5]
  characters <- as.character2(article)
  segmented <- seg_file(characters, from = "v")
  split <- strsplit(segmented, " ")
  testc <- as.data.frame(split)
  wordcount <- nrow(testc)
  total_words <- c(total_words, wordcount)
  i <- i + 1
}

testframe <- us_chinese
testframe2 <- cbind(testframe, adj, total_words)

percent_adj <- c()
for(i in 1:nrow(testframe2)){
  percent <- testframe2[i,7]/testframe2[i,8]
  percent_adj <- c(percent_adj, percent)
  i <- i + 1
}


finalframe <- cbind(testframe2, percent_adj)

adjfreq <- data.frame("date_published" = finalframe$date, "adj" = finalframe$adj)
adjpercent <- data.frame("date_published" = finalframe$date, "adj_percent" = finalframe$percent_adj)
totalframe <- data.frame("date_published" = finalframe$date, "total_words" = finalframe$total_words)

ggplot(aes(x = date_published, y = adj, ymin = 0), data = adjfreq) + geom_point() +
  geom_smooth(span = 0.4) + ggtitle("Total Adjective Use in People's Daily Articles about US Trade Relations 2018-2019") +
  annotate("text", x = as.Date("2018-07-06"), y = 100, label = "First China-specific tariffs", 
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-08-03"), y = 100, label = "China releases List 3", 
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-08-23"), y = 100, label = "US implements List 2", 
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-09-24"), y = 100, label = "US implements List 3",
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-12-02"), y = 100, label = "Temporary truce",
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-12-14"), y = 100, label = "China resumes buying soybeans",
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2018-10-25"), y = 100, label = "US and China resume contact",
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2019-02-22"), y = 100, label = "US and China extend talks",
           angle = 90, size = 2) +
  annotate("text", x = as.Date("2019-01-07"), y = 100, label = "Trade talks resume",
           angle = 90, size = 2)


ggplot(aes(x = date_published, y = adj_percent, ymax = 0.07), data = adjpercent) + geom_point() +
  geom_smooth(span = 0.4) + ggtitle("Total Adjective Use in People's Daily Articles about US Trade Relations 2018-2019 as ADJ/Total Words") +
  annotate("text", x = as.Date("2018-07-06"), y = 0.05, label = "First China-specific tariffs", 
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-08-03"), y = 0.05, label = "China releases List 3", 
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-08-23"), y = 0.05, label = "US implements List 2", 
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-09-24"), y = 0.05, label = "US implements List 3",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-12-02"), y = 0.04, label = "Temporary truce",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-12-14"), y = 0.03, label = "China resumes buying soybeans",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-10-25"), y = 0.05, label = "US and China resume contact",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2019-02-22"), y = 0.05, label = "US and China extend talks",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2019-01-07"), y = 0.05, label = "Trade talks resume",
           angle = 90, size = 3)


ggplot(aes(x = date_published, y = total_words, ymin = 0), data = totalframe) + geom_point() +
  geom_smooth(span = 0.5) + ggtitle("Word Count in People's Daily Articles about US Trade Relations 2018-2019") +
  annotate("text", x = as.Date("2018-07-06"), y = 5000, label = "First China-specific tariffs", 
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-08-03"), y = 5000, label = "China releases List 3", 
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-08-23"), y = 5000, label = "US implements List 2", 
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-09-24"), y = 5000, label = "US implements List 3",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-12-02"), y = 5000, label = "Temporary truce",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-12-14"), y = 5000, label = "China resumes buying soybeans",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-10-25"), y = 5000, label = "US and China resume contact",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2019-02-22"), y = 5000, label = "US and China extend talks",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2019-01-07"), y = 5000, label = "Trade talks resume",
           angle = 90, size = 3)

