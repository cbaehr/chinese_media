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

#SEE FILE people_daily_adj_analysis.R FOR A FULL EXPLANATION OF THE CODE. IT'S THE SAME THING

setwd("C:/Users/irene/Documents/GitHub/chinese_media")

file.edit('.Rprofile')

#NORTH KOREA

nk_xiake <- read.xlsx("C:/Users/irene/Documents/GitHub/chinese_media/realdata/xiakedao/xiake_nk.xlsx")
nk_xiake$date_published <- excel_numeric_to_date(nk_xiake$date_published)
nk_xiake <- nk_xiake[order(as.Date(nk_xiake$date_published, format="%Y/%m/%d")),]


adj <- c()
for(i in 1:nrow(nk_xiake)){
  article <- nk_xiake[i,3]
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
for(i in 1:nrow(nk_xiake)){
  article <- nk_xiake[i,3]
  characters <- as.character2(article)
  segmented <- seg_file(characters, from = "v")
  split <- strsplit(segmented, " ")
  testc <- as.data.frame(split)
  wordcount <- nrow(testc)
  total_words <- c(total_words, wordcount)
  i <- i + 1
}

testframe <- nk_xiake
testframe2 <- cbind(testframe, adj, total_words)

percent_adj <- c()
for(i in 1:nrow(testframe2)){
  percent <- testframe2[i,4]/testframe2[i,5]
  percent_adj <- c(percent_adj, percent)
  i <- i + 1
}


finalframe <- cbind(testframe2, percent_adj)

adjfreq <- data.frame("date_published" = finalframe$date, "adj" = finalframe$adj)
adjpercent <- data.frame("date_published" = finalframe$date, "adj_percent" = finalframe$percent_adj)
totalframe <- data.frame("date_published" = finalframe$date, "total_words" = finalframe$total_words)

ggplot(aes(x = date_published, y = adj, ymin = 0), data = adjfreq) + geom_point() +
  geom_smooth(span = 0.4) + ggtitle("Total Adjective Use in Xiakedao Articles about North Korea") +
  annotate("text", x = as.Date("2019-02-27"), y = 60, label = "Hanoi Summit",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-11-01"), y = 60, label = "NK demands lifting of sanctions",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-09-18"), y = 60, label = "Inter-Korean Summit",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-08-03"), y = 60, label = "Letter from Kim to Trump",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-12-13"), y = 55, label = "Inter-Korean Groundbreaking Rail Ceremony",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2017-11-20"), y = 55, label = "Trump re-lists NK as State Sponsor of Terrorism", 
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-03-08"), y = 60, label = "Trump accepts Kim meeting", 
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-05-24"), y = 60, label = "Trump cancels", 
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-06-12"), y = 60, label = "Singapore summit", 
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-12-25"), y = 60, label = "NK won't denuclearize",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2017-09-03"), y = 60, label = "NK H-Bomb Test",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2017-06-04"), y = 60, label = "NK ICBM Test",
           angle = 90, size = 3)

ggplot(aes(x = date_published, y = adj_percent, ymax = 0.07), data = adjpercent) + geom_point() +
  geom_smooth(span = 0.4) + ggtitle("Total Adjective Use in Xiakedao Articles about North Korea as ADJ/Total Words")  +
  annotate("text", x = as.Date("2019-02-27"), y = 0.05, label = "Hanoi Summit",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-11-01"), y = 0.05, label = "NK demands lifting of sanctions",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-09-18"), y = 0.05, label = "Inter-Korean Summit",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-08-03"), y = 0.05, label = "Letter from Kim to Trump",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-12-13"), y = 0.05, label = "Inter-Korean Groundbreaking Rail Ceremony",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2017-11-20"), y = 0.05, label = "Trump re-lists NK as State Sponsor of Terrorism", 
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-03-08"), y = 0.05, label = "Trump accepts Kim meeting", 
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-05-24"), y = 0.05, label = "Trump cancels", 
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-06-12"), y = 0.05, label = "Singapore summit", 
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-12-25"), y = 0.05, label = "NK won't denuclearize",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2017-09-03"), y = 0.05, label = "NK H-Bomb Test",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2017-06-04"), y = 0.05, label = "NK ICBM Test",
           angle = 90, size = 3)

ggplot(aes(x = date_published, y = total_words), data = totalframe) + geom_point() +
  geom_smooth(span = 0.4) + ggtitle("Word Count in Xiakedao Articles about North Korea") +
  annotate("text", x = as.Date("2019-02-27"), y = 2500, label = "Hanoi Summit",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-11-01"), y = 2500, label = "NK demands lifting of sanctions",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-09-18"), y = 2500, label = "Inter-Korean Summit",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-08-03"), y = 2500, label = "Letter from Kim to Trump",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-12-13"), y = 2400, label = "Inter-Korean Groundbreaking Rail Ceremony",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2017-11-20"), y = 2350, label = "Trump re-lists NK as State Sponsor of Terrorism", 
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-03-08"), y = 2500, label = "Trump accepts Kim meeting", 
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-05-24"), y = 2500, label = "Trump cancels", 
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-06-12"), y = 2500, label = "Singapore summit", 
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-12-25"), y = 2500, label = "NK won't denuclearize",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2017-09-03"), y = 2500, label = "NK H-Bomb Test",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2017-06-04"), y = 2500, label = "NK ICBM Test",
           angle = 90, size = 3)



#US TRADE
us_xiake <- read.xlsx("C:/Users/irene/Documents/GitHub/chinese_media/realdata/xiakedao/xiake_trade.xlsx")
us_xiake$date_published <- excel_numeric_to_date(us_xiake$date_published)
us_xiake <- us_xiake[order(as.Date(us_xiake$date_published, format="%Y/%m/%d")),]


adj <- c()
for(i in 1:nrow(us_xiake)){
  article <- us_xiake[i,3]
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
for(i in 1:nrow(us_xiake)){
  article <- us_xiake[i,3]
  characters <- as.character2(article)
  segmented <- seg_file(characters, from = "v")
  split <- strsplit(segmented, " ")
  testc <- as.data.frame(split)
  wordcount <- nrow(testc)
  total_words <- c(total_words, wordcount)
  i <- i + 1
}

testframe <- us_xiake
testframe2 <- cbind(testframe, adj, total_words)

percent_adj <- c()
for(i in 1:nrow(testframe2)){
  percent <- testframe2[i,4]/testframe2[i,5]
  percent_adj <- c(percent_adj, percent)
  i <- i + 1
}


finalframe <- cbind(testframe2, percent_adj)

adjfreq <- data.frame("date_published" = finalframe$date, "adj" = finalframe$adj)
adjpercent <- data.frame("date_published" = finalframe$date, "adj_percent" = finalframe$percent_adj)
totalframe <- data.frame("date_published" = finalframe$date, "total_words" = finalframe$total_words)

ggplot(aes(x = date_published, y = adj, ymin = 0), data = adjfreq) + geom_point() +
  geom_smooth(span = 0.4) + ggtitle("Total Adjective Use in Xiakedao Articles about US Trade Relations") +
  annotate("text", x = as.Date("2018-05-20"), y = 70, label = "Agreement to end trade war", 
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-07-06"), y = 70, label = "First China-specific tariffs", 
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-08-03"), y = 70, label = "China releases List 3", 
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-08-23"), y = 70, label = "US implements List 2", 
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-09-24"), y = 70, label = "US implements List 3",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-05-03"), y = 70, label = "Trade talks",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-04-16"), y = 70, label = "US boycotts ZTE",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-03-22"), y = 70, label = "US files WTO case",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-12-02"), y = 70, label = "Temporary truce",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-02-07"), y = 70, label = "US tariffs on solar panels",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-12-14"), y = 70, label = "China resumes buying soybeans",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-06-04"), y = 70, label = "Trade talks",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-10-25"), y = 70, label = "US and China resume contact",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2019-02-22"), y = 70, label = "US and China extend talks",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2019-01-07"), y = 70, label = "Trade talks resume",
           angle = 90, size = 3)


ggplot(aes(x = date_published, y = adj_percent, ymax = 0.06), data = adjpercent) + geom_point() +
  geom_smooth(span = 0.4) + ggtitle("Total Adjective Use in Xiakedao Articles about US Trade Relations as ADJ/Total Words") +
  annotate("text", x = as.Date("2018-05-20"), y = 0.05, label = "Agreement to end trade war", 
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-07-06"), y = 0.05, label = "First China-specific tariffs", 
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-08-03"), y = 0.05, label = "China releases List 3", 
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-08-23"), y = 0.05, label = "US implements List 2", 
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-09-24"), y = 0.05, label = "US implements List 3",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-05-03"), y = 0.05, label = "Trade talks",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-04-16"), y = 0.05, label = "US boycotts ZTE",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-03-22"), y = 0.05, label = "US files WTO case",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-12-02"), y = 0.05, label = "Temporary truce",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-02-07"), y = 0.05, label = "US tariffs on solar panels",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-12-14"), y = 0.05, label = "China resumes buying soybeans",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-06-04"), y = 0.05, label = "Trade talks",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-10-25"), y = 0.05, label = "US and China resume contact",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2019-02-22"), y = 0.05, label = "US and China extend talks",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2019-01-07"), y = 0.05, label = "Trade talks resume",
           angle = 90, size = 3)



ggplot(aes(x = date_published, y = total_words, ymin = 0), data = totalframe) + geom_point() +
  geom_smooth(span = 0.5) + ggtitle("Word Count in Xiakedao Articles about US Trade Relations") +
  ylim(0,2050) +
  annotate("text", x = as.Date("2018-05-20"), y = 1800, label = "Agreement to end trade war", 
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-07-06"), y = 1800, label = "First China-specific tariffs", 
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-08-03"), y = 1800, label = "China releases List 3", 
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-08-23"), y = 1800, label = "US implements List 2", 
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-09-24"), y = 1800, label = "US implements List 3",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-05-03"), y = 1800, label = "Trade talks",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-04-16"), y = 1800, label = "US boycotts ZTE",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-03-22"), y = 1800, label = "US files WTO case",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-12-02"), y = 1800, label = "Temporary truce",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-02-07"), y = 1800, label = "US tariffs on solar panels",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-12-14"), y = 1800, label = "China resumes buying soybeans",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-06-04"), y = 1800, label = "Trade talks",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2018-10-25"), y = 1800, label = "US and China resume contact",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2019-02-22"), y = 1800, label = "US and China extend talks",
           angle = 90, size = 3) +
  annotate("text", x = as.Date("2019-01-07"), y = 1800, label = "Trade talks resume",
           angle = 90, size = 3)

