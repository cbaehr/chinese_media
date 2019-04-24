rm(list=ls(all=TRUE))

library(rvest)
library(devtools)
library(dplyr)
library(stringr)
library(xml2)
library(openxlsx)

#set this to your own wd
install.packages("dplyr")
install.packages("openxlsx")
install.packages("stringr")
file.edit('.Rprofile')

setwd("C:/Users/irene/Documents/GitHub/chinese_media")

load("C:/Users/irene/Documents/GitHub/chinese_media/peoples_daily.RData")


#text_data[1,4] <- gsub("\n", "", text_data[1,4])
#text_data[1,4] <- gsub("\t", "", text_data[1,4])
text_data$text <- gsub("\n", "", text_data$text)
text_data$text <- gsub("\t", "", text_data$text)
text_data <- text_data[!text_data$date == "error", ]

#text_data[6,2] <- substr(text_data[6,2], 0, 11)
text_data$date <- substr(text_data$date, 0, 11)

#text_data_2$title <- unique(text_data$title, incomparables = FALSE)
text_data_2 <- text_data[!duplicated(text_data$title), ]

write.csv(text_data_2, "C:/Users/irene/Documents/GitHub/chinese_media/people_daily_clean.csv", 
          fileEncoding = "UTF-8", row.names = F)



