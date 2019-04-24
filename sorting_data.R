rm(list=ls(all=TRUE))
library(rvest)
library(devtools)
library(RDRPOSTagger)
library(tokenizers)
library(dplyr)
library(stringr)
library(xml2)
library(openxlsx)
library(janitor)
library(coreNLP)
library(cleanNLP)
library(RCurl)

setwd("C:/Users/irene/Documents/GitHub/chinese_media")

file.edit('.Rprofile')
#insert this sequence: Sys.setlocale(category = "LC_ALL", locale = "chs")

#people daily, dividing into two parts, trade and nk
#1 = us. 0 = nk
all_data <- read.xlsx("C:/Users/irene/Documents/GitHub/chinese_media/realdata/people_daily/true_people_daily.xlsx")
all_data$date <- excel_numeric_to_date(all_data$date)

nk_data <- all_data[grep("0", all_data$Type),]
us_data <- all_data[grep("1", all_data$Type),]

write.xlsx(nk_data, "C:/Users/irene/Documents/GitHub/chinese_media/realdata/people_daily/people_daily_nk.xlsx")
write.xlsx(us_data, "C:/Users/irene/Documents/GitHub/chinese_media/realdata/people_daily/people_daily_trade.xlsx")

#chinadaily, dividing into trade and nk
all_data_2 <- read.xlsx("C:/Users/irene/Documents/GitHub/chinese_media/realdata/china_daily/china_daily_all_relevant.xlsx")
all_data_2$date_published <- excel_numeric_to_date(all_data_2$date_published)

nk_data_2 <- all_data_2[grep("NK", all_data_2$topic),]
us_data_2 <- all_data_2[grep("US", all_data_2$topic),]

write.xlsx(nk_data_2, "C:/Users/irene/Documents/GitHub/chinese_media/realdata/china_daily/china_daily_nk.xlsx")
write.xlsx(us_data_2, "C:/Users/irene/Documents/GitHub/chinese_media/realdata/china_daily/china_daily_trade.xlsx")
