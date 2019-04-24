rm(list=ls(all=TRUE))

library(rvest)
library(devtools)
library(rJava)
library(RDRPOSTagger)
library(tokenizers)
library(dplyr)
library(xml2)
library(gsubfn)

#install.packages("rvest")
#install.packages("xml2")
install.packages("gsubfn")
setwd("C:/Users/irene/Documents/GitHub/chinese_media")

### Editorials

#only doing pg 1-2
editorial_pages <- c("http://www.chinadaily.com.cn/opinion/editionals",
                     paste0("http://www.chinadaily.com.cn/opinion/editionals/page_", 1:2,".html"))

#making a list of the urls of the articles
#what is "href"?
editorial_urls <- NULL
for(i in editorial_pages) {
  webpage <- read_html(i)
  temp <- grep("/201", html_attr(html_nodes(webpage, "a"), "href"), value = T)
  editorial_urls <- append(editorial_urls, temp)
}

#replacing all the //www in the url with http://www
editorial_urls <- gsub("//www", "http://www", editorial_urls)

editorial_urls <- unique(editorial_urls, incomparables = FALSE)

#creating a table of the input data from the data file
editorial_data <- read.csv("data/inputData/china_daily_pre_data.csv", stringsAsFactors = F)

#making a table with the article, title, date published, and text for every webpage
for(i in 1:length(editorial_urls)) {
  article <- read_html(editorial_urls[i])
  title <- as.character(html_text(html_nodes(article, xpath = '//*[@id="lft-art"]/h1')))
  date_published <- as.character(html_text(html_nodes(article, ".info_l")))
  text <- as.character(html_text(html_nodes(article, xpath = '//*[@id="Content"]')))
  
  editorial_data <- rbind(editorial_data, c(title, date_published, text))
  
  if(i%%100==0) {cat(i, "of", length(editorial_urls), "\n")}
}

#editorial_data <- unique(editorial_data, incomparables = FALSE)

editorial_data <- editorial_data[-1, ]
editorial_data$text <- gsub("\\n", " ", editorial_data$text)


#################################################################################################


editorial_data <- read.csv("C:/Users/irene/Documents/GitHub/chinese_media/irene_china_daily_editorial_data.csv", stringsAsFactors = F)
editorial_data <- editorial_data[-4,]

editorial_data$date_published <- strapplyc(editorial_data$date_published, "\\d+-\\d+-\\d+", simplify = TRUE)
editorial_data <- apply(editorial_data,2,as.character)

write.csv(editorial_data, "C:/Users/irene/Documents/GitHub/chinese_media/china_daily_edit.csv", row.names = F)

################################################################################################
date_test_data <- editorial_data[grep("North Korea|DPRK", editorial_data$text), ]

#toRemove <- grep("North Korea|DPRK", editorial_data$text)
#non_nk_data <- editorial_data[-toRemove, ] 
#non_nk_data <- editorial_data[!grep("North Korea", editorial_data$text), ]
#test_data <- as.data.frame(test_data)
#editorial_data <- as.data.frame(editorial_data)

#write.csv(non_nk_data, "C:/Users/irene/Documents/GitHub/chinese_media/irene_non_nk_data.csv", row.names = F)

#write.csv(test_data, "C:/Users/irene/Documents/GitHub/chinese_media/irene_nk_data.csv", row.names = F)

#non_nk_data <- read.csv("C:/Users/irene/Documents/GitHub/chinese_media/irene_non_nk_data.csv", stringsAsFactors = F)
#test_data <- read.csv("C:/Users/irene/Documents/GitHub/chinese_media/irene_nk_data.csv", stringsAsFactors = F)
