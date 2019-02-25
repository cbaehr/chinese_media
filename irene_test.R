rm(list=ls(all=TRUE))

library(rvest)

setwd("C:/Users/irene/Documents/GitHub/chinese_media")

### Editorials

#only doing pg 1-2
editorial_pages <- c("http://www.chinadaily.com.cn/opinion/editionals",
                     paste0("http://www.chinadaily.com.cn/opinion/editionals/page_", 1:38,".html"))

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

editorial_data <- unique(editorial_data, incomparables = FALSE)

editorial_data <- editorial_data[-1, ]

test_data <- editorial_data[grep("North Korea", editorial_data$text), ]

test_data$text <- gsub("\\n", " ", test_data$text)
test_data <- as.data.frame(test_data)
#editorial_data <- as.data.frame(editorial_data)

#write.csv(editorial_data, "data/inputData/irene_test2_raw_china_daily_editorial_data.csv", row.names = F)
