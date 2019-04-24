rm(list=ls(all=TRUE))

library(rvest)
library(devtools)
library(dplyr)
library(xml2)
library(gsubfn)

setwd("C:/Users/irene/Documents/GitHub/chinese_media")


#oped
op_ed_pages <- c("http://www.chinadaily.com.cn/opinion/op-ed",
                 paste0("http://www.chinadaily.com.cn/opinion/op-ed/page_", 1:85,".html"))

op_ed_urls <- NULL
for(i in op_ed_pages) {
  webpage <- read_html(i)
  temp <- grep("/201", html_attr(html_nodes(webpage, "a"), "href"), value = T)
  op_ed_urls <- append(op_ed_urls, temp)
}
op_ed_urls <- gsub("//www", "http://www", op_ed_urls)

#fix for bug in original code. making sure each article is only there once
op_ed_urls <- unique(op_ed_urls, incomparables = FALSE)

op_ed_data <- read.csv("data/inputData/china_daily_pre_data.csv", stringsAsFactors = F)

for(i in 1:length(op_ed_urls)) {
  article <- read_html(op_ed_urls[i])
  title <- as.character(html_text(html_nodes(article, xpath = '//*[@id="lft-art"]/h1')))
  date_published <- as.character(html_text(html_nodes(article, ".info_l")))
  text <- as.character(html_text(html_nodes(article, xpath = '//*[@id="Content"]')))
  
  op_ed_data <- rbind(op_ed_data, c(title, date_published, text))
  
  if(i%%100==0) {cat(i, "of", length(op_ed_urls), "\n")}
}

op_ed_data <- op_ed_data[-1, ]
op_ed_data$text <- gsub("\\n", " ", op_ed_data$text)

op_ed_data$date_published <- strapplyc(op_ed_data$date_published, "\\d+-\\d+-\\d+", simplify = TRUE)
op_ed_data <- apply(op_ed_data,2,as.character)
write.csv(op_ed_data, "C:/Users/irene/Documents/GitHub/chinese_media/china_daily_oped.csv", row.names = F)


#columnists
columnist_pages <- c("http://www.chinadaily.com.cn/opinion/columnists/fujing",
                     paste0("http://www.chinadaily.com.cn/opinion/columnists/fujing/page_", 1:4,".html"),
                     "http://www.chinadaily.com.cn/opinion/columnists/zhaohuanxin",
                     paste0("http://www.chinadaily.com.cn/opinion/columnists/zhaohuanxin/page_", 1:3,".html"),
                     "http://www.chinadaily.com.cn/opinion/columnists/chenweihua",
                     paste0("http://www.chinadaily.com.cn/opinion/columnists/chenweihua/page_", 1:17,".html"),
                     "http://www.chinadaily.com.cn/opinion/columnists/wanghui")

columnist_urls <- NULL
for(i in columnist_pages) {
  webpage <- read_html(i)
  temp <- grep("/201", html_attr(html_nodes(webpage, "a"), "href"), value = T)
  columnist_urls <- append(columnist_urls, temp)
}

columnist_urls <- gsub("//www", "http://www", columnist_urls)
columnist_urls <- unique(columnist_urls, incomparables = FALSE)

columnist_data <- read.csv("data/inputData/china_daily_pre_data.csv", stringsAsFactors = F)

for(i in 1:length(columnist_urls)) {
  article <- read_html(columnist_urls[i])
  title <- as.character(html_text(html_nodes(article, xpath = '//*[@id="lft-art"]/h1')))
  date_published <- as.character(html_text(html_nodes(article, ".info_l")))
  text <- as.character(html_text(html_nodes(article, xpath = '//*[@id="Content"]')))
  
  columnist_data <- rbind(columnist_data, c(title, date_published, text))
  
  if(i%%100==0) {cat(i, "of", length(columnist_urls), "\n")}
}

columnist_data <- columnist_data[-1, ]
columnist_data$text <- gsub("\\n", " ", columnist_data$text)

columnist_data$date_published <- strapplyc(columnist_data$date_published, "\\d+-\\d+-\\d+", simplify = TRUE)
columnist_data <- apply(columnist_data,2,as.character)
write.csv(columnist_data, "C:/Users/irene/Documents/GitHub/chinese_media/china_daily_columnist.csv", row.names = F)


#News

# make sure the page numbers are up to date
news_pages <- c(paste0("http://www.chinadaily.com.cn/china/governmentandpolicy", 1:205, ".html"),
                paste0("http://www.chinadaily.com.cn/world/asia_pacific/page_", 1:168, ".html"),
                paste0("http://www.chinadaily.com.cn/world/america", 1:116, ".html"),
                paste0("http://www.chinadaily.com.cn/world/china-us", 1:265, ".html"))

news_urls <- NULL
for(i in news_pages) {
  webpage <- read_html(i)
  temp <- grep("/201", html_attr(html_nodes(webpage, "a"), "href"), value = T)
  news_urls <- append(news_urls, temp)
}
news_urls <- gsub("//www", "http://www", news_urls)
news_urls <- unique(news_urls, incomparables = FALSE)

news_data <- read.csv("data/inputData/china_daily_pre_data.csv", stringsAsFactors = F)

for(i in 1:length(news_urls)) {
  article <- read_html(news_urls[i])
  title <- as.character(html_text(html_nodes(article, xpath = '//*[@id="lft-art"]/h1')))
  date_published <- as.character(html_text(html_nodes(article, ".info_l")))
  text <- as.character(html_text(html_nodes(article, xpath = '//*[@id="Content"]')))
  
  news_data <- rbind(news_data, c(title, date_published, text))
  
  if(i%%100==0) {cat(i, "of", length(news_urls), "\n")}
}


news_data <- news_data[-1, ]
news_data$text <- gsub("\\n", " ", news_data$text)

news_data$date_published <- strapplyc(news_data$date_published, "\\d+-\\d+-\\d+", simplify = TRUE)
news_data <- apply(news_data,2,as.character)

news_data <- news_data[!news_data$date_published == "character(0)", ]
news_data <- read.csv("C:/Users/irene/Documents/GitHub/chinese_media/china_daily_news.csv", stringsAsFactors = F)
write.csv(news_data, "C:/Users/irene/Documents/GitHub/chinese_media/china_daily_news.csv", row.names = F)

