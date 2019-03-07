rm(list=ls(all=TRUE))

library(rvest)
library(devtools)
library(dplyr)
library(xml2)

#set this to your own wd
setwd("C:/Users/irene/Documents/GitHub/chinese_media")

#now we have to set the language. when i first scraped, all the characters appeared in unicode. This may not happen 
#on your computer, but if it does run the following code:
file.edit('.Rprofile')
#and paste this in the window that opens up
#Sys.setlocale(category = "LC_ALL", locale = "chs")
#then come back to this file, go to File --> Save with Encoding --> and select Chinese. This should fix the problem!

###May 1 - September 30 2018
#international/foreign trade and relations, the DPRK, and the US
#look at the url for your time period. The page numbers increase by 12. find the first and last
#numbers for your page. in mine, they're 192 to 360.
#?start=192 ?start=360

wechat_pages <- c("https://chuansongme.com/account/xiake_island",
                     paste0("https://chuansongme.com/account/xiake_island?start=", seq(from = 192, to = 360, by = 12)))

#making a list of the urls of the articles
wechat_urls <- NULL
for(i in wechat_pages) {
  page <- read_html(i)
  tempo <- grep("/2", html_attr(html_nodes(page, "a"), "href"), value = T)
  wechat_urls <- append(wechat_urls, tempo)
}


#adding the first part of the url to the second part...
wechat_urls <- gsub("/n", "https://chuansongme.com/n", wechat_urls)

#creating a table of the input data from the dummy data file
wechat_data <- read.csv("data/inputData/china_daily_pre_data.csv", stringsAsFactors = F)

#making a table with the article, title, date published, and text for every webpage
for(i in 1:length(wechat_urls)) {
  article <- read_html(wechat_urls[i])
  title <- as.character(html_text(html_nodes(article, xpath = '//*[@id="img-content"]/h2')))
  date_published <- as.character(html_text(html_nodes(article, xpath = '//*[@id="publish_time"]')))
  text <- as.character(html_text(html_nodes(article, xpath = '//*[@id="js_content"]')))
  
  wechat_data <- rbind(wechat_data, c(title, date_published, text))
  
  #if(i%%100==0) {cat(i, "of", length(wechat_urls), "\n")}
}

#i just used this to test out a page before i ran the whole thing. it's a very slow process.
#test2 <- read_html("https://chuansongme.com/n/2314746344623")
#as.character(html_text(html_nodes(test2, xpath = '//*[@id="img-content"]/h2')))
#date_published <- as.character(html_text(html_nodes(test2, xpath = '//*[@id="publish_time"]')))
#text <- as.character(html_text(html_nodes(test2, xpath = '//*[@id="js_content"]')))
#test_data <- rbind(wechat_data, c(title, date_published, text))


wechat_data <- wechat_data[-1, ]
wechat_data$text <- gsub("\n", "", wechat_data$text)

#write this to your own file. If there's stubborn unicode, you will have to go through and delete it in
#another program.
write.csv(wechat_data, "C:/Users/irene/Documents/GitHub/chinese_media/irene_wechat_data.csv", row.names = F)