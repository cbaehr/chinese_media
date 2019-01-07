library(rvest)

setwd("/Users/christianbaehr/GitHub/chinese_media")

### Editorials

editorial_pages <- c("http://www.chinadaily.com.cn/opinion/editionals",
          paste0("http://www.chinadaily.com.cn/opinion/editionals/page_", 2:36,".html"))

editorial_urls <- NULL
for(i in editorial_pages) {
  webpage <- read_html(i)
  temp <- grep("/201", html_attr(html_nodes(webpage, "a"), "href"), value = T)
  editorial_urls <- append(editorial_urls, temp)
}
editorial_urls <- gsub("//www", "http://www", editorial_urls)

editorial_data <- read.csv("data/inputData/china_daily_pre_data.csv", stringsAsFactors = F)

for(i in 1:length(editorial_urls)) {
  article <- read_html(editorial_urls[i])
  title <- as.character(html_text(html_nodes(article, xpath = '//*[@id="lft-art"]/h1')))
  date_published <- as.character(html_text(html_nodes(article, ".info_l")))
  text <- as.character(html_text(html_nodes(article, xpath = '//*[@id="Content"]')))

  editorial_data <- rbind(editorial_data, c(title, date_published, text))

  if(i%%100==0) {cat(i, "of", length(editorial_urls), "\n")}
}
# write.csv(editorial_data, "data/inputData/raw_china_daily_editorial_data.csv", row.names = F)

### Op-Eds

op_ed_pages <- c("http://www.chinadaily.com.cn/opinion/op-ed",
          paste0("http://www.chinadaily.com.cn/opinion/op-ed/page_", 2:78,".html"))

op_ed_urls <- NULL
for(i in op_ed_pages) {
  webpage <- read_html(i)
  temp <- grep("/201", html_attr(html_nodes(webpage, "a"), "href"), value = T)
  op_ed_urls <- append(op_ed_urls, temp)
}
op_ed_urls <- gsub("//www", "http://www", op_ed_urls)

op_ed_data <- read.csv("data/inputData/china_daily_pre_data.csv", stringsAsFactors = F)

for(i in 1:length(op_ed_urls)) {
  article <- read_html(op_ed_urls[i])
  title <- as.character(html_text(html_nodes(article, xpath = '//*[@id="lft-art"]/h1')))
  date_published <- as.character(html_text(html_nodes(article, ".info_l")))
  text <- as.character(html_text(html_nodes(article, xpath = '//*[@id="Content"]')))

  op_ed_data <- rbind(op_ed_data, c(title, date_published, text))

  if(i%%100==0) {cat(i, "of", length(op_ed_urls), "\n")}
}
# write.csv(op_ed_data, "data/inputData/raw_china_daily_oped_data.csv", row.names = F)

###Columnists

columnist_pages <- c("http://www.chinadaily.com.cn/opinion/columnists/fujing",
                     paste0("http://www.chinadaily.com.cn/opinion/columnists/fujing/page_", 2:4,".html"),
                     "http://www.chinadaily.com.cn/opinion/columnists/zhaohuanxin",
                     paste0("http://www.chinadaily.com.cn/opinion/columnists/zhaohuanxin/page_", 2:3,".html"),
                     "http://www.chinadaily.com.cn/opinion/columnists/chenweihua",
                     paste0("http://www.chinadaily.com.cn/opinion/columnists/chenweihua/page_", 2:17,".html"),
                     "http://www.chinadaily.com.cn/opinion/columnists/wanghui")

columnist_urls <- NULL
for(i in columnist_pages) {
  webpage <- read_html(i)
  temp <- grep("/201", html_attr(html_nodes(webpage, "a"), "href"), value = T)
  columnist_urls <- append(columnist_urls, temp)
}
columnist_urls <- gsub("//www", "http://www", columnist_urls)

columnist_data <- read.csv("data/inputData/china_daily_pre_data.csv", stringsAsFactors = F)

for(i in 1:length(columnist_urls)) {
  article <- read_html(columnist_urls[i])
  title <- as.character(html_text(html_nodes(article, xpath = '//*[@id="lft-art"]/h1')))
  date_published <- as.character(html_text(html_nodes(article, ".info_l")))
  text <- as.character(html_text(html_nodes(article, xpath = '//*[@id="Content"]')))
  
  columnist_data <- rbind(columnist_data, c(title, date_published, text))
  
  if(i%%100==0) {cat(i, "of", length(columnist_urls), "\n")}
}
# write.csv(columnist_data, "data/inputData/raw_china_daily_columnist_data.csv", row.names = F)

