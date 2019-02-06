
setwd("/Users/christianbaehr/GitHub/chinese_media")

data <- read.csv("data/ProcessedData/china_daily_data.csv", stringsAsFactors = F)

data$clean_text <- str_split(gsub("[^[:alnum:] ]", "", data$text), " +")
data$clean_text2 <- lapply(data$clean_text, function(x) {x[!(x %in% stop_words$word)]})

a <- sort(table(unlist(data$clean_text)), decreasing = T)
b <- sort(table(unlist(data$clean_text2)), decreasing = T)
a[!(names(a) %in% names(b))]


sum(data$date_published=="2017-6-30")


sort(table(unlist(data$clean_text[data$date_published=="2017-6-30"])))
sort(table(unlist(data$clean_text2)), decreasing = T)
sort(table(unlist(data$clean_text2[data$date_published=="2017-6-30"])))

new_data <- as.data.frame(sort(unique(data$date_published)), stringsAsFactors = F)
names(new_data) <- "date"
new_data$date <- as.Date(new_data$date)

new_data$unfair_frequency <- NA
new_data$trade_frequency <- NA
for(i in 1:nrow(new_data)) {
  
  new_data$unfair_frequency[i] <- table(unlist(data$clean_text2[data$date_published==new_data$date[i]]))["unfair"]
  new_data$trade_frequency[i] <- table(unlist(data$clean_text2[data$date_published==new_data$date[i]]))["trade"]
}
new_data$unfair_frequency <- ifelse(is.na(new_data$unfair_frequency), 0, new_data$unfair_frequency)
new_data$trade_frequency <- ifelse(is.na(new_data$trade_frequency), 0, new_data$trade_frequency)

library(data.table)
new_data2 <- setDT(new_data)[, .(mn_amt = mean(trade_frequency)), by = .(yr = year(date), mon = months(date))]

new_data2$mon <- match(new_data2$mon, month.name)

new_data2$date <- as.Date(do.call(paste, list(new_data2$yr, new_data2$mon, "01", sep = "-")), 
                          format = "%Y-%m-%d")

plot(new_data2$date, new_data2$mn_amt)


###################

sum(grepl("North Korea", data$title))
sum(grepl("North Korea", data$text))

sum(grepl("DPRK", data$title))
sum(grepl("DPRK", data$text))

sum(grepl("Kim", data$title))
sum(grepl("Kim", data$text))


nk_data <- data[grepl("DPRK", data$text) | grepl("North Korea", data$text),]
non_nk_data <- data[!grepl("DPRK", data$text) | !grepl("North Korea", data$text),]
sum(grepl("DPRK", data$title) | grepl("North Korea", data$title))

library(tidyverse)
library(tidytext)
a=as.data.frame(get_sentiments("afinn"))

get_sentiments(nk_data$text[1])



