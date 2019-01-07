
editorial_data <- read.csv("/Users/christianbaehr/Documents/chinese_media/data/inputData/raw_china_daily_editorial_data.csv", stringsAsFactors = F)

editorial_data <- editorial_data[!duplicated(editorial_data),]
editorial_data <- editorial_data[-1,]
rownames(editorial_data) <- seq(1, nrow(editorial_data), 1)

which(!grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}", editorial_data$date_published))
# 146, 160, 188, 204, 209, 273, 307

editorial_data[146,]
editorial_data$text[146] <- editorial_data$date_published[146]
editorial_data$date_published[146] <- editorial_data$title[146]
editorial_data$title[146] <- "China will outlast tariff war and emerge stronger: China Daily editorial"

editorial_data[160,]
editorial_data$text[160] <- editorial_data$date_published[160]
editorial_data$date_published[160] <- editorial_data$title[160]
editorial_data$title[160] <- "The West once again gets it wrong on China"

editorial_data[188,]
editorial_data$text[188] <- editorial_data$date_published[188]
editorial_data$date_published[188] <- editorial_data$title[188]
editorial_data$title[188] <- "Only in cloud cuckoo land is a trade war easy to win: China Daily editorial"

editorial_data[204,]
editorial_data$text[204] <- editorial_data$date_published[204]
editorial_data$date_published[204] <- editorial_data$title[204]
editorial_data$title[204] <- "China will not lose heart in pursuit of better world: China Daily editorial"

editorial_data[209,]
editorial_data$text[209] <- editorial_data$date_published[209]
editorial_data$date_published[209] <- editorial_data$title[209]
editorial_data$title[209] <- "Deficits show trade 'win' simply wishful thinking: China Daily editorial"

editorial_data[273,]
editorial_data$text[273] <- editorial_data$date_published[273]
editorial_data$date_published[273] <- editorial_data$title[273]
editorial_data$title[273] <- "New levies on $50b in goods to show Washington policy's price"

editorial_data[307,]
editorial_data$text[307] <- editorial_data$date_published[307]
editorial_data$date_published[307] <- editorial_data$title[307]
editorial_data$title[307] <- "Sino-US agreement benefits both countries and the world: China Daily editorial"

sum(grepl("editorial", editorial_data$title))

editorial_data$text <- gsub("\n", "", editorial_data$text)
editorial_data$text <- trimws(editorial_data$text)

editorial_data$date_published <- gsub("\n", "", editorial_data$date_published)
editorial_data$date_published <- trimws(editorial_data$date_published)
editorial_data$date_published <- regmatches(editorial_data$date_published, regexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}",  editorial_data$date_published))

editorial_data$type <- "editorial"

###

oped_data <- read.csv("/Users/christianbaehr/Documents/chinese_media/data/inputData/raw_china_daily_oped_data.csv", stringsAsFactors = F)

oped_data <- oped_data[!duplicated(oped_data),]
oped_data <- oped_data[-1,]
rownames(oped_data) <- seq(1, nrow(oped_data), 1)

which(!grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}", oped_data$date_published))
# 1488, 1547

oped_data[1488,]
oped_data$text[1488] <- oped_data$date_published[1488]
oped_data$date_published[1488] <- oped_data$title[1488]
oped_data$title[1488] <- "Promoting innovation in space science"

oped_data[1547,]
oped_data$text[1547] <- oped_data$date_published[1547]
oped_data$date_published[1547] <- oped_data$title[1547]
oped_data$title[1547] <- "Xi to explain economic priorities in Davos"

# sum(grepl("editorial", oped_data$title))

oped_data$text <- gsub("\n", "", oped_data$text)
oped_data$text <- trimws(oped_data$text)

oped_data$date_published <- gsub("\n", "", oped_data$date_published)
oped_data$date_published <- trimws(oped_data$date_published)
oped_data$date_published <- regmatches(oped_data$date_published, regexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}",  oped_data$date_published))

oped_data$type <- "op-ed"


