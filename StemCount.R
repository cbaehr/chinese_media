# install.packages("tm")
# install.packages("stringr")
# install.packages("SnowballC")
# install.packages("rJava")
# install.packages("qdap")
# install.packages("ggplot2")
library(rJava)
library(qdap)
library(tm)
library(stringr)
library(SnowballC)
library(data.table)
library(dplyr)
library(ggplot2)
library(grid)


#### ChinaDaily Cleaning ####
chinaDaily_Main <- chinaDaily

chinaDaily_Main <- chinaDaily_Main[order(chinaDaily_Main$date_published),]
chinaDaily_Main$ID <- 1:nrow(chinaDaily_Main)
chinaDaily_Main$date_published <- as.character(chinaDaily_Main$date_published)

chinaDaily <- chinaDaily_Main
chinaDaily$Year <- NA
for(i in 1:nrow(chinaDaily)) {
  str2 <- strsplit(chinaDaily$date_published[i], "-")
  chinaDaily$Year[i] <- str2[[1]][1]
}
chinaDaily <- chinaDaily[,c(5,1,3,4,6,2)]

### ChinaDaily_US ###
cd.loc.us <- gregexpr("US ", chinaDaily$text)
cd.loc.us1 <- vector()
for(i in 1:nrow(chinaDaily)) {
  cd.loc.us1 <- c(cd.loc.us1, cd.loc.us[[i]][1][1])
}
chinaDaily$US <- cd.loc.us1
cd_US <- chinaDaily[which(chinaDaily$US != -1),]

cd.loc.unitedstates <- gregexpr("United States", chinaDaily$text)
cd.loc.unitedstates1 <- vector()
for(i in 1:nrow(chinaDaily)) {
  cd.loc.unitedstates1 <- c(cd.loc.unitedstates1, cd.loc.unitedstates[[i]][1][1])
}
chinaDaily$UnitedStates <- cd.loc.unitedstates1
cd_US <- chinaDaily[which(chinaDaily$US != -1),]
cd_US <- rbind(cd_US, chinaDaily[which(chinaDaily$UnitedStates != -1 & (chinaDaily$ID %in% cd_US$ID) == F),])
cd_US$text <- toupper(cd_US$text)

### ChinaDaily_US_Trade ###
cd.loc.us.trade <- gregexpr("TRADE", cd_US$text)
cd.loc.us.trade1 <- vector()
for(i in 1:nrow(cd_US)) {
  cd.loc.us.trade1 <- c(cd.loc.us.trade1, cd.loc.us.trade[[i]][1][1])
}
cd_US$TRADE <- cd.loc.us.trade1
cd_US_trade <- cd_US[which(cd_US$TRADE != -1),]

cd.loc.us.tariff <- gregexpr("TARIFF", cd_US$text)
cd.loc.us.tariff1 <- vector()
for(i in 1:nrow(cd_US)) {
  cd.loc.us.tariff1 <- c(cd.loc.us.tariff1, cd.loc.us.tariff[[i]][1][1])
}
cd_US$TARIFF <- cd.loc.us.tariff1
cd_US_trade <- cd_US[which(cd_US$TRADE != -1),]
cd_US_trade <- rbind(cd_US_trade, cd_US[which(cd_US$TARIFF != -1 & (cd_US$ID %in% cd_US_trade$ID) == F),])

### Extracting All Words from ChinaDaily_US_Trade from 2018 ###
stops <- stopwords(kind = "en")
stops <- toupper(stops)
cd_US_trade$text2 <- cd_US_trade$text

df1 <- cd_US_trade[which(cd_US_trade$Year == "2018"),]
text1 <- gsub('[[:punct:]]',' ', df1$text2[1])
text1 <- gsub('[[:digit:]]',' ', text1)
text1 <- gsub('  ',' ', text1)
text1 <- unlist(strsplit(text1, " "))
text1 <- gsub(" ","", text1)
text1 <- text1[which((nchar(text1) > 3) == T)]
text1 <- as.data.frame(text1, stringsAsFactors = F)
names(text1)[1] <- "word"
text1$stop <- NA
text1$stop <- text1$word %in% stops == T
text2 <- text1[which(text1$stop == F),]
text2 <- text2[order(text2$word),]
text2$ID <- df1$ID[1]
text2$date <- df1$date_published[1]
text2$year <- 2018
text2 <- text2[,c(3,1,5,4)]

cd_US_trWords18 <- text2

df1 <- cd_US_trade[which(cd_US_trade$Year == "2018"),]
for(i in 2:nrow(df1)) {
  text1 <- gsub('[[:punct:]]',' ', df1$text2[i])
  text1 <- gsub('[[:digit:]]',' ', text1)
  text1 <- gsub('  ',' ', text1)
  text1 <- unlist(strsplit(text1, " "))
  text1 <- gsub(' ','', text1)
  text1 <- text1[which((nchar(text1) > 3) == T)]
  text1 <- as.data.frame(text1, stringsAsFactors = F)
  names(text1)[1] <- "word"
  text1$stop <- NA
  text1$stop <- text1$word %in% stops == T
  text2 <- text1[which(text1$stop == F),]
  text2 <- text2[order(text2$word),]
  text2$ID <- df1$ID[i]
  text2$date <- df1$date_published[i]
  text2$year <- 2018
  text2 <- text2[,c(3,1,5,4)]
  cd_US_trWords18 <- rbind(cd_US_trWords18, text2)
  if(i%%10==0) {cat(i)} else {
    if(i%%2==0) {cat(".")}
  }
  if(i == nrow(df1)) {cat("//", i, "of", i)}
}

# cd_US_trWords18$date2 <- gsub("-","", cd_US_trWords18$date)
# cd_US_trWords18$dateN <- as.numeric(cd_US_trWords18$date2)
# cd_US_trWords18 <- cd_US_trWords18[,-(5:6)]
cd_US_trWords18$date <- as.Date(cd_US_trWords18$date)
length(unique(cd_US_trWords18$word))
unq <- unique(cd_US_trWords18$word)
unq.stem <- stemmer(unq, capitalize = T)

cd_US_trWords18 <- left_join(cd_US_trWords18[,1:4], 
                         data.frame(word = unq, stem = as.character(toupper(unq.stem))))
cd_US_trWords18 <- cd_US_trWords18[11:nrow(cd_US_trWords18),]
cd_US_trWords18.2 <- as.data.frame(sort(unique(cd_US_trWords18$date)), stringsAsFactors = F)
names(cd_US_trWords18.2)[1] <- "date"
cd_US_trWords18.2$words <- NA
cd_US_trWords18.2$stem <- NA
for(i in 1:nrow(cd_US_trWords18.2)) {
  cd_US_trWords18.2$words[i] <- list(cd_US_trWords18$word[which(cd_US_trWords18$date == cd_US_trWords18.2$date[i])])
  cd_US_trWords18.2$stem[i] <- list(cd_US_trWords18$stem[which(cd_US_trWords18$date == cd_US_trWords18.2$date[i])])
}
cd_US_trWords18.2 <- as.data.frame(cd_US_trWords18.2, stringsAsFactors= F)
cd_US_trWords18.2$date <- as.Date(cd_US_trWords18.2$date)
cd_US_trWords18.2$wk <- week(cd_US_trWords18.2$date)
cd_US_trWords18.2$freq <- 1
wkART <- setDT(cd_US_trWords18.2[,4:5])[, .(sum_amt = sum(freq)), by = .(wk = wk)]

#### ChinaDaily_US_Trade Word Stems Weekly Sums for 2018 ####
cd_US_tradeStems18 <- data.frame(Stem = unique(cd_US_trWords18$stem))
new <- rep(0, nrow(cd_US_tradeStems18))
for (i in 1:52) {
  cd_US_tradeStems18 <- cbind(cd_US_tradeStems18, new)
  names(cd_US_tradeStems18)[i+1] <- paste0("wk",i)
}
wkDTx <- data.frame(wk = 1:52)
for (i in 1:nrow(cd_US_tradeStems18)) {
  stemPres <- cd_US_trWords18[which(cd_US_trWords18$stem == cd_US_tradeStems18$Stem[i]),4:5]
  stemPres$freq <- 1
  stemPres$date <- as.Date(stemPres$date)
  wkDT <- setDT(stemPres)[, .(sum_amt = sum(freq)), by = .(wk = week(date))]
  wkDT <- wkDT[order(wkDT$wk),]
  wkDT <- merge(wkDTx, wkDT, all.x = T)
  wkDT$sum_amt[which(is.na(wkDT$sum_amt) == T)] <- 0 
  cd_US_tradeStems18[i,2:53] <- wkDT$sum_amt 
  
  if(i%%100==0) {cat(i)} else {
    if(i%%20==0) {cat(".")}
  }
  if(i == nrow(cd_US_tradeStems18)) {cat("//", i, "of", i)}
}

rownames(cd_US_tradeStems18) <- cd_US_tradeStems18$Stem
cd_US_tradeStems18$YrMean <- rowMeans(cd_US_tradeStems18[,2:53])
cd_US_tradeStems18$YrSum <- rowSums(cd_US_tradeStems18[,2:53])
cd_US_tradeStems18 <- cd_US_tradeStems18[which(cd_US_tradeStems18$YrSum > 10),]

#### China-US Events ####
ChinaUS_events$Date <- as.Date(ChinaUS_events$Date)
ChinaUS_events$wk <- week(ChinaUS_events$Date)
ChAnnoun <- ChinaUS_events[which((ChinaUS_events$Actor == "c" | ChinaUS_events$Actor == "b") & 
                                   ChinaUS_events$Announcement == 1),]

#### Plots ####
ggplot() +
  geom_line(aes(x = 1:52, y = as.vector(unlist(cd_US_tradeStems18["WILL",2:53])), colour = "WILL")) + 
  geom_line(aes(x = 1:52, y = as.vector(unlist(cd_US_tradeStems18["NEGOTI",2:53])), colour = "NEGOTI")) + 
  geom_vline(xintercept = week(ChAnnoun$Date[-c(7:8)])) +
  labs(colour = "Word Stem") +
  xlab("Week # (2018)") + ylab("Weekly Frequency Sum of Word Stem") +
  ggtitle("Weekly Frequencies of Word Stems in 2018 China Daily Articles Mentioning 'Trade' and 'US'", 
          subtitle = "With Chinese Trade Policy Announcements") +
  geom_text(aes(x = week(ChAnnoun$Date)[1] - 0.25, label="China reacts to US List 1, proposes own", y = 85), 
          angle = 90, text = element_text(size = 1)) +
  geom_text(aes(x = week(ChAnnoun$Date)[2] + 0.25, label="China announces antidumping duties against US sorghum", 
                y = 85), angle = 90, text = element_text(size = 1)) +
  geom_text(aes(x = week(ChAnnoun$Date)[3] - 0.25, label="China announces it will end sorghum duties", y = 85), 
            angle = 90, text = element_text(size = 1)) +
  geom_text(aes(x = week(ChAnnoun$Date)[4] + 0.25, label="US and China agree to pause trade war", y = 85), 
            angle = 90, text = element_text(size = 1)) +
  geom_text(aes(x = week(ChAnnoun$Date)[5] - 0.25, label="China reacts to US List 2, proposes own", y = 85), 
            angle = 90, text = element_text(size = 1)) +
  geom_text(aes(x = week(ChAnnoun$Date)[6] - 0.25, label="China Reacts to US List 3, proposes Own", y = 85), 
            angle = 90, text = element_text(size = 1)) +
  geom_text(aes(x = week(ChAnnoun$Date)[9] - 0.25, label="US and China agree to temporary truce", y = 85), 
            angle = 90, text = element_text(size = 1)) 


# write.csv(cd_US_tradeStems18, "cd_US_tradeStems18.csv")



  

#####








