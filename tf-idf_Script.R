library(quanteda)
library(lubridate)
library(dplyr)
library(tidytext)
library(ggplot2)
library(ggpubr)

# *** External Files Needed: ****
View(chinaDaily)
View(china_daily_trade)
View(china_daily_nk)
View(ChinaUS_events)
View(NK_events)

# *** China/US Trade events Policy & Implementation only ***
usPolImp <- ChinaUS_events[which((ChinaUS_events$Actor == "u" | ChinaUS_events$Actor == "b") & 
                                   (ChinaUS_events$Political.Move == 1 | ChinaUS_events$Implementation == 1)),]
usPolImp$Year <- NA
for (i in 1:nrow(usPolImp)) {
  usPolImp$Year[i] <- strsplit(as.character(usPolImp$Date),"-")[[i]][1]
}
usPolImp$week <- week(usPolImp$Date)
usPolImp$yrwk <- paste0(usPolImp$Year, "-", usPolImp$week)

#### Functions ####
countStm <- function(cd_df, yr) {
  count.stm1 <- data.frame(stem = "...", word = "...", stringsAsFactors = F)
  cdx1 <- cd_df[which(cd_df$year == yr),]
  
  for (w in 1:52) {
    if(w %in% unique(cdx1$week) == F) {
      count.stm1$new <- NA
      names(count.stm1)[w+2] <- paste0("wk", w)
    } else {
      txt1 <- cdx1$text[which(cdx1$week == w)]
      #print(w)
      txt1 <- gsub('[[:punct:]]','', txt1)
      txt1 <- gsub('[[:digit:]]','', txt1)
      dfm1 <- dfm(txt1, remove_punct = T)
      dfm1 <- dfm_remove(dfm1, stopwords("english"))
      dfm1 <- dfm_remove(dfm1,  "\\p{Z}", valuetype = "regex")
      stem1 <- char_wordstem(colnames(dfm1), language = quanteda_options("language_stemmer"))
      id1 <- data.frame(id = 1:length(stem1), stem1 = stem1, colnames(dfm1), stringsAsFactors = F)
      id1 <- id1[which(duplicated(id1$stem1) == F),]
      stem2 <- dfm_wordstem(dfm1, language = quanteda_options("language_stemmer"))
      id1$new <- colMeans(stem2)
      names(id1) <- c("id", "stem", "word", paste0("wk", w))
      count.stm1 <- merge(count.stm1, id1[,2:4], by = c("stem"), all = T)
      count.stm1 <- count.stm1[order(count.stm1[,2], decreasing = T),]
      n <- length(count.stm1$word.x[which(is.na(count.stm1$word.x) == T)])
      count.stm1$word.x[(nrow(count.stm1) - (n)):nrow(count.stm1)] <- count.stm1$word.y[(nrow(count.stm1) - (n)):nrow(count.stm1)]
      count.stm1$word.y <- NULL
      names(count.stm1)[2] <- "word"
    }
  }
  
  count.stm1 <- count.stm1[order(count.stm1$stem),]
  count.stm1[is.na(count.stm1)] <- 0
  count.stm1$year <- yr
  return(count.stm1)
}


cdL <- function(cdDf) {
  L <- reshape(cdDf, varying = names(cdDf)[3:54], v.names = "wk.mean", timevar = "week",
               idvar = c("stem", "year"), direction = "long")
  L <- L[2:nrow(L),]
  row.names(L) <- 1:nrow(L)
  L <- as.tbl(L)
  L$yrwk <- paste0(L$year,"-",L$week)
  L$yrwk <- as.factor(L$yrwk)
  L <- L[which(L$stem != "..."),]
  L <- L %>%
    bind_tf_idf(term = stem, document = yrwk, n = wk.mean) %>%
    arrange(desc(tf_idf))
  L$sentiment <- tokens_lookup(tokens(L$word), 
                               dictionary = data_dictionary_LSD2015, exclusive = FALSE)
  L$sentiment <- as.character(L$sentiment)
  L$yrwk2 <- as.character(L$yrwk)
  L$rank <- NA
  L$WkN <- NA
  for(i in 1:length(unique(L$yrwk2))) {
    yw1 <- L[which(L$yrwk2 == unique(L$yrwk2)[i]),]
    yw1 <- yw1[order(yw1$tf_idf, decreasing = T),]
    yw1$rank[which(yw1$wk.mean > 0)] <- 1:nrow(yw1[which(yw1$wk.mean > 0),])
    yw1$WkN <- nrow(yw1[which(yw1$wk.mean > 0),])
    yw2 <- L[which(L$yrwk2 != unique(L$yrwk2)[i]),]
    L <- rbind(yw1, yw2)
  }
  return(L)
}

cd.Sent <- function(df, n) {
  yr <- 2017:2018
  for(u in yr) {
    df.sent <- data.frame(year = u, week = 1:52, nPos = NA, nNeg = NA, nWk = NA, tf.idf_Median = NA,
                          tf.idf_Max = NA, tf.idf_Min = NA)
    for(i in 1:52) {
      v <- df[which(df$year == u & df$week == i & df$rank < (n+1) & df$tf_idf > 0),]
      v$WkN <- as.numeric(v$WkN)
      df.sent$nPos[i] <- nrow(v[which(v$sentiment == "POSITIVE"),])
      df.sent$nNeg[i] <- nrow(v[which(v$sentiment == "NEGATIVE"),])
      
      if(nrow(v) < 1) {
        df.sent$nWk[i] <- 0
        df.sent$tf.idf_Median[i] <- 0
        df.sent$tf.idf_Max[i] <- 0
        df.sent$tf.idf_Min[i] <- 0
      } else {
        if(unique(v$WkN) > 0) {
          df.sent$nWk[i] <- unique(v$WkN)
          # df.sent$nWk[i] <- unique(df$WkN[which(df$rank %in% 1:n == T & 
          #                                         df$year == u & df$week == i)])
          df.sent$tf.idf_Median[i] <- median(v$tf_idf)
          df.sent$tf.idf_Max[i] <- max(v$tf_idf)
          df.sent$tf.idf_Min[i] <- min(v$tf_idf)
        } else {
          df.sent$nWk[i] <- 0
          df.sent$tf.idf_Median[i] <- 0
          df.sent$tf.idf_Max[i] <- 0
          df.sent$tf.idf_Min[i] <- 0
        }
      }
    }
    if(u == 2017) {
      df.sent$posPerc <- df.sent$nPos/df.sent$nWk
      df.sent$negPerc <- df.sent$nNeg/df.sent$nWk
      
      df.sent300 <- df.sent[which(df.sent$nPos != 0),]
    } else {
      df.sent$posPerc <- df.sent$nPos/df.sent$nWk
      df.sent$negPerc <- df.sent$nNeg/df.sent$nWk
      
      df.sent300 <- rbind(df.sent300, df.sent[which(df.sent$nPos != 0),])
      df.sent300$diff <- df.sent300$nNeg - df.sent300$nPos
      df.sent300$yrwk <- paste0(df.sent300$year, "-", df.sent300$week)
      df.sent300$id <- 1:nrow(df.sent300)
    }
  }
  return(df.sent300)
}


#### chinaDaily Editing ####
chinaDaily <- chinaDaily[which(duplicated(chinaDaily$text) == F),]  ## Just to make sure there aren't any duplicated articles
chinaDaily$week <- week(chinaDaily$date_published)
chinaDaily$year <- as.numeric(chinaDaily$Year)

#### ALL ####

# *** Use countStm function from above to collect all unique terms from the articles 
# in the specific year and the mean of how many times each term occurs per week that year. ***
cdAll.16 <- countStm(chinaDaily, 2016) 
cdAll.17 <- countStm(chinaDaily, 2017)
cdAll.18 <- countStm(chinaDaily, 2018)
cdAll.19 <- countStm(chinaDaily, 2019)

cdAll <- rbind(cdAll.16, cdAll.17, cdAll.18, cdAll.19)
cdAll <- cdAll[which((nchar(cdAll$stem) > 1) == T),]
cdAll[is.na(cdAll)] <- 0


# *** Use cdL function from above to: 
# (1) reshape the above dataframe into long format, 
# (2) Run td_idf on each word (with the specific week being the "document")
# (3) Run a positive/negative sentiment test on each unique word
# (4) Rank each term by week (1 = highest td_idf for each week) ***
cdAllL <- cdL(cdAll) 


#### Trade ####

china_daily_trade$year <- year(china_daily_trade$date_published)
china_daily_trade$week <- week(china_daily_trade$date_published)

cdTrd.17 <- countStm(china_daily_trade, 2017)
cdTrd.18 <- countStm(china_daily_trade, 2018)
cdTrd.19 <- countStm(china_daily_trade, 2019)

cdTrd <- rbind(cdTrd.17, cdTrd.18, cdTrd.19)
cdTrd <- cdTrd[which((nchar(cdTrd$stem) > 1) == T),]
cdTrd[is.na(cdTrd)] <- 0

cdTrdL <- cdL(cdTrd) 


#### North Korea ####

china_daily_nk$year <- year(china_daily_nk$date_published)
china_daily_nk$week <- week(china_daily_nk$date_published)

cdNk.17 <- countStm(china_daily_nk, 2017)
cdNk.18 <- countStm(china_daily_nk, 2018)
cdNk.19 <- countStm(china_daily_nk, 2019)

cdNk <- rbind(cdNk.17, cdNk.18, cdNk.19)
cdNk <- cdNk[which((nchar(cdNk$stem) > 1) == T),]
cdNk[is.na(cdNk)] <- 0

cdNkL <- cdL(cdNk) 


#### Plotting (Trade) ####

# *** This function creates a dataframe from which the data will be plotted, allowing for
# choice of the number of observations to be examined (based on the words with 
# the highest td_idf ranks each week). ***  

cdAll.sent500 <- cd.Sent(cdAllL, 500) 

cdAll.sent50 <- cd.Sent(cdAllL, 50)  ## The top 50 words offers a clearer visualization of the data

all50.tr <- ggplot(cdAll.sent50[-(1:13),]) +  ## Due to a lack of data, the first 13 weeks are skewed.
  geom_line(aes(id, posPerc, colour = "Postive")) +
  geom_line(aes(id, negPerc, colour = "Negative")) +
  labs(y = "Percent", colour = "Sentiment") +
  ylim(0,0.05) +
  scale_x_continuous(name = "Week", breaks = seq(20,70,10), labels = cdAll.sent50$yrwk[seq(20,70,10)]) +
  ggtitle("Sentiment % by Week - All [Top 50] with Trade Events", 
          subtitle = paste0("Positive Mean: ", round(mean(cdAll.sent50$posPerc),4)*100, "%", " | ", 
                            "Negative Mean: ", round(mean(cdAll.sent50$negPerc),4)*100, "%")) +
  ## The below lines are for the Trade event dates
  geom_vline(aes(xintercept = cdAll.sent50$id[which(cdAll.sent50$yrwk %in% usPolImp$yrwk[1] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = cdAll.sent50$id[which(cdAll.sent50$yrwk %in% usPolImp$yrwk[2] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = cdAll.sent50$id[which(cdAll.sent50$yrwk %in% usPolImp$yrwk[3] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = cdAll.sent50$id[which(cdAll.sent50$yrwk %in% usPolImp$yrwk[4] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = cdAll.sent50$id[which(cdAll.sent50$yrwk %in% usPolImp$yrwk[5] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = cdAll.sent50$id[which(cdAll.sent50$yrwk %in% usPolImp$yrwk[6] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = cdAll.sent50$id[which(cdAll.sent50$yrwk %in% usPolImp$yrwk[7] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = cdAll.sent50$id[which(cdAll.sent50$yrwk %in% usPolImp$yrwk[8] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = cdAll.sent50$id[which(cdAll.sent50$yrwk %in% usPolImp$yrwk[9] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = cdAll.sent50$id[which(cdAll.sent50$yrwk %in% usPolImp$yrwk[10] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = cdAll.sent50$id[which(cdAll.sent50$yrwk %in% usPolImp$yrwk[11] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = cdAll.sent50$id[which(cdAll.sent50$yrwk %in% usPolImp$yrwk[12] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = cdAll.sent50$id[which(cdAll.sent50$yrwk %in% usPolImp$yrwk[13] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = cdAll.sent50$id[which(cdAll.sent50$yrwk %in% usPolImp$yrwk[14] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = cdAll.sent50$id[which(cdAll.sent50$yrwk %in% usPolImp$yrwk[15] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = cdAll.sent50$id[which(cdAll.sent50$yrwk %in% usPolImp$yrwk[16] == T)]), 
             size = 0.25) 



cdTrd.sent50 <- cd.Sent(cdTrdL, 50)

trd50 <- ggplot(cdTrd.sent50[-(1:13),]) +
  geom_line(aes(id, posPerc, colour = "Postive")) +
  geom_line(aes(id, negPerc, colour = "Negative")) +
  labs(y = "Percent", colour = "Sentiment") +
  ylim(0,0.05) +
  scale_x_continuous(name = "Week", breaks = seq(10,70,10), labels = cdTrd.sent50$yrwk[seq(10,70,10)]) +
  ggtitle("Sentiment % by Week - US TRADE [Top 50]", 
          subtitle = paste0("Positive Mean: ", round(mean(cdTrd.sent50$posPerc),4)*100, "%", " | ", 
                            "Negative Mean: ", round(mean(cdTrd.sent50$negPerc),4)*100, "%")) +
  geom_vline(aes(xintercept = cdTrd.sent50$id[which(cdTrd.sent50$yrwk %in% usPolImp$yrwk[1] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = cdTrd.sent50$id[which(cdTrd.sent50$yrwk %in% usPolImp$yrwk[2] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = cdTrd.sent50$id[which(cdTrd.sent50$yrwk %in% usPolImp$yrwk[3] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = cdTrd.sent50$id[which(cdTrd.sent50$yrwk %in% usPolImp$yrwk[4] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = cdTrd.sent50$id[which(cdTrd.sent50$yrwk %in% usPolImp$yrwk[5] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = cdTrd.sent50$id[which(cdTrd.sent50$yrwk %in% usPolImp$yrwk[6] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = cdTrd.sent50$id[which(cdTrd.sent50$yrwk %in% usPolImp$yrwk[7] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = cdTrd.sent50$id[which(cdTrd.sent50$yrwk %in% usPolImp$yrwk[8] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = cdTrd.sent50$id[which(cdTrd.sent50$yrwk %in% usPolImp$yrwk[9] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = cdTrd.sent50$id[which(cdTrd.sent50$yrwk %in% usPolImp$yrwk[10] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = cdTrd.sent50$id[which(cdTrd.sent50$yrwk %in% usPolImp$yrwk[11] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = cdTrd.sent50$id[which(cdTrd.sent50$yrwk %in% usPolImp$yrwk[12] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = cdTrd.sent50$id[which(cdTrd.sent50$yrwk %in% usPolImp$yrwk[13] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = cdTrd.sent50$id[which(cdTrd.sent50$yrwk %in% usPolImp$yrwk[14] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = cdTrd.sent50$id[which(cdTrd.sent50$yrwk %in% usPolImp$yrwk[15] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = cdTrd.sent50$id[which(cdTrd.sent50$yrwk %in% usPolImp$yrwk[16] == T)]), 
             size = 0.25) 

# *** This will plot the two graphs side by side. ***
ggarrange(all50.tr, trd50, ncol = 2, nrow = 1)


#### Plotting (North Korea) ####
all50.nk <- ggplot(cdAll.sent50[-(1:13),]) +  ## Due to a lack of data, the first 13 weeks are skewed.
  geom_line(aes(id, posPerc, colour = "Postive")) +
  geom_line(aes(id, negPerc, colour = "Negative")) +
  labs(y = "Percent", colour = "Sentiment") +
  ylim(0,0.05) +
  scale_x_continuous(name = "Week", breaks = seq(20,70,10), labels = cdAll.sent50$yrwk[seq(20,70,10)]) +
  ggtitle("Sentiment % by Week - All [Top 50] with North Korea Events", 
          subtitle = paste0("Positive Mean: ", round(mean(cdAll.sent50$posPerc),4)*100, "%", " | ", 
                            "Negative Mean: ", round(mean(cdAll.sent50$negPerc),4)*100, "%")) +
  ## The below lines are for the North Korea event dates
  geom_vline(aes(xintercept = cdNk.sent50$id[which(cdNk.sent50$yrwk %in% NK_events$yrwk[2] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = cdNk.sent50$id[which(cdNk.sent50$yrwk %in% NK_events$yrwk[3] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = cdNk.sent50$id[which(cdNk.sent50$yrwk %in% NK_events$yrwk[4] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = cdNk.sent50$id[which(cdNk.sent50$yrwk %in% NK_events$yrwk[5] == T)]),
             size = 0.25) +
  geom_vline(aes(xintercept = cdNk.sent50$id[which(cdNk.sent50$yrwk %in% NK_events$yrwk[6] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = cdNk.sent50$id[which(cdNk.sent50$yrwk %in% NK_events$yrwk[7] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = cdNk.sent50$id[which(cdNk.sent50$yrwk %in% NK_events$yrwk[8] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = cdNk.sent50$id[which(cdNk.sent50$yrwk %in% NK_events$yrwk[9] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = cdNk.sent50$id[which(cdNk.sent50$yrwk %in% NK_events$yrwk[10] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = cdNk.sent50$id[which(cdNk.sent50$yrwk %in% NK_events$yrwk[11] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = cdNk.sent50$id[which(cdNk.sent50$yrwk %in% NK_events$yrwk[12] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = cdNk.sent50$id[which(cdNk.sent50$yrwk %in% NK_events$yrwk[13] == T)]), 
             size = 0.25)


Nk50 <- ggplot(cdNk.sent50) +
  geom_line(aes(id, posPerc, colour = "Postive")) +
  geom_line(aes(id, negPerc, colour = "Negative")) +
  labs(y = "Percent", colour = "Sentiment") +
  ylim(0,0.07) +
  scale_x_continuous(name = "Week", breaks = seq(10,70,10), labels = cdNk.sent50$yrwk[seq(10,70,10)]) +
  ggtitle("Sentiment % by Week - North Korea [Top 50]", 
          subtitle = paste0("Positive Median: ", round(median(cdNk.sent50$posPerc),4)*100, "%", " | ", 
                            "Negative Median: ", round(median(cdNk.sent50$negPerc),4)*100, "%")) +
  geom_vline(aes(xintercept = cdNk.sent50$id[which(cdNk.sent50$yrwk %in% NK_events$yrwk[2] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = cdNk.sent50$id[which(cdNk.sent50$yrwk %in% NK_events$yrwk[3] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = cdNk.sent50$id[which(cdNk.sent50$yrwk %in% NK_events$yrwk[4] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = cdNk.sent50$id[which(cdNk.sent50$yrwk %in% NK_events$yrwk[5] == T)]),
             size = 0.25) +
  geom_vline(aes(xintercept = cdNk.sent50$id[which(cdNk.sent50$yrwk %in% NK_events$yrwk[6] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = cdNk.sent50$id[which(cdNk.sent50$yrwk %in% NK_events$yrwk[7] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = cdNk.sent50$id[which(cdNk.sent50$yrwk %in% NK_events$yrwk[8] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = cdNk.sent50$id[which(cdNk.sent50$yrwk %in% NK_events$yrwk[9] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = cdNk.sent50$id[which(cdNk.sent50$yrwk %in% NK_events$yrwk[10] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = cdNk.sent50$id[which(cdNk.sent50$yrwk %in% NK_events$yrwk[11] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = cdNk.sent50$id[which(cdNk.sent50$yrwk %in% NK_events$yrwk[12] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = cdNk.sent50$id[which(cdNk.sent50$yrwk %in% NK_events$yrwk[13] == T)]), 
             size = 0.25)



# *** This will plot the two graphs side by side. ***
ggarrange(all50.nk, Nk50, ncol = 2, nrow = 1)
