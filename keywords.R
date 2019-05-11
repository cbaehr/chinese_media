library(quanteda)
library(lubridate)
library(dplyr)
library(tidytext)
library(ggplot2)
library(ggpubr)


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

#### Trade Stems ####
china_daily_trade$year <- year(china_daily_trade$date_published)
china_daily_trade$week <- week(china_daily_trade$date_published)

cdTrd.17 <- countStm(china_daily_trade, 2017)
cdTrd.18 <- countStm(china_daily_trade, 2018)
cdTrd.19 <- countStm(china_daily_trade, 2019)

cdTrd <- rbind(cdTrd.17, cdTrd.18, cdTrd.19)
cdTrd <- cdTrd[which((nchar(cdTrd$stem) > 1) == T),]
cdTrd[is.na(cdTrd)] <- 0

#### Keywords ####
### keywords.usTrd --> Weekly Avg: The weekly frequency of each word divided by the total number of 
# articles that week
## With adjustments for specific words with weak stemming

keywords.usTrd <- data.frame(Year = c(rep(2017, 52), rep(2018, 52), rep(2019,52)), Week = rep(1:52,3), 
                             negoti = c(unlist(cdTrd[which(cdTrd$stem == "negoti" & cdTrd$year == 2017), 
                                                     3:54]),
                                        unlist(cdTrd[which(cdTrd$stem == "negoti" & cdTrd$year == 2018), 
                                            3:54]),
                                        unlist(cdTrd[which(cdTrd$stem == "negoti" & cdTrd$year == 2019), 
                                                     3:54])))

keywords.usTrd$war <- c(unlist(cdTrd[which(cdTrd$stem == "war" & cdTrd$year == 2017), 3:54]),
                        unlist(cdTrd[which(cdTrd$stem == "war" & cdTrd$year == 2018), 3:54]),
                        unlist(cdTrd[which(cdTrd$stem == "war" & cdTrd$year == 2019), 3:54]))
keywords.usTrd$friction <- c(unlist(cdTrd[which(cdTrd$stem == "friction" & cdTrd$year == 2017), 3:54]),
                             unlist(cdTrd[which(cdTrd$stem == "friction" & cdTrd$year == 2018), 3:54]),
                             unlist(cdTrd[which(cdTrd$stem == "friction" & cdTrd$year == 2019), 3:54]))
# ********** // disput, disputati
keywords.usTrd$disput <- c(unlist(cdTrd[which(cdTrd$stem == "disput" & cdTrd$year == 2017), 3:54]),
                           unlist(colSums(cdTrd[which((cdTrd$stem == "disput" | cdTrd$stem == "disputati") & 
                                                cdTrd$year == 2018), 3:54])),
                           unlist(cdTrd[which(cdTrd$stem == "disput" & cdTrd$year == 2019), 3:54]))
# **********
keywords.usTrd$structur <- c(unlist(cdTrd[which(cdTrd$stem == "structur" & cdTrd$year == 2017), 3:54]),
                             unlist(cdTrd[which(cdTrd$stem == "structur" & cdTrd$year == 2018), 3:54]),
                             unlist(cdTrd[which(cdTrd$stem == "structur" & cdTrd$year == 2019), 3:54]))
keywords.usTrd$purchas <- c(unlist(cdTrd[which(cdTrd$stem == "purchas" & cdTrd$year == 2017), 3:54]),
                            unlist(cdTrd[which(cdTrd$stem == "purchas" & cdTrd$year == 2018), 3:54]),
                            rep(0,52))
keywords.usTrd$unfair <- c(unlist(cdTrd[which(cdTrd$stem == "unfair" & cdTrd$year == 2017), 3:54]),
                           unlist(cdTrd[which(cdTrd$stem == "unfair" & cdTrd$year == 2018), 3:54]),
                           unlist(cdTrd[which(cdTrd$stem == "unfair" & cdTrd$year == 2019), 3:54]))
# ********** // hegemon
keywords.usTrd$hegemon <- c(unlist(cdTrd[which(cdTrd$stem == "hegemoni" & cdTrd$year == 2017), 3:54]),
                            unlist(colSums(cdTrd[which((cdTrd$stem == "hegemon" | cdTrd$stem == "hegemoni" | 
                                                  cdTrd$stem == "hegemonist") & cdTrd$year == 2018), 3:54])),
                            unlist(colSums(cdTrd[which((cdTrd$stem == "hegemon" | cdTrd$stem == "hegemoni") & 
                                                 cdTrd$year == 2019), 3:54])))
# ********** // unilateral
keywords.usTrd$unilater <- c(unlist(cdTrd[which(cdTrd$stem == "unilater" & cdTrd$year == 2017), 3:54]),
                             unlist(colSums(cdTrd[which((cdTrd$stem == "unilater" | cdTrd$stem == "unilateralist") & 
                                                          cdTrd$year == 2018), 3:54])),
                             unlist(colSums(cdTrd[which((cdTrd$stem == "unilater" | cdTrd$stem == "unilateralist") & 
                                                          cdTrd$year == 2019), 3:54])))
# ********** // discrimin
keywords.usTrd$discrimin <- c(unlist(colSums(cdTrd[which((cdTrd$stem == "discrimin" |
                                                                 cdTrd$stem == "discriminatori" |
                                                                 cdTrd$stem == "nondiscriminatori") &
                                                           cdTrd$year == 2017), 3:54])), 
                              unlist(colSums(cdTrd[which((cdTrd$stem == "discrimin" | 
                                                                 cdTrd$stem == "discriminatori" |
                                                                 cdTrd$stem == "nondiscriminatori" |
                                                                 cdTrd$stem == "indiscriminatori") &
                                                     cdTrd$year == 2018), 3:54])),
                              unlist(cdTrd[which(cdTrd$stem == "discrimin" & cdTrd$year == 2019), 3:54]))
# **********

keywords.usTrd$yrwk <- paste0(keywords.usTrd$Year, "-", keywords.usTrd$Week)
keywords.usTrd$id <- 1:nrow(keywords.usTrd)


ggplot(data = keywords.usTrd) +
  geom_line(aes(x = 1:104, y = negoti, colour = "negoti")) + 
  geom_line(aes(x = 1:104, y = war, colour = "war")) +
  geom_line(aes(x = 1:104, y = friction, colour = "friction")) + 
  geom_line(aes(x = 1:104, y = disput, colour = "disput")) + 
  geom_line(aes(x = 1:104, y = structur, colour = "structur")) + 
  geom_line(aes(x = 1:104, y = purchas, colour = "purchas")) + 
  geom_line(aes(x = 1:104, y = unfair, colour = "unfair")) + 
  geom_line(aes(x = 1:104, y = hegemon, colour = "hegemon")) + 
  geom_line(aes(x = 1:104, y = unilater, colour = "unilater")) + 
  geom_line(aes(x = 1:104, y = discrimin, colour = "discrimin")) 

china_daily_trade$yrwk <- paste0(china_daily_trade$year, "-", china_daily_trade$week)

cdTrd.wkN <- data.frame(yrwk = keywords.usTrd$yrwk, n = NA)
for(i in cdTrd.wkN$yrwk) {
  cdTrd.wkN$n[which(cdTrd.wkN$yrwk == i)] <- nrow(china_daily_trade[which(china_daily_trade$yrwk == i),])
}

keywords.usTrd$wkN <- cdTrd.wkN$n
#### Discriminate ####
disc <- ggplot(data = keywords.usTrd[1:116,]) +
  geom_bar(stat = "identity", aes(x = id, y = wkN/35), alpha = 0.3) +
  scale_y_continuous(sec.axis = sec_axis(~.*35, name = "# of Articles Each Week", breaks = seq(0,12,3))) +
  geom_line(aes(x = id, y = discrimin, colour = "discrimin/Discriminate"), size = 1) + 
  labs(y = "Avg Freq per Article per Week", colour = "Stem/Word") +
  #ylim(0,0.09) +
  scale_x_continuous(name = "Week", breaks = seq(10,110,20), labels = keywords.usTrd$yrwk[seq(10,110,20)]) +
  ggtitle("Weekly Avg Keyword Frequency - Discriminate") +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[1] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[2] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[3] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[4] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[5] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[6] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[7] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[8] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[9] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[10] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[11] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[12] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[13] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[14] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[15] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[16] == T)]), 
             size = 0.25) +
  theme(legend.position = "none")

#### Dispute ####
dis <- ggplot(data = keywords.usTrd[1:116,]) +
  geom_bar(stat = "identity", aes(x = id, y = wkN/4), alpha = 0.3) +
  scale_y_continuous(sec.axis = sec_axis(~.*4, name = "# of Articles Each Week", breaks = seq(0,12,3))) +
  geom_line(aes(x = id, y = disput, colour = "disput/Dispute"), size = 1) + 
  labs(y = "Avg Freq per Article per Week", colour = "Stem/Word") +
  #ylim(0,0.09) +
  scale_x_continuous(name = "Week", breaks = seq(10,110,20), labels = keywords.usTrd$yrwk[seq(10,110,20)]) +
  ggtitle("Weekly Avg Keyword Frequency - Dispute") +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[1] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[2] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[3] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[4] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[5] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[6] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[7] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[8] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[9] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[10] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[11] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[12] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[13] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[14] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[15] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[16] == T)]), 
             size = 0.25) +
  theme(legend.position = "none")
#### Friction ####
fri <- ggplot(data = keywords.usTrd[1:116,]) +
  geom_bar(stat = "identity", aes(x = id, y = wkN/6), alpha = 0.3) +
  scale_y_continuous(sec.axis = sec_axis(~.*6, name = "# of Articles Each Week", breaks = seq(0,12,3))) +
  geom_line(aes(x = id, y = friction, colour = "friction/Friction"), size = 1) + 
  labs(y = "Avg Freq per Article per Week", colour = "Stem/Word") +
  #ylim(0,0.09) +
  scale_x_continuous(name = "Week", breaks = seq(10,110,20), labels = keywords.usTrd$yrwk[seq(10,110,20)]) +
  ggtitle("Weekly Avg Keyword Frequency - Friction") +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[1] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[2] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[3] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[4] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[5] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[6] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[7] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[8] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[9] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[10] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[11] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[12] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[13] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[14] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[15] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[16] == T)]), 
             size = 0.25) +
  theme(legend.position = "none")
#### Hegemony ####
heg <- ggplot(data = keywords.usTrd[1:116,]) +
  geom_bar(stat = "identity", aes(x = id, y = wkN/4), alpha = 0.3) +
  scale_y_continuous(sec.axis = sec_axis(~.*4, name = "# of Articles Each Week", breaks = seq(0,12,2))) +
  geom_line(aes(x = id, y = hegemon, colour = "hegemon/Hegemony"), size = 1) + 
  labs(y = "Avg Freq per Article per Week", colour = "Stem/Word") +
  #ylim(0,0.09) +
  scale_x_continuous(name = "Week", breaks = seq(10,110,20), labels = keywords.usTrd$yrwk[seq(10,110,20)]) +
  ggtitle("Weekly Avg Keyword Frequency - Hegemony") +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[1] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[2] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[3] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[4] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[5] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[6] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[7] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[8] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[9] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[10] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[11] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[12] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[13] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[14] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[15] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[16] == T)]), 
             size = 0.25) +
  theme(legend.position = "none")
#### Negotiate ####
neg <- ggplot(data = keywords.usTrd[1:116,]) +
  geom_bar(stat = "identity", aes(x = id, y = wkN/4), alpha = 0.3) +
  scale_y_continuous(sec.axis = sec_axis(~.*4, name = "# of Articles Each Week", breaks = seq(0,12,2))) +
  geom_line(aes(x = id, y = negoti, colour = "negoti/Negotiate"), size = 1) + 
  labs(y = "Avg Freq per Article per Week", colour = "Stem/Word") +
  #ylim(0,0.09) +
  scale_x_continuous(name = "Week", breaks = seq(10,110,20), labels = keywords.usTrd$yrwk[seq(10,110,20)]) +
  ggtitle("Weekly Avg Keyword Frequency - Negotiate") +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[1] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[2] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[3] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[4] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[5] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[6] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[7] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[8] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[9] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[10] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[11] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[12] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[13] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[14] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[15] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[16] == T)]), 
             size = 0.25) +
  theme(legend.position = "none")

#### Purchase ####
pur <- ggplot(data = keywords.usTrd[1:116,]) +
  geom_bar(stat = "identity", aes(x = id, y = wkN/6), alpha = 0.3) +
  scale_y_continuous(sec.axis = sec_axis(~.*6, name = "# of Articles Each Week", breaks = seq(0,12,3))) +
  geom_line(aes(x = id, y = purchas, colour = "purchas/Purchase"), size = 1) + 
  labs(y = "Avg Freq per Article per Week", colour = "Stem/Word") +
  #ylim(0,0.09) +
  scale_x_continuous(name = "Week", breaks = seq(10,110,20), labels = keywords.usTrd$yrwk[seq(10,110,20)]) +
  ggtitle("Weekly Avg Keyword Frequency - Purchase") +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[1] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[2] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[3] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[4] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[5] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[6] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[7] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[8] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[9] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[10] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[11] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[12] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[13] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[14] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[15] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[16] == T)]), 
             size = 0.25) +
  theme(legend.position = "none")

#### Structure ####
str <- ggplot(data = keywords.usTrd[1:116,]) +
  geom_bar(stat = "identity", aes(x = id, y = wkN/8), alpha = 0.3) +
  scale_y_continuous(sec.axis = sec_axis(~.*8, name = "# of Articles Each Week", breaks = seq(0,12,2))) +
  geom_line(aes(x = id, y = structur, colour = "structur/Structure"), size = 1) + 
  labs(y = "Avg Freq per Article per Week", colour = "Stem/Word") +
  #ylim(0,0.09) +
  scale_x_continuous(name = "Week", breaks = seq(10,110,20), labels = keywords.usTrd$yrwk[seq(10,110,20)]) +
  ggtitle("Weekly Avg Keyword Frequency - Structure") +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[1] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[2] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[3] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[4] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[5] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[6] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[7] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[8] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[9] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[10] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[11] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[12] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[13] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[14] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[15] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[16] == T)]), 
             size = 0.25) +
  theme(legend.position = "none")

#### Unfair ####
unf <- ggplot(data = keywords.usTrd[1:116,]) +
  geom_bar(stat = "identity", aes(x = id, y = wkN/6), alpha = 0.3) +
  scale_y_continuous(sec.axis = sec_axis(~.*6, name = "# of Articles Each Week", breaks = seq(0,12,3))) +
  geom_line(aes(x = id, y = unfair, colour = "unfair/Unfair"), size = 1) + 
  labs(y = "Avg Freq per Article per Week", colour = "Stem/Word") +
  #ylim(0,0.09) +
  scale_x_continuous(name = "Week", breaks = seq(10,110,20), labels = keywords.usTrd$yrwk[seq(10,110,20)]) +
  ggtitle("Weekly Avg Keyword Frequency - Unfair") +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[1] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[2] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[3] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[4] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[5] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[6] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[7] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[8] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[9] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[10] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[11] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[12] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[13] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[14] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[15] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[16] == T)]), 
             size = 0.25) +
  theme(legend.position = "none")
#### Unilateral ####
uni <- ggplot(data = keywords.usTrd[1:116,]) +
  geom_bar(stat = "identity", aes(x = id, y = wkN/6), alpha = 0.3) +
  scale_y_continuous(sec.axis = sec_axis(~.*6, name = "# of Articles Each Week", breaks = seq(0,12,3))) +
  geom_line(aes(x = id, y = unilater, colour = "unilater/Unilateral"), size = 1) + 
  labs(y = "Avg Freq per Article per Week", colour = "Stem/Word") +
  #ylim(0,0.09) +
  scale_x_continuous(name = "Week", breaks = seq(10,110,20), labels = keywords.usTrd$yrwk[seq(10,110,20)]) +
  ggtitle("Weekly Avg Keyword Frequency - Unilateral") +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[1] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[2] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[3] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[4] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[5] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[6] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[7] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[8] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[9] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[10] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[11] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[12] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[13] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[14] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[15] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[16] == T)]), 
             size = 0.25) +
  theme(legend.position = "none")

#### War ####
war <- ggplot(data = keywords.usTrd[1:116,]) +
  geom_bar(stat = "identity", aes(x = id, y = wkN/2), alpha = 0.3) +
  scale_y_continuous(sec.axis = sec_axis(~.*2, name = "# of Articles Each Week", breaks = seq(0,12,2))) +
  geom_line(aes(x = id, y = war, colour = "war/War"), size = 1) + 
  labs(y = "Avg Freq per Article per Week", colour = "Stem/Word") +
  #ylim(0,0.09) +
  scale_x_continuous(name = "Week", breaks = seq(10,110,20), labels = keywords.usTrd$yrwk[seq(10,110,20)]) +
  ggtitle("Weekly Avg Keyword Frequency - War") +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[1] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[2] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[3] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[4] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[5] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[6] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[7] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[8] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[9] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[10] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[11] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[12] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[13] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[14] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[15] == T)]), 
             size = 0.25) +
  geom_vline(aes(xintercept = keywords.usTrd$id[which(keywords.usTrd$yrwk %in% usPol$yrwk[16] == T)]), 
             size = 0.25) +
  theme(legend.position = "none")

#### Multi-Plots ####
ggarrange(disc, dis, ncol = 2, nrow = 1)
ggarrange(fri, heg, ncol = 2, nrow = 1)
ggarrange(neg, pur, ncol = 2, nrow = 1)
ggarrange(str, unf, ncol = 2, nrow = 1)
ggarrange(uni, war, ncol = 2, nrow = 1)

