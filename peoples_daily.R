
library(RCurl)
library(rvest)
library(httr)

pages <- paste0("http://search.people.com.cn/cnpeople/search.do?pageNum=",
                2:1126,
                "&keyword=%CC%D8%C0%CA%C6%D5&siteName=news&facetFlag=true&nodeType=belongsId&nodeId=0")

urls <- NULL
for(i in pages) {
  test <- NULL
  while(is.null(test) | class(test)=="try-error") {
    test <- try(download.file(i, destfile = "/Users/christianbaehr/Downloads/scrapedpage.html", quiet=TRUE))
  }
  
  webpage <- read_html("/Users/christianbaehr/Downloads/scrapedpage.html")
  temp <- unique(grep(".people.com.cn/", html_attr(html_nodes(webpage, "a"), "href"), value=T))
  urls <- append(urls, temp)
}
urls <- unique(urls)

# webpage <- read_html(urls[2])
# pages <- unique(grep(".people.com.cn/", html_attr(html_nodes(webpage, "a"), "href"), value=T))

text_data <- as.data.frame(matrix(data = NA, nrow = 1, ncol = 5))[0,]
names(text_data) <- c("title", "date", "source", "text", "url") 

for(i in urls) {
  
  temp <- ifelse(any(class(try(read_html(i)))=='try-error'), 'error', 'good')
  
  if(temp=='good') {
    article <- read_html(i)
    title <- as.character(html_text(html_nodes(article, xpath = '/html/body/div[@class="clearfix w1000_320 text_title"]/h1')))
    date <- as.character(html_text(html_nodes(article, xpath = '/html/body/div[@class="clearfix w1000_320 text_title"]/div/div[@class="fl"]')))
    source <- as.character(html_text(html_nodes(article, xpath = '/html/body/div[@class="clearfix w1000_320 text_title"]/div[@class="box01"]/div[@class="fl"]/a')))
    text <- as.character(html_text(html_nodes(article, xpath = '//*[@id="rwb_zw"]')))
    
    row <- c(ifelse(length(title)==0, "error", title),
             ifelse(length(date)==0, "error", date),
             ifelse(length(source)==0, "error", source),
             ifelse(length(text)==0, "error", text),
             ifelse(length(i)==0, "error", i))
    
    text_data[(nrow(text_data)+1),] <- row
  }
  
  else {
    x <- (nrow(text_data)+1)
    text_data[x,] <- "error"
    text_data$url[x] <- i
  }
}

x <- content(GET('http://korea.people.com.cn/n1/2019/0207/c407881-30615267.html'), "raw")
guess_encoding(x)

a=read_html('http://korea.people.com.cn/n1/2019/0207/c407881-30615267.html', encoding = "ISO-8859-1")
title <- as.character(html_text(html_nodes(a, xpath = '/html/body/div[@class="clearfix w1000_320 text_title"]/h1')))




<div class="clearfix w1000_320 text_title">
  <h3 class="pre"></h3>
  <h1>特朗普说美朝领导人2月底将在越南举行第二次会晤</h1>
  <h4 class="sub"></h4>
  <p class="author">记者&nbsp;刘晨&nbsp;朱东阳</p>
  <div class="box01">
  <div class="fl">2019年02月07日11:23&nbsp;&nbsp;来源：<a href="http://www.xinhuanet.com/" target="_blank">新华网</a></div>
  <div class="fr">
  <div class="fx">
  <div id="ops_share"><div class="ops_shareLayer"><span class="ops_tit">分享到：</span><ul class="ops_icons"><li><a href="javascript:void(0)" onclick="_opsShare.share('icon_rmwb');return false;" class="icon_rmwb" title="人民微博"><i> </i></a></li><li><a href="javascript:void(0)" onclick="_opsShare.share('icon_sina');return false;" class="icon_sina" title="新浪微博"><i> </i></a></li><li><a href="javascript:void(0)" onclick="_opsShare.share('icon_weixin');return false;" class="icon_weixin" title="微信"><i> </i></a></li><li><a href="javascript:void(0)" onclick="_opsShare.share('icon_qzone');return false;" class="icon_qzone" title="QQ空间"><i> </i></a></li><li><a href="javascript:void(0)" onclick="_opsShare.share('icon_copy');return false;" class="icon_copy" title="复制地址"><i> </i></a></li></ul></div></div>
  <script src="http://www.people.com.cn/img/2016wb/share_qr.min.js" charset="utf-8"></script>
  </div>
  <div class="message" id="rwb_bbstop"><a href="http://bbs1.people.com.cn/postLink.do?nid=30615267" target="_blank"><img src="http://www.people.com.cn/img/2016wb/images/icon04.jpg" width="29" height="23"></a>&nbsp;</div>
  </div>
  </div>
  </div>





'/html/body/div[@class="clearfix w1000_320 text_title"]/h1'



article <- read_html(pages[2])
title <- as.character(html_text(html_nodes(article, xpath = '/html/body/div[@class="clearfix w1000_320 text_title"]/h1')))
source <- as.character(html_text(html_nodes(article, xpath = '/html/body/div[@class="clearfix w1000_320 text_title"]/div[@class="box01"]/div[@class="fl"]/a')))
text <- html_text(html_nodes(article, xpath = '//*[@id="rwb_zw"]/p[12]'))










editorial_data <- read.csv("data/inputData/china_daily_pre_data.csv", stringsAsFactors = F)

for(i in 1:length(editorial_urls)) {
  article <- read_html(editorial_urls[i])
  title <- as.character(html_text(html_nodes(article, xpath = '//*[@id="lft-art"]/h1')))
  date_published <- as.character(html_text(html_nodes(article, ".info_l")))
  text <- as.character(html_text(html_nodes(article, xpath = '//*[@id="Content"]')))
  
  editorial_data <- rbind(editorial_data, c(title, date_published, text))
  
  if(i%%100==0) {cat(i, "of", length(editorial_urls), "\n")}
}