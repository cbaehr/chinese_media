
setwd("/Users/christianbaehr/github/chinese_media/test")
# install_github("bmschmidt/wordVectors")
# install_github("mukul13/rword2vec")
# library(rword2vec)

bin_to_txt("/Users/christianbaehr/GitHub/chinese_media/word2vec-api-master/GoogleNews-vectors-negative300.bin",
           "/Users/christianbaehr/Downloads/vec.txt")

library(rword2vec)
dist=distance(file_name = "/Users/christianbaehr/GitHub/chinese_media/word2vec-api-master/GoogleNews-vectors-negative300.bin",search_word = "king",num = 10)
dist





