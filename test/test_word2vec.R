
word2vec <- function(fileName) {
  # Convert test.txt to test.bin.
  if (grepl('.txt', fileName, fixed=T)) {binaryFileName <- gsub('.txt', '.bin', fileName, fixed=T)}
  else {binaryFileName <- paste0(fileName, '.bin')}
  
  # Train word2vec model.
  if (!file.exists(binaryFileName)) {
    # Lowercase and setup ngrams.
    prepFileName <- 'temp.prep'
    prep_word2vec(origin=fileName, destination=prepFileName, lowercase=T, bundle_ngrams=2)
    
    # Train word2vec model.
    model <- train_word2vec(prepFileName, binaryFileName, vectors=200, threads=4, window=12, iter=5, negative_samples=0)
    
    # Cleanup.
    unlink(prepFileName)
  } else {model <- read.vectors(binaryFileName)}
  
  model
}

###################

setwd("/Users/christianbaehr/github/chinese_media/test")

library(devtools)
# install_github("bmschmidt/wordVectors")
# install_github("mukul13/rword2vec")
library(wordVectors)
library(rword2vec)

# bin_to_txt(bin_file = "/Users/christianbaehr/Github/chinese_media/test/GoogleNews-vectors-negative300.bin",
#            txt_file = "/Users/christianbaehr/Github/chinese_media/test/GoogleNews-vectors-negative300_txt.bin")

data <- word2vec(fileName = "/Users/christianbaehr/Github/chinese_media/test/GoogleNews-vectors-negative300_txt.bin")













