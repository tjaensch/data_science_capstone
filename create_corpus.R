rm(list=ls())
gc()

textBlogs <- readLines('en_US.blogs.txt', encoding = 'UTF-8')
textTwitter <- readLines('en_US.twitter.txt', encoding = 'unknown')
textNews <- readLines('en_US.news.txt', encoding = 'UTF-8')

getSampleByPercent<-function(textSource, percent) {
    
    textLength<-length(textSource)
    sampleIndex <- sample(1:textLength, as.integer(textLength*percent/100), replace=F )
    
    textSource[sampleIndex]    
}


sampleBlogs <- getSampleByPercent(textBlogs, 5)
sampleTwitter <- getSampleByPercent(textTwitter, 5)
sampleNews <- getSampleByPercent(textNews, 5)


textForCorpus <- c( sampleBlogs, sampleTwitter, sampleNews )

textForCorpus <- iconv(textForCorpus, 'latin1', 'ASCII', sub=' ');
textForCorpus <- gsub('[^[:alpha:][:space:][:punct:]]', '', textForCorpus);

save(textForCorpus, file='textForCorpus.RData')
