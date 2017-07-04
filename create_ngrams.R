rm(list=ls())
gc()

if (!file.exists('temp')) {
    dir.create('temp')
}

library(tm)
library(SnowballC)
library(RWeka)

load(file='textForCorpus.RData')

tokenizeBigram   <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tokenizeTrigram  <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
tokenizeFourgram <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))

# Process source data in chunks

linesInChunk <- 1000
numberOfIterations <- as.integer( length(textForCorpus) / linesInChunk )

# Creating separate frequency tables and saving to temporary files
for (i in 0:numberOfIterations) {

    startPosition <- i*linesInChunk+1
    endPosition <- min( (i+1)*linesInChunk, length(textForCorpus) )
 
    linesToProcess <- textForCorpus[ startPosition:endPosition ] 
    
    partialCorpus <- VCorpus(VectorSource(linesToProcess))
    
    partialCorpus <- tm_map(partialCorpus, content_transformer(tolower))
    partialCorpus <- tm_map(partialCorpus, stripWhitespace)
    partialCorpus <- tm_map(partialCorpus, removeNumbers)
    partialCorpus <- tm_map(partialCorpus, removePunctuation)
    
    tdmBigrams   <- TermDocumentMatrix(partialCorpus, control = list(tokenize = tokenizeBigram))
    tdmTrigrams  <- TermDocumentMatrix(partialCorpus, control = list(tokenize = tokenizeTrigram))
    tdmFourgrams <- TermDocumentMatrix(partialCorpus, control = list(tokenize = tokenizeFourgram))
        
    # Sort by descending frequency
    
    sortedFreqBigrams   <- sort(rowSums(as.matrix(tdmBigrams)),   decreasing=TRUE)
    sortedFreqTrigrams  <- sort(rowSums(as.matrix(tdmTrigrams)),  decreasing=TRUE)
    sortedFreqFourgrams <- sort(rowSums(as.matrix(tdmFourgrams)), decreasing=TRUE)

    dataBigrams    <- data.frame(terms=names(sortedFreqBigrams),   freq=sortedFreqBigrams)
    dataTrigrams   <- data.frame(terms=names(sortedFreqTrigrams),  freq=sortedFreqTrigrams)
    dataFourgrams  <- data.frame(terms=names(sortedFreqFourgrams), freq=sortedFreqFourgrams)
        
    save(dataBigrams,   file=paste('temp/2-',i,'.RData',sep=''))
    save(dataTrigrams,  file=paste('temp/3-',i,'.RData',sep=''))
    save(dataFourgrams, file=paste('temp/4-',i,'.RData',sep=''))
    
    print(i)
}

allBigrams   <- dataBigrams[0,]
allTrigrams  <- dataTrigrams[0,]
allFourgrams <- dataFourgrams[0,]

mergeTwoData <- function (data1, data2){
    
    data1 <- data1[order(data1$terms),]
    data2 <- data2[order(data2$terms),]
    
    data1$freq[data1$terms %in% data2$terms] <- data1$freq[data1$terms %in% data2$terms] + data2$freq[data2$terms %in% data1$terms]

    newdata <- rbind(data1, data2[!(data2$terms %in% data1$terms),])
    newdata
}

# Merging frequency tables from temporary files
for (i in 0:numberOfIterations) {
 
    load(paste('temp/2-',i,'.RData',sep='')) #dataBigrams
    load(paste('temp/3-',i,'.RData',sep='')) #dataTrigrams
    load(paste('temp/3-',i,'.RData',sep='')) #dataFourgrams
    
    #allUnigrams  <- mergeTwoData( allUnigrams,  dataUnigrams )    
    allBigrams   <- mergeTwoData( allBigrams,   dataBigrams )    
    allTrigrams  <- mergeTwoData( allTrigrams,  dataTrigrams )    
    allFourgrams <- mergeTwoData( allFourgrams, dataFourgrams )   
    
    print(i)
}

save(allBigrams,   file='allBigrams.RData')
save(allTrigrams,  file='allTrigrams.RData')
save(allFourgrams, file='allFourgrams.RData')
