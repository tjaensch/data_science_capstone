library(shiny)
library(tm)
library(stringr)
library(ggplot2)
library(RColorBrewer)

load('bigrams.RData')
load('trigrams.RData') 
load('fourgrams.RData')

cleanQuery <- function(string)
{
    
    string <- iconv(string, 'latin1', 'ASCII', sub=' ');
    string <- gsub('[^[:alpha:][:space:][:punct:]]', '', string);
    
    # we use same routine like in 'createNGrams' to ensure same method of text preparation
    corpus <- VCorpus(VectorSource(string))
    
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, removePunctuation)
    
    # take it back from corpus
    string <- as.character(corpus[[1]])
    string <- gsub('(^[[:space:]]+|[[:space:]]+$)', '', string)
    
    if (is.na(string)) {
        string <- ''
    }
    
    string
}


starts_with <- function(vars, match, ignore.case = TRUE) {
    if (ignore.case) match <- tolower(match)
    n <- nchar(match)
    
    if (ignore.case) vars <- tolower(vars)
    substr(vars, 1, n) == match
}

# if threshold=0, it takes random
getWord<-function(ngrams, freqThreshold=1) {
    
    result<-''
    
    if (freqThreshold>1 | freqThreshold<0) {
        freqThreshold<-1
    }
    
    if (nrow(ngrams)>0) {
        
        # take random, if there are some with equal maximum frequency
        # if we use freqThreshold, take random from terms with frequency > x*max
        # this means, if threshold=0.75, it takes random one from top quarter
        
        maxFrequency <- max(ngrams$freq)
        topResults <- ngrams[ ngrams$freq>=as.integer(maxFrequency*freqThreshold), ] 
        
        result<-sample(topResults$terms,1)
    }
    
    result<-word(result,-1)
}


predictNextWord <- function(string, freqThreshold=1)
{
    string <- cleanQuery(string)
    
    stringVector <- unlist(strsplit(string, split=' '));
    queryLength <- length(stringVector);
    
    prediction <- ''
    
    usedN <- 0
    usedForChoice<-data.frame()
    
    # Looking for four-grams
    if (queryLength>=3)
    {
        last3Words <- paste(paste(stringVector[(queryLength-2):queryLength], collapse=' '),' ', sep='');
        
        searchIndex <- starts_with(allFourgrams$terms, last3Words)
        foundFourgrams<- allFourgrams[searchIndex, ];
        prediction<-getWord(foundFourgrams, freqThreshold)
        
        usedN <- 4
        usedForChoice<-foundFourgrams
        
    }
    
    # Looking for tri-grams
    if (queryLength>= 2 & prediction=='')
    {
        
        last2Words <- paste(paste(stringVector[(queryLength-1):queryLength], collapse=' '),' ', sep='');
        
        searchIndex <- starts_with(allTrigrams$terms, last2Words)
        foundTrigrams<- allTrigrams[searchIndex, ];
        prediction<-getWord(foundTrigrams, freqThreshold)
        
        usedN <- 3
        usedForChoice<-foundTrigrams
    }
    
    # Looking for bi-grams
    if (queryLength>= 1 & prediction=='')
    {
        lastWord <- paste(stringVector[queryLength],' ',sep='')
        
        searchIndex <- starts_with(allBigrams$terms, lastWord)
        
        foundBigrams<- allBigrams[searchIndex, ];
        prediction<-getWord(foundBigrams, freqThreshold)
        
        usedN <- 2
        usedForChoice<-foundBigrams
    }
    
    list(prediction=prediction, depth=usedN, ngram=usedForChoice, threshold=freqThreshold)
}

    
function(input, output) {
    
    dataset <- reactive({
        
        pred<-predictNextWord(input$userQuery, (input$thresholdPercents/100))
        pred
        
    })
    
    output$prediction<-renderPrint({
        
        pred<-dataset()
        
        highlight<-paste('<u><b>',pred$prediction,'</b></u>')
        if (pred$prediction=='') {
            highlight<-'<span style="color:#aaa">Word prediction</span>'
        }
        
        HTML(paste('<span style="font-size: 150%;">',input$userQuery,highlight,'</span>'))
    })

    output$somedebug<-renderPrint({
        
        pred<-dataset()

        h<-'<br/><br/>'
        
        if (nrow(pred$ngram)>0) {
        
            h<-paste(h, 'Found phrases starting with <b>',pred$depth,'-gram</b>:',nrow(pred$ngram),'<br/>',sep='')
        }
        
        HTML(h)
    })
    
    output$freqplot<-renderPlot({
        
        pred<-dataset()
        
        if (nrow(pred$ngram)>0) {
            
            plotdata <- pred$ngram
            plotdata <- head(plotdata[order(plotdata$freq, decreasing=TRUE), ], 10)
            
            thresholdLevel <- max(plotdata$freq) * pred$threshold
                
            plotdata$threshold <- plotdata$freq >= thresholdLevel
            plotdata$threshold <- factor( plotdata$threshold, levels=c(T,F) )
            
            gp<-ggplot(plotdata, aes(terms, freq))+geom_hline(yintercept = thresholdLevel, color='red')+geom_bar(stat='identity', aes(fill=threshold))+labs(x='',y='')+theme(axis.text.x = element_text(angle = 90, hjust = 1, size=14))+scale_fill_manual(labels=c('above','below'), values=c('#669966','#999999'))
            
            gp
        }
        
    }, width=400, height=400)
}
