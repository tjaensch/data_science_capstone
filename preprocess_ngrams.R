rm(list=ls())
gc()

load('allBigrams.RData')
load('allTrigrams.RData')
load('allFourgrams.RData')

allBigrams   <- allBigrams[  allBigrams$freq > 2,]
allTrigrams  <- allTrigrams[ allTrigrams$freq > 2,]
allFourgrams <- allFourgrams[allFourgrams$freq > 2,]

allBigrams$terms   <- as.character(allBigrams$terms)
allTrigrams$terms  <- as.character(allTrigrams$terms)
allFourgrams$terms <- as.character(allFourgrams$terms)

save(allBigrams,   file='bigrams.RData')
save(allTrigrams,  file='trigrams.RData')
save(allFourgrams, file='fourgrams.RData')
