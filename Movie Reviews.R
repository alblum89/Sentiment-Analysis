#import libraries to work with
library(plyr)
library(stringr)
library(e1071)
library(RTextTools)
library(proxy)
library(ROCR)
#load up word polarity list and format it
afinn_list <- read.delim(file='C:\\Users\\Alex\\Documents\\Teachers College\\Masters Thesis\\AFINN-111.txt', header=FALSE, stringsAsFactors=FALSE)
names(afinn_list) <- c('word', 'score')
afinn_list$word <- tolower(afinn_list$word)    

#categorize words as very negative to very positive and add some movie-specific words
vNegTerms <- afinn_list$word[afinn_list$score==-5 | afinn_list$score==-4]
negTerms <- c(afinn_list$word[afinn_list$score==-3 | afinn_list$score==-2 | afinn_list$score==-1], "second-rate", "moronic", "third-rate", "flawed", "juvenile", "boring", "distasteful", "ordinary", "disgusting", "senseless", "static", "brutal", "confused", "disappointing", "bloody", "silly", "tired", "predictable", "stupid", "uninteresting", "trite", "uneven", "outdated", "dreadful", "bland")
posTerms <- c(afinn_list$word[afinn_list$score==3 | afinn_list$score==2 | afinn_list$score==1], "first-rate", "insightful", "clever", "charming", "comical", "charismatic", "enjoyable", "absorbing", "sensitive", "intriguing", "powerful", "pleasant", "surprising", "thought-provoking", "imaginative", "unpretentious")
vPosTerms <- c(afinn_list$word[afinn_list$score==5 | afinn_list$score==4], "uproarious", "riveting", "fascinating", "dazzling", "legendary")    

#load up positive and negative sentences and format
posText <- read.delim(file='C:\\Users\\Alex\\Documents\\Teachers College\\Masters Thesis\\rt-polaritypos.txt', header=FALSE, stringsAsFactors=FALSE)
posText <- posText$V1
posText <- unlist(lapply(posText, function(x) { str_split(x, "\n") }))
negText <- read.delim(file='C:\\Users\\Alex\\Documents\\Teachers College\\Masters Thesis\\rt-polarityneg.txt', header=FALSE, stringsAsFactors=FALSE)
negText <- negText$V1
negText <- unlist(lapply(negText, function(x) { str_split(x, "\n") }))    

#function to calculate number of words in each category within a sentence
sentimentScore <- function(sentences, vNegTerms, negTerms, posTerms, vPosTerms){
  final_scores <- matrix('', 0, 5)
  scores <- laply(sentences, function(sentence, vNegTerms, negTerms, posTerms, vPosTerms){
    initial_sentence <- sentence
    #remove unnecessary characters and split up by word 
    sentence <- gsub('[[:punct:]]', '', sentence)
    sentence <- gsub('[[:cntrl:]]', '', sentence)
    sentence <- gsub('\\d+', '', sentence)
    sentence <- tolower(sentence)
    wordList <- str_split(sentence, '\\s+')
    words <- unlist(wordList)
    #build vector with matches between sentence and each category
    vPosMatches <- match(words, vPosTerms)
    posMatches <- match(words, posTerms)
    vNegMatches <- match(words, vNegTerms)
    negMatches <- match(words, negTerms)
    #sum up number of words in each category
    vPosMatches <- sum(!is.na(vPosMatches))
    posMatches <- sum(!is.na(posMatches))
    vNegMatches <- sum(!is.na(vNegMatches))
    negMatches <- sum(!is.na(negMatches))
    score <- c(vNegMatches, negMatches, posMatches, vPosMatches)
    #add row to scores table
    newrow <- c(initial_sentence, score)
    final_scores <- rbind(final_scores, newrow)
    return(final_scores)
  }, vNegTerms, negTerms, posTerms, vPosTerms)
  return(scores)
}    

#build tables of positive and negative sentences with scores
posResult <- as.data.frame(sentimentScore(posText, vNegTerms, negTerms, posTerms, vPosTerms))
negResult <- as.data.frame(sentimentScore(negText, vNegTerms, negTerms, posTerms, vPosTerms))
posResult <- cbind(posResult, 'positive')
colnames(posResult) <- c('sentence', 'vNeg', 'neg', 'pos', 'vPos', 'sentiment')
negResult <- cbind(negResult, 'negative')
colnames(negResult) <- c('sentence', 'vNeg', 'neg', 'pos', 'vPos', 'sentiment')    

#combine the positive and negative tables
results <- rbind(posResult, negResult)
results2 <- cbind.data.frame(results$sentence, results$sentiment)
colnames(results2) <- c("Sentence", "Sentiment")
results3 <- results2[sample(1:10663, size=4000, replace = FALSE),]
results4 <- results2[sample(1:10663, size=500, replace = FALSE),]

###Results matrix and containers
results_matrix <- create_matrix(results3$Sentence, language="english", removeNumbers=TRUE, stemWords = FALSE,
                                weighting = tm::weightTfIdf)
results_matrix_tf <- create_matrix(results3$Sentence, language="english", removeNumbers=TRUE, stemWords = TRUE,
                                weighting = tm::weightTf)
results_matrixNet <- create_matrix(results4$Sentence, language="english", removeNumbers=TRUE, stemWords = FALSE,
                                weighting = tm::weightTfIdf)
results_matrix_tfNet <- create_matrix(results4$Sentence, language="english", removeNumbers=TRUE, stemWords = FALSE,
                                   weighting = tm::weightTf)
TextContainer <- create_container(results_matrix, as.numeric(as.factor(results3$Sentiment)), trainSize=1:3000,
                                  testSize=3001:4000, virgin=FALSE)
TextContainer_tf <- create_container(results_matrix_tf, as.numeric(as.factor(results3$Sentiment)), trainSize=1:3001,
                                  testSize=3001:4000, virgin=FALSE)

TextContainerNet <- create_container(results_matrixNet, as.numeric(as.factor(results4$Sentiment)), trainSize=1:375,
                                  testSize=376:500, virgin=FALSE)
TextContainer_tfNet <- create_container(results_matrix_tfNet, as.numeric(as.factor(results4$Sentiment)), trainSize=1:375,
                                     testSize=376:500, virgin=FALSE)
set.seed(85)
SVM <- train_model(TextContainer,"SVM")
MAXENT <- train_model(TextContainer,"MAXENT")
NNET <- train_model(TextContainerNet,"NNET")

SVM_tf <- train_model(TextContainer_tf,"SVM")
MAXENT_tf <- train_model(TextContainer_tf,"MAXENT")
NNET_tf <- train_model(TextContainer_tfNet,"NNET")

SVM_CLASSIFY <- classify_model(TextContainer, SVM)
MAXENT_CLASSIFY <- classify_model(TextContainer, MAXENT)
NNET_CLASSIFY <- classify_model(TextContainerNet, NNET)


SVM_CLASSIFY_tf <- classify_model(TextContainer_tf, SVM_tf)
MAXENT_CLASSIFY_tf <- classify_model(TextContainer_tf, MAXENT_tf)

analytics1 <- create_analytics(TextContainer, SVM_CLASSIFY)
docsTDM <- as.matrix(results_matrix_tf)
docsdissim <- proxy::dist(docsTDM, method = "cosine")
kmeans <- kmeans(docsdissim, k=6)
plot(h, sub = "")

write.csv(analytics@document_summary, "DocumentSummary.csv")
set.seed(8589)
N=10

##### CV ########
SVM_CV <- cross_validate(TextContainer, N, "SVM")
MAXENT_CV <- cross_validate(TextContainer, N, "MAXENT")
NNET_CV <- cross_validate(TextContainerNet, N, "NNET", size = 3, maxitnnet = 10)

SVM_CV_tf <- cross_validate(TextContainer_tf, N, "SVM")
MAXENT_CV_tf <- cross_validate(TextContainer_tf, N, "MAXENT")
NNET_CV_tf <- cross_validate(TextContainer_tfNet, N, "NNET", size = 2)

   

#run the naive bayes algorithm using all four categories
classifier <- naiveBayes(results[,2:5], results[,6])
naive_pred <- predict(classifier, results)
confusionMatrix(naive_pred, results$sentiment)

#display the confusion table for the classification ran on the same data
confTable <- table(predict(classifier, results), results[,6], dnn=list('predicted','actual'))
confTable    

confTable2 <- matrix(c(396, 144, 141, 319), ncol=2, byrow = TRUE)
colnames(confTable2) <- c("Positive", "Negative")
rownames(confTable2) <- c("Positive", "Negative")
#run a binomial test for confidence interval of results
binom.test(confTable[1,1] + confTable[2,2], nrow(results), p=0.5)
binom.test(confTable2[1,1] + confTable2[2,2], n=1000, p=0.5)

t.test(SVM_CLASSIFY$SVM_PROB, mu=.5)
t.test(MAXENT_CLASSIFY$MAXENTROPY_PROB, mu=.5)
t.test(SVM_CLASSIFY_tf$SVM_PROB, mu=.5)
t.test(MAXENT_CLASSIFY_tf$MAXENTROPY_PROB, mu=.5)

