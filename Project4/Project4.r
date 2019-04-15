library(RColorBrewer)
library(wordcloud)
library(tidyverse)
library(tidytext)
library(stringr)
library(caret)
library(tm)
library(e1071)
library(gmodels)
library(readr)
library(purrr)

# Set the global paths
spam.path <- file.path("C:/Users/Matt/Documents/CUNY/DATA/PROJECT4/spam")
spam2.path <- file.path("C:/Users/Matt/Documents/CUNY/DATA/PROJECT4/spam_2")
easyham.path <- file.path("C:/Users/Matt/Documents/CUNY/DATA/PROJECT4/easy_ham")
easyham2.path <- file.path("C:/Users/Matt/Documents/CUNY/DATA/PROJECT4/easy_ham_2")
hardham.path <- file.path("C:/Users/Matt/Documents/CUNY/DATA/PROJECT4/hard_ham")
hardham2.path <- file.path("C:/Users/Matt/Documents/CUNY/DATA/PROJECT4/hard_ham_2")

##FUNCTION: to analyze message body  look for the first line break in each email (from Conway and White)
get.msg <- function(path) {
  con <- file(path, open = "rt", encoding = "latin1")
  text <- readLines(con)
  if (is.numeric(which(text == "")[1]) && is.finite(which(text == "")[1]) && 
      which(text == "")[1] < length(text)) {
    msg <- text[seq(which(text == "")[1] + 1, length(text), 1)]
  } else {
    msg <- ""
  }
  close(con)
  return(paste(msg, collapse = "\n"))
}

##FUNCTION: to store spam email body, ignore cmds files (from Conway and White)
#spam
spam.docs <- dir(spam.path)
spam.docs <- spam.docs[spam.docs != "cmds"]
all.spam <- sapply(spam.docs, function(p) get.msg(file.path(spam.path, p, sep="")))

#add classifier to spam DF
spam.dfr <- as.data.frame(all.spam)
spam.dfr$goodness <- 0 #0 means spam
names(spam.dfr) <- c("text", "outcome")
head(spam.dfr, 1)[,1]


#ham
ham.docs <- dir(ham.path)
ham.docs <- ham.docs[ham.docs != "cmds"]
all.ham <- sapply(ham.docs, function(p) get.msg(file.path(ham.path, p, sep="")))

#add classifier to ham DF
ham.dfr    <- as.data.frame(all.ham)
ham.dfr$goodness <- 1 #1 means spam
names(ham.dfr) <- c("text", "outcome")
head(ham.dfr, 1)[,1]


##TDM is created by (from Conway and White)
get.tdm <- function(doc.vec) {
  control <- list(stopwords = TRUE, removePunctuation = TRUE, removeNumbers = TRUE, 
                  minDocFreq = 5)
  doc.corpus <- Corpus(VectorSource(doc.vec))
  # Construct Corpus using vector of emails
  doc.dtm <- TermDocumentMatrix(doc.corpus, control)
  return(doc.dtm)
}

#spam tdm to dataframe
spam.tdm <- get.tdm(all.spam)
mspam <- as.matrix(spam.tdm)
vspam <- sort(rowSums(mspam),decreasing=TRUE)
dspam <- data.frame(word = names(vspam),freq=vspam)

#top 20 terms in the spam corpus
dspam[1:20, 1:2]

#vspam inspect
head(mspam)

#wordcloud of spam words
set.seed(1234)
wordcloud(words = dspam$word, freq = dspam$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


#ham tdm to dataframe
ham.tdm <- get.tdm(all.ham)
mham <- as.matrix(ham.tdm)
vham <- sort(rowSums(mham),decreasing=TRUE)
dham <- data.frame(word = names(vham),freq=vham)

#top 20 terms in the spam corpus
dham[1:20, 1:2]

#wordcloud of ham words
set.seed(1234)
wordcloud(words = dham$word, freq = dspam$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(7, "Paired"))


#####################
#training and testing df (75/25)
nrow(spam.dfr)
nrow(ham.dfr)

#train
rawspamtrain.data<-(spam.dfr[1:375,]) 
hamtrain.data<- (ham.dfr[1:1050,])

#test
rawspamtest.data<-(spam.dfr[376:500,])
hamtest.data<- (ham.dfr[1051:1400,])


#combo of spam and ham train data
train.data<-rbind(rawspamtrain.data, hamtrain.data)
#combo of spam and ham test data
test.data<-rbind(rawspamtest.data, hamtest.data)

#bayes
convert_counts = function(x) {
  x = ifelse(x > 0, 1, 0)
  x = factor(x, levels = c(0, 1), labels=c("0", "1"))
  return (x)
}

#
library(e1071)
spam_classifier <- naiveBayes(spam.dfr, spam.dfr$goodness)
email_test.predicted = predict(spam_classifier,
                             ham.dtm)

#
library(gmodels)
CrossTable(email_test.predicted,
           train.data$goodness,
           prop.chisq = FALSE, # as before
           prop.t     = FALSE, # eliminate cell proprtions
           dnn        = c("predicted", "actual")) # relabels rows+cols
