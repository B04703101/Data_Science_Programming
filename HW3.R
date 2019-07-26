library(NLP)
library(xml2)
library(tm)
library(tmcn)
library(RColorBrewer)
library(tmap)
library(jiebaRD)
library(jiebaR)
library(wordcloud)
library(dplyr)
library(readr)


#Ū�J���R�奻
file <- read_file("corpus.txt")
docs <- Corpus(VectorSource(file))
toSpace <- content_transformer(function(x, pattern) {
  return (gsub(pattern, "", x))
}

#�������I�B�ťաB�Ʀr
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, stripWhitespace)

#�_��
mixseg <-  worker()
jieba_tokenizer=function(d){
  unlist(segment(d[[1]],mixseg))
}

seg <-  lapply(docs, jieba_tokenizer)

#Ū�J���ε��A�æs���r��
f <- readLines('stopword.txt', encoding = 'UTF-8')

stopwords <- c(NULL)

for(i in 1:length(f)){
  stopwords[i] <- f[i]
}

#�������ε�
segWords <- filter_segment(seg,stopwords)


#�s��dataframe�A�z��r�W
freqFrame <-  as.data.frame(table(unlist(segWords)))


freqFrame <- filter(freqFrame, Freq > 10)

#ø�s��r��


wordcloud(freqFrame$Var1,freqFrame$Freq, min.freq = 10,
          max.words=200, random.order=FALSE, rot.per=0.2, 
          colors=brewer.pal(8, "Dark2"))

