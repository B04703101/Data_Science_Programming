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


#讀入分析文本
file <- read_file("corpus.txt")
docs <- Corpus(VectorSource(file))
toSpace <- content_transformer(function(x, pattern) {
  return (gsub(pattern, "", x))
}

#移除標點、空白、數字
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, stripWhitespace)

#斷詞
mixseg <-  worker()
jieba_tokenizer=function(d){
  unlist(segment(d[[1]],mixseg))
}

seg <-  lapply(docs, jieba_tokenizer)

#讀入停用詞，並存成字串
f <- readLines('stopword.txt', encoding = 'UTF-8')

stopwords <- c(NULL)

for(i in 1:length(f)){
  stopwords[i] <- f[i]
}

#移除停用詞
segWords <- filter_segment(seg,stopwords)


#存成dataframe，篩選字頻
freqFrame <-  as.data.frame(table(unlist(segWords)))


freqFrame <- filter(freqFrame, Freq > 10)

#繪製文字雲


wordcloud(freqFrame$Var1,freqFrame$Freq, min.freq = 10,
          max.words=200, random.order=FALSE, rot.per=0.2, 
          colors=brewer.pal(8, "Dark2"))


