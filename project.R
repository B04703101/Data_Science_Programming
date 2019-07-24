
# source
# https://www.theramenrater.com/
# https://www.kaggle.com/residentmario/ramen-ratings
# data size: 2580 


library(dplyr)
library(magrittr)
library(ggplot2)
library(tm)
library(tokenizers)
library(wordcloud)

data <-  read.csv("ramen.csv")
data$Stars <- as.numeric(levels(data$Stars))[data$Stars] 
data <- data[complete.cases(data$Stars), ]

data %>%
  group_by(Country) %>%
  summarise(avg_star = mean(Stars), var_star = var(Stars)) %>% 
  print.data.frame()
  


c1 <- group_by(data, Country) %>% 
  summarise(avg_star = mean(Stars), count = n()) %>% 
  arrange(desc(avg_star)) %>% 
  filter (count > 100) 



d1 <- rbind.data.frame(head(c1,3),tail(c1,3))


ggplot( d1, aes(x = reorder(Country, -avg_star), y = avg_star))+
  geom_bar( aes(fill = avg_star>4), position = 'dodge', col = 'transparent', stat = "identity", width = 0.5)+
  labs(title = "各國泡麵評分", 
       x = "國家", y = "分數")+
  ylim(0,5)+
   scale_fill_discrete(guide = 'none') +
  theme(
    plot.title = element_text(color="black", size=8, face="plain",hjust = 0.5),
    axis.title.x = element_text(color="black", size=8, face="plain"),
    axis.title.y = element_text(color="black", size=8, face="plain")
  )


data$Variety <- as.character(levels(data$Variety))[data$Variety] 

variety <-  ""
for (i in 1:ncol(data)){
  variety <- paste(data[,3], collapse = "")
}


Encoding(variety)  <- "UTF-8"


variety <- variety %>% 
  removePunctuation() %>% 
  removeNumbers() %>% 
  tolower()

#different to tokenize
txtList <- lapply(variety, strsplit," ")
txtChar <-  unlist(txtList)
txtChar <-  txtChar[txtChar!=""]

word_data <-  as.data.frame(table(txtChar))
colnames(word_data) <-  c("Word","freq")
wordFreq <-  word_data[order(word_data$freq,decreasing=T),]

#remove stopword
df <- data.frame(stopwords("english"))
antiWord <-  data.frame(df, stringsAsFactors=F) 
names( antiWord) <- "Word"
result <-  anti_join(wordFreq, antiWord,  by = c("Word" = "Word") ) %>% 
  arrange(desc(freq))

#remove genenal words
result <- result[-1:-3,]

wordcloud(words = result$Word, freq = result$freq, scale = c(4, 1) ,min.freq = 40,
          max.words=200, random.order=FALSE, rot.per=0.1, 
          colors=brewer.pal(8, "Dark2"))

favor <- rbind(result[1,], result[2,], result[5,], result[9,], result[11,],
               result[14,], result[16,], result[17,], result[20,], result[23,])


#way2
tok <- tokenize_words(variety)
tok_cha <-  unlist(tok)








  
