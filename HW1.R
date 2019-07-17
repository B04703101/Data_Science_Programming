# import library
library(dplyr)
library(ggplot2)

#import data
GNI <-  read.csv("C:/Users/user/Documents/GNI.csv")
life <-  read.csv("C:/Users/user/Documents/life.csv")

#select data
China_gni <- select(GNI,"China")
China_life <- select(life,"China")

#change names and combine
combinatoin <- cbind(1962:2017,China_gni,China_life)

names(combinatoin) <- c("Year","GNI","life_expectancy")

head(combinatoin)

#show in ggplot2
ggplot(combinatoin, aes(x=GNI, y=life_expectancy)) + geom_point() +
 ggtitle('Relation between GNI and life expectancy in China')+
 theme(plot.title = element_text(hjust = 0.5))
