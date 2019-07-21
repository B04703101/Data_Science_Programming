library(dplyr)
library(magrittr)
library(ggplot2)

#讀取及清洗資料

data <-  read.csv("elderly_data.csv")

  m1 <- mutate(data, ratio = 老年人口 / 人口數) %>% 
  arrange(desc(ratio)) 


#展示長條圖
data1 = head(select(m1, X , ratio))


  ggplot( data1, aes(x = X, y = ratio))+
    geom_bar(stat = "identity", width = 0.5)+
    labs(title = "老年人口比例前六高長條圖", 
         x = "行政區名稱", y = "65歲以上人口佔總人口比例")+
    theme(
      plot.title = element_text(color="black", size=8, face="plain",hjust = 0.5),
      axis.title.x = element_text(color="black", size=8, face="plain"),
      axis.title.y = element_text(color="black", size=8, face="plain")
    )


