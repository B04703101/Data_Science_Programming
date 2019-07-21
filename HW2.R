library(dplyr)
library(magrittr)
library(ggplot2)

#Ū���βM�~���

data <-  read.csv("elderly_data.csv")

  m1 <- mutate(data, ratio = �Ѧ~�H�f / �H�f��) %>% 
  arrange(desc(ratio)) 


#�i�ܪ�����
data1 = head(select(m1, X , ratio))


  ggplot( data1, aes(x = X, y = ratio))+
    geom_bar(stat = "identity", width = 0.5)+
    labs(title = "�Ѧ~�H�f��ҫe����������", 
         x = "��F�ϦW��", y = "65���H�W�H�f���`�H�f���")+
    theme(
      plot.title = element_text(color="black", size=8, face="plain",hjust = 0.5),
      axis.title.x = element_text(color="black", size=8, face="plain"),
      axis.title.y = element_text(color="black", size=8, face="plain")
    )

