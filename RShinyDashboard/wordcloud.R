install.packages("tm")
library(tm)
library(tidyverse)
library(dplyr)
install.packages("wordcloud")
library(wordcloud)
install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("wordcloud2")
library(wordcloud2)
publications = read.csv("USC_SDG1to16_Pub_list (1).csv")
publications = publications[publications$Year %in% c(2020,2021,2022),]


View(publications)

text_1 <- publications %>%
  filter(Primary.SDG == 3) %>%
  slice(1:1000) %>%
  select(Abstract)
docs <- Corpus(VectorSource(text_1))
docs <- docs %>% tm_map(removeNumbers) %>% tm_map(removePunctuation) %>% tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, c(stopwords("english"),"used","also","using","ghg","found","model","models-","soc","across","including","will","well","csr","use","authors","high","whether","findings","higher","ses","per","can","however","time","two","show","may","-well","data","years","adds","days","among","total","results","text","risk","low","one","work")) 
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
set.seed(1234) # for reproducibility 
wordcloud(words = df$word, freq = df$freq, min.freq = 1,max.words=50, random.order=FALSE, rot.per=0,colors='#4C9F38')

