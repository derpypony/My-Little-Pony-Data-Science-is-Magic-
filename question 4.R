options(stringsAsFactors = F)
library(tidyverse)
full_mlp_script <- read_csv("full_mlp_script.csv")

full_mlp_script <- full_mlp_script %>% filter(episode!='My Little Pony The Movie')

library(NLP)
library(openNLP)
library("openNLPmodels.en", lib.loc="~/R/win-library/3.5")

sent.token.annotator <-
  Maxent_Sent_Token_Annotator(language = "en")
word.token.annotator <-
  Maxent_Word_Token_Annotator(language = "en")
pos.tag.annotator <-
  Maxent_POS_Tag_Annotator(language = "en")

# we are gonna have a list, it's length is 26, each cell is for a dataframe
body <- vector('list',26)
library(progress)
# set progression bar
pb <- progress_bar$new(total = 22)
for(i in 5:26)
{
  pb$tick()
  new_writer <- writer_score$writer[i]
  writer_script <- full_mlp_script %>% filter(str_detect(writer,new_writer))
  content <- paste(writer_script$dialoag,collapse = ' ')
  
  annotations <- annotate(content,list(sent.token.annotator,
                                       word.token.annotator,
                                       pos.tag.annotator))
  ann.df<-as.data.frame(annotations)
  
  ann.df$word <- ""
  new_length <- length(ann.df$id)
  for(j in 1:new_length)
  {
    ann.df$word[j] <- substr(content,ann.df[j,3],ann.df[j,4])
  }
  ann.df$word <- tolower(ann.df$word)
  new.ann <- ann.df %>% filter(!str_detect(word,'[:punct:]')) %>% anti_join(stop_words)
  
  new.ann$features <- unname(unlist(new.ann$features))
  
  body[[i]] <- new.ann %>% filter(str_detect(features,'JJ')) %>% count(word,sort = T) %>% top_n(50)
  
}
# looks like our computer can't handle too much data at once, so we divide
# the data of M. A. Larson, Meghan McCarthy and Josh Haber
# first we deal with M. A. Larson who has 18 edpside
writer_script <- full_mlp_script %>% filter(str_detect(writer,'M. A. Larson')) %>% 
  mutate(part = (1:3579) %/% 600 + 1)

lason <- list()
pb <- progress_bar$new(total = 6)
for(i in 1:6)
{
  pb$tick()
  content <- paste(subset(writer_script, part==i)$dialoag,collapse = ' ')
  
  annotations <- annotate(content,list(sent.token.annotator,
                                       word.token.annotator,
                                       pos.tag.annotator))
  ann.df<-as.data.frame(annotations)
  
  ann.df$word <- ""
  new_length <- length(ann.df$id)
  for(j in 1:new_length)
  {
    ann.df$word[j] <- substr(content,ann.df[j,3],ann.df[j,4])
  }
  ann.df$word <- tolower(ann.df$word)
  new.ann <- ann.df %>% filter(type=='word') %>% filter(!str_detect(word,'[:punct:]')) %>% anti_join(stop_words)
  
  new.ann$features <- unname(unlist(new.ann$features))
  
  lason[[i]] <- new.ann %>% filter(str_detect(features,'JJ'))
  
}
body[[2]] <- data.frame(word=unlist(lapply(lason, '[[', 6))) %>% count(word, sort=T) %>% top_n(50)




# second we deal with Meghan McCarthy 
writer_script <- full_mlp_script %>% filter(str_detect(writer,'Meghan McCarthy')) %>% 
  mutate(part = (1:5019) %/% 600 + 1)

McCarthy <- list()
pb <- progress_bar$new(total = 9)
for(i in 1:9)
{
  pb$tick()
  content <- paste(subset(writer_script, part==i)$dialoag,collapse = ' ')
  
  annotations <- annotate(content,list(sent.token.annotator,
                                       word.token.annotator,
                                       pos.tag.annotator))
  ann.df<-as.data.frame(annotations)
  
  ann.df$word <- ""
  new_length <- length(ann.df$id)
  for(j in 1:new_length)
  {
    ann.df$word[j] <- substr(content,ann.df[j,3],ann.df[j,4])
  }
  ann.df$word <- tolower(ann.df$word)
  new.ann <- ann.df %>% filter(type=='word') %>% filter(!str_detect(word,'[:punct:]')) %>% anti_join(stop_words)
  
  new.ann$features <- unname(unlist(new.ann$features))
  
  McCarthy[[i]] <- new.ann %>% filter(str_detect(features,'JJ'))
  
}
body[[3]] <- data.frame(word=unlist(lapply(McCarthy, '[[', 6))) %>% count(word, sort=T) %>% top_n(50)


# third we deal with Josh Haber 
writer_script <- full_mlp_script %>% filter(str_detect(writer,'Josh Haber')) %>% 
  mutate(part = (1:4397) %/% 600 + 1)

Haber <- list()
pb <- progress_bar$new(total = 8)
for(i in 1:8)
{
  pb$tick()
  content <- paste(subset(writer_script, part==i)$dialoag,collapse = ' ')
  
  annotations <- annotate(content,list(sent.token.annotator,
                                       word.token.annotator,
                                       pos.tag.annotator))
  ann.df<-as.data.frame(annotations)
  
  ann.df$word <- ""
  new_length <- length(ann.df$id)
  for(j in 1:new_length)
  {
    ann.df$word[j] <- substr(content,ann.df[j,3],ann.df[j,4])
  }
  ann.df$word <- tolower(ann.df$word)
  new.ann <- ann.df %>% filter(type=='word') %>% filter(!str_detect(word,'[:punct:]')) %>% anti_join(stop_words)
  
  new.ann$features <- unname(unlist(new.ann$features))
  
  Haber[[i]] <- new.ann %>% filter(str_detect(features,'JJ'))
  
}
body[[4]] <- data.frame(word=unlist(lapply(Haber, '[[', 6))) %>% count(word, sort=T) %>% top_n(50)



for(i in 1:26)
{
  body[[i]]$writer <- writer_score$writer[i]
}
top_adj_writer <- do.call(rbind,body)
write.csv(top_adj_writer, 'top_adj_writer.csv', row.names = F)


# you can remove all data and import writer_score.csv and top_adj_writer.csv
top_adj_writer <- read_csv("top_adj_writer.csv")
writer_score <- read_csv("writer_score.csv")
top_writer <- writer_score %>% filter(N>10)
top_adj <- top_adj_writer %>% inner_join(top_writer)
top_adj <- top_adj %>% filter(!str_detect(word,'<'))

top_adj <- top_adj %>% left_join(get_sentiments('b'))

new_top <- top_adj
new_top$sentiment[is.na(new_top$sentiment)] <- 'positive'
write.csv(new_top,'new_top.csv',row.names = F)
# now we plot word cloud
library(wordcloud)
library(reshape2)

par(mar=c(0.1,0.1,0.1,0.1))
new_top %>% filter(writer=='Nick Confalone')  %>%
  acast(word ~ sentiment, value.var = "n", fill=0) %>%
  comparison.cloud(colors = c("Red", "Blue"),
                   max.words = 100, title.size=0.001)
