options(stringsAsFactors = F)
library(tidyverse)
writer_score <- read_csv("writer_score.csv")
full_mlp <- read_csv("full_mlp.csv")
# remove thre shorts and a MLP movie
full_mlp <- full_mlp[-c(196,198:200),]
writer_score$Positive <- 0
writer_score$Negative <- 0
# We only consider writers who wrote more than 2 episodes
writer_score <- writer_score %>% filter(N > 2)
library(tidytext)
# Creat my own sentiment words
my_sentiment <- rbind(get_sentiments('b'), 
                      data.frame(word=c('no','not',"don't","haven't","won't", "never", "can't", "couldn't", "without"),
                                 sentiment='negative'))
# Please note tht there are dulpicate in my_sentiment, remove duplicate
my_sentiment <- my_sentiment[!duplicated(my_sentiment$word),]
for(i in 1:26)
{
  writer_name <- writer_score$writer[i]
  writer_content <- full_mlp %>% filter(str_detect(writer,writer_name))
  Total_number <- length(writer_content$writer)
  tidy_content <- writer_content %>% unnest_tokens(word,content) %>% inner_join(my_sentiment)
  score <- tidy_content %>% count(sentiment)
  writer_score[i,3] <- score$n[2]/Total_number
  writer_score[i,4] <- score$n[1]/Total_number
}
# please note that we will use 'Fox' to represent 'Michael P. Fox & Wil Fox',
# 'Joanna Lewis' to represent 'Joanna Lewis & Kristine Songco', 
# 'Kevin Burke' to represent 'Kevin Burke & Chris "Doc" Wyatt'
writer_score$writer <- str_replace(writer_score$writer,'Fox', 'Michael P. Fox & Wil Fox')
writer_score$writer <- str_replace(writer_score$writer,'Joanna Lewis', 'Joanna Lewis & Kristine Songco')
writer_score$writer <- str_replace(writer_score$writer,'Kevin Burke', 'Kevin Burke & Chris "Doc" Wyatt')

writer_score$writer <- paste0(writer_score$writer, ' (', writer_score$N, ' episodes)')
writer_score$writer <- fct_reorder(writer_score$writer,writer_score$Positive+writer_score$Negative)


question6_data <- writer_score %>% mutate(score=Positive+Negative) %>% 
  select(1,2,5) %>% mutate(type='sentiment_word')

write.csv(question6_data,'question6_data.csv',row.names = F)



new_writer_score <- writer_score %>% select(-2) %>% gather('Positive', 'Negative', key=polarity, value=sent_score)

new_writer_score %>% ggplot(aes(writer,sent_score,fill=polarity))+
  geom_col()+coord_flip()+labs(x='', y='# Positive/Negative words per episode', 
                               title = 'Sentiment words useage frequency between writers')
# now we plot positive/negative words propotion chart
writer_score$pos_prop <- writer_score$Positive/(writer_score$Positive+writer_score$Negative)
writer_score$writer <- fct_reorder(writer_score$writer,writer_score$pos_prop)

new_writer_score <- writer_score %>% select(-2) %>% gather('Positive', 'Negative', key=polarity, value=sent_score)
