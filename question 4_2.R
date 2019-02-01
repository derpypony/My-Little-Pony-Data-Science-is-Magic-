options(stringsAsFactors = F)
library(tidytext)
library(tidyverse)
writer_score <- read_csv("writer_score.csv")
full_mlp_script <- read_csv("full_mlp_script.csv")
writer_score$'Twilight Sparkle' <- 0
writer_score$'Pinkie Pie' <- 0
writer_score$'Rarity' <- 0
writer_score$'Rainbow Dash' <- 0
writer_score$'Applejack' <- 0
writer_score$'Fluttershy' <- 0
# Creat my own sentiment words
my_sentiment <- rbind(get_sentiments('b'), 
                      data.frame(word=c('no','not',"don't","haven't","won't", "never", "can't", "couldn't", "without"),
                                 sentiment='negative'))
# Please note tht there are dulpicate in my_sentiment, remove duplicate
my_sentiment <- my_sentiment[!duplicated(my_sentiment$word),]



for(i in 1:26)
{
  for(j in 1:6)
  {
    Pony <- names(writer_score)[j+2]
    Writer <- writer_score$writer[i]
    pony_writer <- full_mlp_script %>% filter(str_detect(pony,Pony)&str_detect(writer,Writer))
    len <- length(pony_writer$episode)
    tidy_data <- pony_writer %>% unnest_tokens(word,dialoag) %>% inner_join(my_sentiment)
    sent <- tidy_data %>% count(sentiment)
    writer_score[i,(j+2)] <- (sent$n[2]-sent$n[1])/len
  }
}
# because Kevin Lappin didn't even give rainbowdash any lines, we have NA

write.csv(writer_score,'sent for pony and writer.csv', row.names = F)

# please note that we will use 'Fox' to represent 'Michael P. Fox & Wil Fox',
# 'Joanna Lewis' to represent 'Joanna Lewis & Kristine Songco', 
# 'Kevin Burke' to represent 'Kevin Burke & Chris "Doc" Wyatt'
sent_for_pony_and_writer$writer <- str_replace(sent_for_pony_and_writer$writer,'Fox', 'Michael P. Fox & Wil Fox')
sent_for_pony_and_writer$writer <- str_replace(sent_for_pony_and_writer$writer,'Joanna Lewis', 'Joanna Lewis & Kristine Songco')
sent_for_pony_and_writer$writer <- str_replace(sent_for_pony_and_writer$writer,'Kevin Burke', 'Kevin Burke & Chris "Doc" Wyatt')

sent_for_pony_and_writer$writer <- paste0(sent_for_pony_and_writer$writer, ' (', sent_for_pony_and_writer$N, ' episodes)')

tidy_sent <- sent_for_pony_and_writer %>% select(-2) %>% gather(names(sent_for_pony_and_writer)[-c(1,2)], key=pony, value=prop)

# Using the same techenique as in question 1, 
# and we use prop to represent sentiment score
# you can only change the names of ponies to recycle the following code
A_R_least<- tidy_sent %>% filter(pony=='Fluttershy'|pony=='Twilight Sparkle') %>% 
  group_by(pony) %>% top_n(5, wt=-prop) %>% ungroup() %>% mutate(color='Top 5 most negative')

A_R_most<- tidy_sent %>% filter(pony=='Fluttershy'|pony=='Twilight Sparkle') %>% 
  group_by(pony) %>% top_n(5, wt=prop) %>% ungroup() %>% mutate(color='Top 5 most positive')

A_R <- rbind(A_R_least,A_R_most)


new_A_R <- A_R %>% arrange(pony,prop) %>% mutate(order=1:20)


new_A_R %>% ggplot(aes(x=order,y=prop,fill=color))+geom_col()+facet_wrap(~pony, scales='free_y')+
  coord_flip()+labs(x='', y='# Average number of positive words minus negative words per line', 
                    subtitle='The top 5 writers who give them most positive and negative personalities',
                    title='Fluttershy and Twilight Sparkle personalities')+
  scale_x_continuous(
    breaks = new_A_R$order,
    labels = new_A_R$writer,
    expand = c(0,0)
  )
