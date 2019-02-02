options(stringsAsFactors = F)
library(tidytext)
library(tidyverse)
writer_score <- read_csv("writer_score.csv")
# we only focus on those who wrote more than 2 episodes
writer_score <- filter(writer_score, N > 2)
clean_dialog <- read_csv("clean_dialog.csv")
writer_score$'Twilight Sparkle' <- 0
writer_score$'Pinkie Pie' <- 0
writer_score$'Rarity' <- 0
writer_score$'Rainbow Dash' <- 0
writer_score$'Applejack' <- 0
writer_score$'Fluttershy' <- 0
# Creat my own sentiment words
my_sentiment <- rbind(get_sentiments('b'), 
                      data.frame(word=c('no','not',"don't","haven't","won't", 
                                        "never", "can't", "couldn't", "without"),
                                 sentiment='negative'))
# Please note tht there are dulpicate in my_sentiment, remove duplicate
my_sentiment <- my_sentiment[!duplicated(my_sentiment$word),]
# we count the average number of positive words minus negative words per line
for(i in 1:26)
{
  for(j in 1:6)
  {
    Pony <- names(writer_score)[j+2]
    Writer <- writer_score$writer[i]
    
    pony_writer <- clean_dialog %>% 
      filter(str_detect(pony,Pony)&str_detect(writer,Writer))
    
    len <- length(pony_writer$title)
    
    tidy_data <- pony_writer %>% unnest_tokens(word,dialog) %>% 
      inner_join(my_sentiment)
    
    sent <- tidy_data %>% count(sentiment)
    
    writer_score[i,(j+2)] <- (sent$n[2]-sent$n[1])/len
  }
}
# because Kevin Lappin didn't even give rainbowdash any lines, we have NA
# please note that we will use 'Fox' to represent 'Michael P. Fox & Wil Fox',
# 'Joanna Lewis' to represent 'Joanna Lewis & Kristine Songco', 
# 'Kevin Burke' to represent 'Kevin Burke & Chris "Doc" Wyatt'
writer_score$writer <- str_replace(writer_score$writer,'Fox', 'Michael P. Fox & Wil Fox')
writer_score$writer <- str_replace(writer_score$writer,'Joanna Lewis', 'Joanna Lewis & Kristine Songco')
writer_score$writer <- str_replace(writer_score$writer,'Kevin Burke', 'Kevin Burke & Chris "Doc" Wyatt')

writer_score$writer <- 
  paste0(writer_score$writer, ' (', writer_score$N, ' episodes)')

tidy_sent <- writer_score %>% select(-2) %>% 
  gather(names(writer_score)[-c(1,2)], key=pony, value=prop)

# Using the same techenique as in question 1, 
# and we use prop to represent sentiment score
# you can only change the names of ponies to recycle the following code

my_pony <- c('Twilight Sparkle', 'Fluttershy', 'Pinkie Pie', 'Rarity', 'Applejack', 'Rainbow Dash')

# use i and j to track ponies
# you can use three combination 
# i=1,j=2: i=3,j=4; i=5,j=6 to replicate my result
i = 5
j = 6

A_R_least<- tidy_sent %>% filter(pony==my_pony[i]|pony==my_pony[j]) %>% 
  group_by(pony) %>% top_n(5, wt=-prop) %>% ungroup() %>% mutate(color='Top 5 most negative')

A_R_most<- tidy_sent %>% filter(pony==my_pony[i]|pony==my_pony[j]) %>% 
  group_by(pony) %>% top_n(5, wt=prop) %>% ungroup() %>% mutate(color='Top 5 most positive')

A_R <- rbind(A_R_least,A_R_most)

new_A_R <- A_R %>% arrange(pony,prop) %>% mutate(order=1:20)

new_A_R %>% ggplot(aes(x=order,y=prop,fill=color))+geom_col()+facet_wrap(~pony, scales='free_y')+
  coord_flip()+labs(x='', y='# Average number of positive words minus negative words per line', 
                    subtitle='The top 5 writers who give them most positive and negative personalities',
                    title=paste0(my_pony[i],' and ', my_pony[j], ' personalities'))+
  scale_x_continuous(
    breaks = new_A_R$order,
    labels = new_A_R$writer,
    expand = c(0,0)
  )
print(paste0(my_pony[i],' and ', my_pony[j], ' personalities'))
