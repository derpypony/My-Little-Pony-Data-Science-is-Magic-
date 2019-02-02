options(stringsAsFactors = F)
library(tidyverse)
pony_song_data <- read_csv("pony_song_data.csv")
pony_song_data$Length[c(5,8,9,28,44,46,47,49,56,62,64,80,99)] <- 
  c('0:33','4:13','0:42','1:53','2:59','3:16','2:27','2:16','2:17','3:15','3:26','3:03','3:44')

time_data <- data.frame(do.call(rbind,str_split(pony_song_data$Length,pattern = ':')))

pony_song_data$minute <- as.numeric(time_data$X1)
pony_song_data$second <- as.numeric(time_data$X2)

pony_song_data$total_second <- 60*pony_song_data$minute+pony_song_data$second

new_song_data <- pony_song_data %>% group_by(title) %>% 
  summarise(total_second=sum(total_second))

pony_synopsis <- read_csv("pony_synopsis.csv")

writer_and_episode <- pony_synopsis %>% select(1,2)
  
episode_song <- writer_and_episode %>% 
  left_join(new_song_data)  

episode_song$total_second <- ifelse(is.na(episode_song$total_second),
                                    yes=0,no=episode_song$total_second)

writer_score <- read_csv("writer_score.csv")

writer_score$song_second <- 0
# we only focus on writers who wrote more than 2 episods
writer_score <- filter(writer_score,N > 2)
# we count seconds of songs per episode
for(i in 1:26)
{
  new_writer <- writer_score$writer[i]
  
  writer_data <- episode_song %>% 
    filter(str_detect(writer,new_writer))
  
  writer_score$song_second[i] <- mean(writer_data$total_second)
}

writer_score$writer <- str_replace(writer_score$writer,'Fox', 'Michael P. Fox & Wil Fox')
writer_score$writer <- str_replace(writer_score$writer,'Joanna Lewis', 'Joanna Lewis & Kristine Songco')
writer_score$writer <- str_replace(writer_score$writer,'Kevin Burke', 'Kevin Burke & Chris "Doc" Wyatt')

writer_score$writer <- paste0(writer_score$writer, ' (', writer_score$N, ' episodes)')

writer_score$writer <- fct_reorder(writer_score$writer,writer_score$song_second)

writer_score %>% ggplot(aes(x=writer,y=song_second))+geom_col()+coord_flip()+
  labs(x='', y='# Seconds of songs per episode', 
       title='Seconds of songs per episode for each writer')

new_writer_score <- writer_score %>% 
  mutate(score=song_second, type="song_length") %>% 
  select(1,2,4,5)

question6_data <- read_csv("question6_data.csv")

Q6 <- rbind(question6_data,new_writer_score)

order <- (question6_data %>% arrange(score))$writer

Q6$writer <- factor(Q6$writer,levels = order)

Q6 %>% ggplot(aes(writer, score, fill=type))+geom_col(position='dodge')+
  coord_flip()+
  labs(x='',y='# length of songs (second) per episode and numbers of sentiment words per episode',
       title='Comparison between song length and sentiment words count')
