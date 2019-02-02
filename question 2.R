library(tidyverse)
options(stringsAsFactors = F)
writer_score <- read_csv("writer_score.csv")
clean_dialog <- read_csv("clean_dialog.csv")

pony_matrix <- 
  data.frame(writer = writer_score$writer,N = writer_score$N,
             'Celestia' = 0,
             'Luna' = 0,
             'Bloom' = 0,
             'Sweetie' = 0,
             'Scootaloo' = 0,
             'Spike' = 0)

# we will count how many lines these secondary characters have per lines in episodes
for(i in 1:43)
{
  new_writer = pony_matrix[i, 1]
  new_writer_episode <- clean_dialog %>% filter(str_detect(writer,new_writer))
  N <- length(new_writer_episode$title)
  for(j in 1:6)
  {
    new_pony <- names(pony_matrix)[j+2]
    new_writer_pony_episode <- new_writer_episode %>% filter(str_detect(pony,new_pony))
    pony_matrix[i,(j+2)] <- length(new_writer_pony_episode$pony)/N
  }
}

pony_data <- pony_matrix %>% mutate(CMC = Bloom+Sweetie+Scootaloo) %>% 
  select(-c(5,6,7)) %>% filter(N > 2) %>% arrange(-N)

# use a named vector called new_writer_episode to 
# track how many episodes each writer write
new_writer_episode <- pony_data$N
names(new_writer_episode) <- pony_data$writer

pony_data <- select(pony_data,-2)

new_pony_data_secondary <- pony_data %>% 
  gather(names(pony_data)[-1], key=pony, value=per_line) %>% 
  inner_join(data.frame(writer=names(new_writer_episode), N=new_writer_episode))                                                             

# please note that we use 'Fox' to represent 'Michael P. Fox & Wil Fox',
# 'Joanna Lewis' to represent 'Joanna Lewis & Kristine Songco', 
# 'Kevin Burke' to represent 'Kevin Burke & Chris "Doc" Wyatt'

new_pony_data_secondary$writer <- str_replace(new_pony_data_secondary$writer,'Fox', 'Michael P. Fox & Wil Fox')
new_pony_data_secondary$writer <- str_replace(new_pony_data_secondary$writer,'Joanna Lewis', 'Joanna Lewis & Kristine Songco')
new_pony_data_secondary$writer <- str_replace(new_pony_data_secondary$writer,'Kevin Burke', 'Kevin Burke & Chris "Doc" Wyatt')

new_pony_data_secondary$pony <- str_replace(new_pony_data_secondary$pony,'Celestia', 'Princess Celestia')
new_pony_data_secondary$pony <- str_replace(new_pony_data_secondary$pony,'Luna', 'Princess Luna')

new_pony_data_secondary$writer <- paste0(new_pony_data_secondary$writer, ' (', new_pony_data_secondary$N, ' episodes)')

my_pony <- c('Princess Celestia', 'Princess Luna', 'Spike', 'CMC')
# use i and j to track ponies
# you can use two combination 
# i=1,j=2: i=3,j=4 to replicate my result
i = 3
j = 4


A_R_most<- new_pony_data_secondary %>% filter(pony==my_pony[i]|pony==my_pony[j]) %>% 
  group_by(pony) %>% top_n(5, wt=per_line) %>% ungroup()

new_A_R <- A_R_most %>% arrange(pony,per_line) %>% mutate(order=1:10)

new_A_R %>% ggplot(aes(x=order,y=per_line, fill=pony), fill='Blue')+
  geom_col(show.legend = F)+facet_wrap(~pony, scales='free_y')+
  coord_flip()+labs(x='', y=paste0('# Propotion of lines of ', my_pony[i], ' and ', my_pony[j], ' (per line)'), 
                    title=paste0('Top 5 writers who give ', my_pony[i], ' and ', my_pony[j], ' most lines'))+
  scale_x_continuous(
    breaks = new_A_R$order,
    labels = new_A_R$writer,
    expand = c(0,0)
  )
print(paste0('Top 5 writers who give ', my_pony[i], ' and ', my_pony[j], ' most lines'))
# we omit the code for other secondary characters because they are repetitive