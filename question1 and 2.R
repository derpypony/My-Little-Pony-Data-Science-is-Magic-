library(tidyverse)
options(stringsAsFactors = F)
full_mlp_script <- read_csv("full_mlp_script.csv")
mlp_writer <- count(full_mlp_script,writer,sort=T)$writer
# the mlp_writer vector is a little messy, we clean them
# first we only choose those write episodes solely 
writer_vector <- subset(mlp_writer,!str_detect(mlp_writer,',|&|;'))
# now we check who are left
mlp_writer %>% 
  str_remove_all(paste(writer_vector,collapse = '|')) %>% 
  View()
# notice how Michael P. Fox and Wil Fox always appear together, we use Fox to represent these two
# 'Joanna Lewis' to represent 'Joanna Lewis and Kristine Songco', because they write togrther,too. 
# 'Kevin Burke' to represent 'Kevin Burke and Chris "Doc" Wyatt', the same reason as above
writer_vector[32:41] <- c('F.M. De Marco', 'Teddy Antonio', 
                          'Jayson Thiessen', 'Betsy McGowen', 'Jim Miller',
                          'Joanna Lewis',
                          'Fox',
                          'Kevin Burke',
                          'Sammie Crowley & Whitney Wetta',
                          'Barry Safchik & Michael Platt')
# we use writer_score$N to track how many episodes each writer participate
writer_score <- data.frame(writer=writer_vector, N=0)

writer_episode <- full_mlp_script %>% select(1,2,3) %>% unique()

for(i in 1:41)
{
  writer_score[i,2] <- sum(str_detect(writer_episode$writer,writer_score[i,1]))
}
# we save the data writer_score
write.csv(writer_score,'writer_score.csv',row.names = F)

# we creat a data.frame called pony_matrix to track how many lines 
# each one of mane6 have for each writer
pony_matrix <- 
  data.frame(writer = writer_score$writer,N = writer_score$N,
             'Twilight' = 0,
             'Applejack' = 0,
             'Rainbow' = 0,
             'Rarity' = 0,
             'Fluttershy' = 0,
             'Pinkie' = 0)

for(i in 1:41)
{
  new_writer = pony_matrix[i, 1]
  new_writer_episode <- full_mlp_script %>% filter(str_detect(writer,new_writer))
  for(j in 1:6)
  {
    new_pony <- names(pony_matrix)[j+2]
    new_writer_pony_episode <- new_writer_episode %>% filter(str_detect(pony,new_pony))
    pony_matrix[i,(j+2)] <- length(new_writer_pony_episode$pony)
  }
}
# we change the value in pony_matrix to proportion of lines in mane 6
for(i in 1:41)
{
  pony_matrix[i,3:8] <- pony_matrix[i,3:8]/sum(pony_matrix[i,3:8])
}

write.csv(pony_matrix,'pony_matrix.csv',row.names = F)
# we will only consider those who wrote more than two episode, that is
# three episodes and above
pony_data <- pony_matrix %>% filter(N > 2) %>% arrange(-N)
# we creat a named vector called new_writer_episode to track how many episodes each writer wrote
new_writer_episode <- pony_data$N
names(new_writer_episode) <- pony_data$writer
# remove the N column in pony_data
pony_data <- select(pony_data,-2)

new_pony_data <- pony_data %>% 
  gather(names(pony_data)[-1], key=pony, value=prop) %>% 
  inner_join(data.frame(writer=names(new_writer_episode), N=new_writer_episode))                                                             

new_pony_data$writer <- str_replace(new_pony_data$writer,'Fox', 'Michael P. Fox & Wil Fox')
new_pony_data$writer <- str_replace(new_pony_data$writer,'Joanna Lewis', 'Joanna Lewis & Kristine Songco')
new_pony_data$writer <- str_replace(new_pony_data$writer,'Kevin Burke', 'Kevin Burke & Chris "Doc" Wyatt')

new_pony_data$pony <- str_replace(new_pony_data$pony,'Twilight', 'Twilight Sparkle')
new_pony_data$pony <- str_replace(new_pony_data$pony,'Pinkie', 'Pinkie Pie')
new_pony_data$pony <- str_replace(new_pony_data$pony,'Rainbow', 'Rainbow Dash')

write.csv(new_pony_data,'new_pony_data.csv',row.names = F)

# For top 9 writers, we plot the propotion chart
# we use top data.frame to track the top 8 writers
top <- new_pony_data %>% filter(N > 8)
top$writer <- paste0(top$writer, ' (', top$N, ' episodes)')
top$writer <- fct_reorder(top$writer,top$N)
top$pony <- factor(top$pony, levels = c('Twilight Sparkle', 'Pinkie Pie', 'Rainbow Dash', 'Rarity', 'Fluttershy', 'Applejack'))

top %>% ggplot(aes(x=writer,y=prop, fill=pony))+geom_col()+
  coord_flip()+labs(x='', y='# propotion of lines', title='Propotion of lines of mane 6 for top 9 writers')+
  scale_fill_manual(values= c("#6633CC", "#FF66CC","#00CCFF" ,"#FFFFFF", "#FFFF00", "#FF9933"))

# Next we examine for each pony, who write most lines and who write lest lines
new_pony_data$writer <- paste0(new_pony_data$writer, ' (', new_pony_data$N, ' episodes)')

T_F_least <- new_pony_data %>% filter(pony=='Twilight Sparkle'|pony=='Fluttershy') %>% 
  group_by(pony) %>% top_n(5, wt=-prop) %>% ungroup() %>% mutate(color='Last 5')

T_F_most<- new_pony_data %>% filter(pony=='Twilight Sparkle'|pony=='Fluttershy') %>% 
  group_by(pony) %>% top_n(5, wt=prop) %>% ungroup() %>% mutate(color='Top 5')

T_F <- rbind(T_F_least,T_F_most)

T_F$color <- factor(T_F$color,levels=c('Top 5','Last 5'))
# Then we need to fix the facet problems

new_T_F <- T_F %>% arrange(pony,prop) %>% mutate(order=1:20)

new_T_F %>% ggplot(aes(x=order,y=prop,fill=color))+geom_col()+facet_wrap(~pony, scales='free_y')+
  coord_flip()+labs(x='', y='# Propotion of lines of Fluttershy and Twilight Sparkle', 
                    title='The top/bottom 5 writers who give Fluttershy and Twilight Sparkle most/least lines')+
  scale_x_continuous(
    breaks = new_T_F$order,
    labels = new_T_F$writer,
    expand = c(0,0)
  )
# I omit the the code for the rest of main 6 simply because they are repetitive 

pony_matrix <- 
  data.frame(writer = writer_score$writer,N = writer_score$N,
             'Celestia' = 0,
             'Luna' = 0,
             'Bloom' = 0,
             'Sweetie' = 0,
             'Scootaloo' = 0,
             'Spike' = 0)

# we will count how many lines these secondary characters have per lines in episodes
for(i in 1:41)
{
  new_writer = pony_matrix[i, 1]
  new_writer_episode <- full_mlp_script %>% filter(str_detect(writer,new_writer))
  N <- length(new_writer_episode$episode)
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

write.csv(new_pony_data_secondary,'new_pony_data_secondary.csv', row.names = F)

new_pony_data_secondary$writer <- paste0(new_pony_data_secondary$writer, ' (', new_pony_data$N, ' episodes)')

# first we recycle the code we use before on Celestia and Luna
A_R_most<- new_pony_data_secondary %>% filter(pony=='Princess Celestia'|pony=='Princess Luna') %>% 
  group_by(pony) %>% top_n(5, wt=per_line) %>% ungroup()

new_A_R <- A_R_most %>% arrange(pony,per_line) %>% mutate(order=1:10)

new_A_R %>% ggplot(aes(x=order,y=per_line, fill=pony), fill='Blue')+
  geom_col(show.legend = F)+facet_wrap(~pony, scales='free_y')+
  coord_flip()+labs(x='', y='# Propotion of lines of Princess Celestia and Princess Luna (per line)', 
                    title='The top 5 writers who give Princess Celestia and Princess Luna most lines')+
  scale_x_continuous(
    breaks = new_A_R$order,
    labels = new_A_R$writer,
    expand = c(0,0)
  )
# we omit the code for other secondary characters because they are repetitive