library(tidyverse)

options(stringsAsFactors = F)

mlp_writer <- count(full_mlp_script,writer,sort=T)$writer


writer_vector <- mlp_writer[c(1:7,9:16,19,21,23:26,28,31,33:36,41,46,56,63)]


mlp_writer %>% 
  str_remove_all(paste(writer_vector,collapse = '|')) %>% 
  View()


writer_vector[32:41] <- c('F.M. De Marco', 'Teddy Antonio', 
                          'Jayson Thiessen', 'Betsy McGowen', 'Jim Miller',
                          'Joanna Lewis',
                          'Fox',
                          'Kevin Burke',
                          'Sammie Crowley & Whitney Wetta',
                          'Barry Safchik & Michael Platt')


# please note that we will use 'Fox' to represent 'Michael P. Fox & Wil Fox',
# 'Joanna Lewis' to represent 'Joanna Lewis & Kristine Songco', 
# 'Kevin Burke' to represent 'Kevin Burke & Chris "Doc" Wyatt'

writer_score <- data.frame(writer=writer_vector, N=0)

writer_episode <- count(full_mlp_script, episode, writer)

for(i in 1:41)
{
  writer_score[i,2] <- sum(str_detect(writer_episode$writer,writer_score[i,1]))
}
write.csv(writer_score,'writer_score.csv',row.names = F)

# clean all the data and import them from scrtch



pony_matrix <- 
  data.frame(writer = writer_score$writer,times = writer_score$N,
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
rm(i,j,new_writer,new_pony,new_writer_episode,new_writer_pony_episode)

for(i in 1:41)
{
  pony_matrix[i,3:8] <- pony_matrix[i,3:8]/sum(pony_matrix[i,3:8])
}
rm(i)
write.csv(pony_matrix,'pony_matrix.csv',row.names = F)
# we will only consider those who wrote more than two episode, that is
# three episodes and above
 

pony_data <- pony_matrix %>% filter(times>2) %>% arrange(-times)

new_writer_episode <- pony_data$times
names(new_writer_episode) <- pony_data$writer

pony_data <- select(pony_data,-2)


new_pony_data <- pony_data %>% 
  gather(names(pony_data)[-1], key=pony, value=prop) %>% 
  inner_join(data.frame(writer=names(new_writer_episode), times=unname(new_writer_episode)))                                                             


write.csv(new_pony_data,'new_pony_data.csv',row.names = F)
# Clean all the data and import new_pony_data.csv
# For top 9 writers, we plot the propotion chart
# please note that we use 'Fox' to represent 'Michael P. Fox & Wil Fox',
# 'Joanna Lewis' to represent 'Joanna Lewis & Kristine Songco', 
# 'Kevin Burke' to represent 'Kevin Burke & Chris "Doc" Wyatt'

new_pony_data$writer <- str_replace(new_pony_data$writer,'Fox', 'Michael P. Fox & Wil Fox')
new_pony_data$writer <- str_replace(new_pony_data$writer,'Joanna Lewis', 'Joanna Lewis & Kristine Songco')
new_pony_data$writer <- str_replace(new_pony_data$writer,'Kevin Burke', 'Kevin Burke & Chris "Doc" Wyatt')

new_pony_data$pony <- str_replace(new_pony_data$pony,'Twilight', 'Twilight Sparkle')
new_pony_data$pony <- str_replace(new_pony_data$pony,'Pinkie', 'Pinkie Pie')
new_pony_data$pony <- str_replace(new_pony_data$pony,'Rainbow', 'Rainbow Dash')

write.csv(new_pony_data,'new_pony_data.csv', row.names = F)

top <- new_pony_data %>% filter(times>8)
top$writer <- paste0(top$writer, ' (', top$times, ' episodes)')



top$writer <- fct_reorder(top$writer,top$times)
top$pony <- factor(top$pony, levels = c('Twilight Sparkle', 'Pinkie Pie', 'Rainbow Dash', 'Rarity', 'Fluttershy', 'Applejack'))

top %>% ggplot(aes(x=writer,y=prop, fill=pony))+geom_col()+
  coord_flip()+labs(x='', y='# propotion of lines', title='Propotion of lines of mane 6 for top 9 writers')+
  scale_fill_manual(values= c("#6633CC", "#FF66CC","#00CCFF" ,"#FFFFFF", "#FFFF00", "#FF9933"))


# Next we examine for each pony, who write most lines and who write lest lines
new_pony_data$writer <- paste0(new_pony_data$writer, ' (', new_pony_data$times, ' episodes)')


T_F_lest <- new_pony_data %>% filter(pony=='Twilight Sparkle'|pony=='Fluttershy') %>% 
  group_by(pony) %>% top_n(5, wt=-prop) %>% ungroup() %>% mutate(color='Last 5')

T_F_most<- new_pony_data %>% filter(pony=='Twilight Sparkle'|pony=='Fluttershy') %>% 
  group_by(pony) %>% top_n(5, wt=prop) %>% ungroup() %>% mutate(color='Top 5')

T_F <- rbind(T_F_lest,T_F_most)

T_F[9,3] <- 0.003
T_F$color <- factor(T_F$color, levels = c('Top 5', 'Last 5'))

T_F %>% ggplot(aes(x=writer,y=prop,fill=color))+geom_col()+facet_wrap(~pony, scales='free_y')+
  coord_flip()+labs(x='', y='# Propotion of lines of Fluttershy and Twilight Sparkle', 
                    title='The top/last 5 writers who give Fluttershy and Twilight Sparkle most/lest lines')
# The we need to fix the facet problems
new_T_F <- T_F %>% arrange(pony,prop) %>% mutate(order=1:20)

new_T_F %>% ggplot(aes(x=order,y=prop,fill=color))+geom_col()+facet_wrap(~pony, scales='free_y')+
  coord_flip()+labs(x='', y='# Propotion of lines of Fluttershy and Twilight Sparkle', 
                    title='The top/bottom 5 writers who give Fluttershy and Twilight Sparkle most/least lines')+
  scale_x_continuous(
    breaks = new_T_F$order,
    labels = new_T_F$writer,
    expand = c(0,0)
  )
rm(T_F,new_T_F,T_F_lest,T_F_most)


# we can use the same code above to plot chart for pinkie pie and rarity


R_P_lest <- new_pony_data %>% filter(pony=='Rarity'|pony=='Pinkie Pie') %>% 
  group_by(pony) %>% top_n(5, wt=-prop) %>% ungroup() %>% mutate(color='Last 5')

R_P_most<- new_pony_data %>% filter(pony=='Rarity'|pony=='Pinkie Pie') %>% 
  group_by(pony) %>% top_n(5, wt=prop) %>% ungroup() %>% mutate(color='Top 5')

R_P <- rbind(R_P_lest,R_P_most)


R_P$color <- factor(R_P$color, levels = c('Top 5', 'Last 5'))

new_R_P <- R_P %>% arrange(pony,prop) %>% mutate(order=1:20)

new_R_P %>% ggplot(aes(x=order,y=prop,fill=color))+geom_col()+facet_wrap(~pony, scales='free_y')+
  coord_flip()+labs(x='', y='# Propotion of lines of Pinkie Pie and Rarity Pie', 
                    title='The top/bottom 5 writers who give Pinkie Pie and Rarity most/least lines')+
  scale_x_continuous(
    breaks = new_R_P$order,
    labels = new_R_P$writer,
    expand = c(0,0)
  )


# we can use the same code above to plot chart for Applejack and Rainbow dash


A_R_lest <- new_pony_data %>% filter(pony=='Applejack'|pony=='Rainbow Dash') %>% 
  group_by(pony) %>% top_n(5, wt=-prop) %>% ungroup() %>% mutate(color='Last 5')

A_R_most<- new_pony_data %>% filter(pony=='Applejack'|pony=='Rainbow Dash') %>% 
  group_by(pony) %>% top_n(5, wt=prop) %>% ungroup() %>% mutate(color='Top 5')

A_R <- rbind(A_R_lest,A_R_most)


A_R$color <- factor(A_R$color, levels = c('Top 5', 'Last 5'))

new_A_R <- A_R %>% arrange(pony,prop) %>% mutate(order=1:20)

new_A_R[11:12,3] <- 0.003

new_A_R %>% ggplot(aes(x=order,y=prop,fill=color))+geom_col()+facet_wrap(~pony, scales='free_y')+
  coord_flip()+labs(x='', y='# Propotion of lines of Applejack and Rainbow Dash', 
                    title='The top/bottom 5 writers who give Applejack and Rainbow Dash most/least lines')+
  scale_x_continuous(
    breaks = new_A_R$order,
    labels = new_A_R$writer,
    expand = c(0,0)
  )


# remove all the data and start over
library(tidyverse)
options(stringsAsFactors = F)
pony_matrix <- 
  data.frame(writer = writer_score$writer,times = writer_score$N,
             'Celestia' = 0,
             'Luna' = 0,
             'Bloom' = 0,
             'Sweetie' = 0,
             'Scootaloo' = 0,
             'Spike' = 0)


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
rm(N,i,j,new_writer,new_pony,new_writer_episode,new_writer_pony_episode)


# Now we can procee to keep cleaning the data


pony_data <- pony_matrix %>% mutate(CMC = Bloom+Sweetie+Scootaloo) %>% select(-c(5,6,7)) %>% filter(times>2) %>% arrange(-times)

new_writer_episode <- pony_data$times
names(new_writer_episode) <- pony_data$writer

pony_data <- select(pony_data,-2)


new_pony_data <- pony_data %>% 
  gather(names(pony_data)[-1], key=pony, value=prop) %>% 
  inner_join(data.frame(writer=names(new_writer_episode), times=unname(new_writer_episode)))                                                             

# please note that we use 'Fox' to represent 'Michael P. Fox & Wil Fox',
# 'Joanna Lewis' to represent 'Joanna Lewis & Kristine Songco', 
# 'Kevin Burke' to represent 'Kevin Burke & Chris "Doc" Wyatt'

new_pony_data$writer <- str_replace(new_pony_data$writer,'Fox', 'Michael P. Fox & Wil Fox')
new_pony_data$writer <- str_replace(new_pony_data$writer,'Joanna Lewis', 'Joanna Lewis & Kristine Songco')
new_pony_data$writer <- str_replace(new_pony_data$writer,'Kevin Burke', 'Kevin Burke & Chris "Doc" Wyatt')

new_pony_data$pony <- str_replace(new_pony_data$pony,'Celestia', 'Princess Celestia')
new_pony_data$pony <- str_replace(new_pony_data$pony,'Luna', 'Princess Luna')

names(new_pony_data)[3] <- 'per_thousand_lines'

write.csv(new_pony_data,'new_pony_data_secondary.csv', row.names = F)
# remove all data and import new_pony_data_secondary.csv
new_pony_data <- new_pony_data_secondary
rm(new_pony_data_secondary)
new_pony_data$writer <- paste0(new_pony_data$writer, ' (', new_pony_data$times, ' episodes)')
# first we recycle the code we use before on Celestia and Luna


A_R_most<- new_pony_data %>% filter(pony=='Princess Celestia'|pony=='Princess Luna') %>% 
  group_by(pony) %>% top_n(5, wt=per_thousand_lines) %>% ungroup()

A_R <- A_R_most

new_A_R <- A_R %>% arrange(pony,per_thousand_lines) %>% mutate(order=1:10)

new_A_R %>% ggplot(aes(x=order,y=per_thousand_lines, fill=pony), fill='Blue')+geom_col(show.legend = F)+facet_wrap(~pony, scales='free_y')+
  coord_flip()+labs(x='', y='# Propotion of lines of Princess Celestia and Princess Luna', 
                    title='The top 5 writers who give Princess Celestia and Princess Luna most lines')+
  scale_x_continuous(
    breaks = new_A_R$order,
    labels = new_A_R$writer,
    expand = c(0,0)
  )
# Second we recycle the code we use before on Spike and CMC


A_R_most<- new_pony_data %>% filter(pony=='Spike'|pony=='CMC') %>% 
  group_by(pony) %>% top_n(5, wt=per_thousand_lines) %>% ungroup()

A_R <- A_R_most

new_A_R <- A_R %>% arrange(pony,per_thousand_lines) %>% mutate(order=1:10)

new_A_R %>% ggplot(aes(x=order,y=per_thousand_lines, fill=pony))+geom_col(show.legend = F)+facet_wrap(~pony, scales='free_y')+
  coord_flip()+labs(x='', y='# Propotion of lines of CMC and Spike', 
                    title='The top 5 writers who give CMC and Spike most lines')+
  scale_x_continuous(
    breaks = new_A_R$order,
    labels = new_A_R$writer,
    expand = c(0,0)
  )




