library(tidyverse)
options(stringsAsFactors = F)
clean_dialog <- read_csv("clean_dialog.csv")
mlp_writer <- count(clean_dialog,writer,sort=T)$writer

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
writer_vector[32:43] <- c('F.M. De Marco', 'Teddy Antonio', 
                          'Jayson Thiessen', 'Betsy McGowen', 'Jim Miller',
                          'Sammie Crowley & Whitney Wetta',
                          'Barry Safchik & Michael Platt',
                          'Kevin Burke',
                          'Joanna Lewis',
                          'Fox',
                          'Rita Hsiao',
                          'Joe Ballarini')
# we use writer_score$N to track how many episodes each writer participate
writer_score <- data.frame(writer=writer_vector, N=0)

writer_episode <- clean_dialog %>% select(1,2) %>% unique()

for(i in 1:43)
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

for(i in 1:43)
{
  new_writer = pony_matrix[i, 1]
  new_writer_episode <- clean_dialog %>% filter(str_detect(writer,new_writer))
  for(j in 1:6)
  {
    new_pony <- names(pony_matrix)[j+2]
    new_writer_pony_episode <- new_writer_episode %>% filter(str_detect(pony,new_pony))
    pony_matrix[i,(j+2)] <- length(new_writer_pony_episode$pony)
  }
}
# we change the value in pony_matrix to proportion of lines in mane 6
for(i in 1:43)
{
  pony_matrix[i,3:8] <- pony_matrix[i,3:8]/sum(pony_matrix[i,3:8])
}

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
my_pony <- c('Twilight Sparkle', 'Fluttershy', 'Pinkie Pie', 'Rarity', 'Applejack', 'Rainbow Dash')

new_pony_data$writer <- paste0(new_pony_data$writer, ' (', new_pony_data$N, ' episodes)')

# use i and j to track ponies
# you can use three combination 
# i=1,j=2: i=3,j=4; i=5,j=6 to replicate my result
i = 5
j = 6

T_F_least <- new_pony_data %>% filter(pony==my_pony[i]|pony==my_pony[j]) %>% 
  group_by(pony) %>% top_n(5, wt=-prop) %>% ungroup() %>% mutate(color='Last 5')

T_F_most<- new_pony_data %>% filter(pony==my_pony[i]|pony==my_pony[j]) %>% 
  group_by(pony) %>% top_n(5, wt=prop) %>% ungroup() %>% mutate(color='Top 5')

T_F <- rbind(T_F_least,T_F_most)

T_F$color <- factor(T_F$color,levels=c('Top 5','Last 5'))
# Then we need to fix the facet problems

new_T_F <- T_F %>% arrange(pony,prop) %>% mutate(order=1:20)

new_T_F %>% ggplot(aes(x=order,y=prop,fill=color))+geom_col()+facet_wrap(~pony, scales='free_y')+
  coord_flip()+labs(x='', y= paste0('# Propotion of lines of ', my_pony[i] ,' and ', my_pony[j]), 
                    title= paste0('The Top_Bottom 5 writers who give ', my_pony[i] ,' and ', my_pony[j], ' most_least lines'))+
  scale_x_continuous(
    breaks = new_T_F$order,
    labels = new_T_F$writer,
    expand = c(0,0)
  )
print(paste0('The Top_Bottom 5 writers who give ', my_pony[i] ,' and ', my_pony[j], ' most_least lines'))
# I omit the the code for the rest of main 6 simply because they are repetitive 