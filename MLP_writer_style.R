library(tidyverse)
library(plotly)
library(htmlwidgets)
library(IRdisplay)
options(stringsAsFactors = F)
clean_dialog <- read_csv('clean_dialog.csv')
mlp_writer <- count(clean_dialog,writer,sort=T)$writer
# The mlp_writer vector is a little messy, we clean them a bit.
# First we only choose those who write episodes solely 
writer_vector <- subset(mlp_writer,!str_detect(mlp_writer,',|&|;'))
# Now we check who are left
# mlp_writer %>% str_remove_all(paste(writer_vector,collapse = '|')) %>% View()
# Notice how Michael P. Fox and Wil Fox always appear together, 
# we use Fox to represent these two.
# We use 'Joanna Lewis' to represent 'Joanna Lewis and Kristine Songco', 
# because they write togrther,too. 
# We use 'Kevin Burke' to represent 'Kevin Burke and Chris "Doc" Wyatt', 
# with the same reason as above
writer_vector[32:43] <- c('F.M. De Marco', 'Teddy Antonio', 
                          'Jayson Thiessen', 'Betsy McGowen', 'Jim Miller',
                          'Sammie Crowley & Whitney Wetta',
                          'Barry Safchik & Michael Platt',
                          'Kevin Burke',
                          'Joanna Lewis',
                          'Fox',
                          'Rita Hsiao',
                          'Joe Ballarini')
# We use writer_score to track how many episodes each writer participate
writer_score <- data.frame(writer=writer_vector, N=0)

writer_episode <- clean_dialog %>% select(1,2) %>% unique()

for(i in 1:43){
  writer_score[i,2] <- 
    sum(str_detect(writer_episode$writer,writer_score[i,1]))
}
rm(i,writer_vector, mlp_writer, writer_episode)
# We creat a data.frame called pony_matrix to track how many lines 
# each one of mane6 have for each writer
pony_matrix <- 
  data.frame(writer = writer_score$writer,
             N = writer_score$N,
             'Twilight' = 0,
             'Applejack' = 0,
             'Rainbow' = 0,
             'Rarity' = 0,
             'Fluttershy' = 0,
             'Pinkie' = 0)
for(i in 1:43){
  new_writer = pony_matrix[i, 1]
  new_writer_episode <- clean_dialog %>% 
    filter(str_detect(writer,new_writer))
  for(j in 1:6)
  {
    new_pony <- names(pony_matrix)[j+2]
    new_writer_pony_episode <- new_writer_episode %>% 
      filter(str_detect(pony,new_pony))
    pony_matrix[i,(j+2)] <- length(new_writer_pony_episode$pony)
  }
}
# we change the value in pony_matrix to proportion of lines in mane 6
for(i in 1:43){
  pony_matrix[i,3:8] <- pony_matrix[i,3:8]/sum(pony_matrix[i,3:8])
}
rm(i, j, new_pony, new_writer, new_writer_episode, new_writer_pony_episode)
# We will only consider those who wrote more than two episode.
new_pony_data <- pony_matrix %>% 
  filter(N > 2) %>% 
  gather(key=pony, value=prop, -c('writer', 'N')) %>%
  select(1:4) %>% 
  mutate(writer = fct_recode(writer, 'Michael P. Fox & Wil Fox' = 'Fox',
                             'Joanna Lewis & Kristine Songco' = 'Joanna Lewis',
                             'Kevin Burke & Chris "Doc" Wyatt' = 'Kevin Burke'),
         pony = fct_recode(pony, 'Twilight Sparkle' = 'Twilight',
                           'Pinkie Pie' = 'Pinkie',
                           'Rainbow Dash' = 'Rainbow'))
# For top 10 writers, we plot the propotion chart.
# We use top to track the top 10 writers
top <- new_pony_data %>% 
  filter(N > 6) %>% 
  mutate(writer = paste0(writer, ' (', N, ' episodes)')) %>% 
  mutate(writer = fct_reorder(writer, N),
         pony = factor(pony, levels = c('Twilight Sparkle', 'Pinkie Pie', 
                                        'Rainbow Dash', 'Rarity', 'Fluttershy', 
                                        'Applejack')))
# We need to make a palette for our plot_ly to work
pal = c('Twilight Sparkle' = "#6633CC", 
        'Pinkie Pie' = "#FF66CC", 
        'Rainbow Dash' = "#00CCFF", 
        'Rarity' = "grey95", 
        'Fluttershy' = "#FFFF00", 
        'Applejack' = "#FF9933")
p1 <- top %>%
  plot_ly(x = ~ prop, y = ~ writer, color = ~ pony, type = 'bar', colors = pal) %>% 
  layout(barmode = 'stack', 
         title = 'Propotion of Lines of Mane 6 for Top 10 Writers',
         yaxis = list(title = ''),
         xaxis = list(title = '# Propotion of Lines of Mane6'))
saveWidget(p1, 'demo1.html', selfcontained = F)
display_html('<iframe src="demo1.html", width = 900, height = 400></iframe>')
give_most_lines <- function(pony_1){
  new_pony_data$writer <- paste0(new_pony_data$writer, ' (', new_pony_data$N, ' episodes)')
  
  T_F_least <- new_pony_data %>% filter(pony==pony_1) %>% 
    top_n(10, wt=-prop) %>% mutate(color='Last 5')
  
  T_F_most<- new_pony_data %>% filter(pony==pony_1) %>% 
    top_n(10, wt = prop) %>% mutate(color='Top 5')
  
  T_F <- rbind(T_F_least,T_F_most)
  
  p2 <- T_F %>% 
    mutate(writer = fct_reorder(writer, prop)) %>% 
    plot_ly(y = ~ writer, x = ~ prop, type = 'bar', color = ~ color, colors = c('#660033', '#6699FF')) %>% 
    layout(title = paste0('The Top_Bottom 10 Writers Who Give ', pony_1, ' Most_Least Lines'),
           yaxis = list(title = ''),
           xaxis = list(title = '# Propotion of lines'))
  filename <- paste0(pony_1,'demo2.html')
  saveWidget(p2, filename, selfcontained = F)
  my_display <- paste0('<iframe src="', filename, '", width = 900, height = 500></iframe>')
  display_html(my_display)
}
give_most_lines('Twilight Sparkle')
give_most_lines('Fluttershy')
give_most_lines('Pinkie Pie')
give_most_lines('Rarity')
give_most_lines('Applejack')
give_most_lines('Rainbow Dash')
rm(pal, top, new_pony_data, pony_matrix, give_most_lines)

pony_matrix <- 
  data.frame(writer = writer_score$writer,
             N = writer_score$N,
             'Celestia' = 0,
             'Luna' = 0,
             'Bloom' = 0,
             'Sweetie' = 0,
             'Scootaloo' = 0,
             'Spike' = 0)

# we will count how many lines these secondary characters have per lines in episodes
for(i in 1:43){
  new_writer = pony_matrix[i, 1]
  new_writer_episode <- clean_dialog %>% 
    filter(str_detect(writer,new_writer))
  N <- length(new_writer_episode$title)
  for(j in 1:6)
  {
    new_pony <- names(pony_matrix)[j+2]
    new_writer_pony_episode <- new_writer_episode %>%
      filter(str_detect(pony,new_pony))
    pony_matrix[i,(j+2)] <- length(new_writer_pony_episode$pony)/N
  }
}
rm(i, j, new_pony, new_writer, new_writer_episode, new_writer_pony_episode, N)
pony_data <- pony_matrix %>% 
  mutate(CMC = Bloom+Sweetie+Scootaloo) %>% 
  select(-c(5,6,7)) %>% 
  filter(N > 2)

new_pony_data_secondary <- pony_data %>%
  gather(key=pony, value=per_line,-c(1:2)) %>% 
  mutate(writer =  fct_recode(writer, 'Michael P. Fox & Wil Fox' = 'Fox',
                              'Joanna Lewis & Kristine Songco' = 'Joanna Lewis',
                              'Kevin Burke & Chris "Doc" Wyatt' = 'Kevin Burke'),
         pony = fct_recode(pony, 'Princess Celestia' = 'Celestia',
                           'Princess Luna' = 'Luna')) %>% 
  mutate(writer = paste0(writer, ' (', N, ' episodes)'))
# we have 'Princess Celestia', 'Princess Luna', 'Spike', 'CMC'
give_most_lines <- function(pony_1){
  A_R_most <- new_pony_data_secondary %>% filter(pony==pony_1) %>% 
    top_n(10, wt=per_line) %>% 
    mutate(writer = fct_reorder(writer, per_line))
  
  p3 <- A_R_most %>% plot_ly(x = ~ per_line, y = ~ writer, type = 'bar') %>% 
    layout(title = paste0('Top 10 Writers Who Give ', pony_1, ' Most Lines'),
           xaxis = list(title = paste0('# Propotion of lines of ', pony_1, ' (per line)')),
           yaxis = list(title = ''))
  filename <- paste0(pony_1,'demo3.html')
  saveWidget(p3, filename, selfcontained = F)
  my_display <- paste0('<iframe src="', filename, '", width = 900, height = 500></iframe>')
  display_html(my_display)  
}
give_most_lines('Princess Celestia')
give_most_lines('Princess Luna')
give_most_lines('CMC')
give_most_lines('Spike')
rm(give_most_lines, pony_data, pony_matrix, new_pony_data_secondary)

pony_synopsis <- read_csv("pony_synopsis.csv")
writer_score_backup <- writer_score
writer_score <- writer_score %>% 
  mutate(Positive = 0,
         Negative = 0)
# We only consider writers who wrote more than 2 episodes
writer_score <- writer_score %>% filter(N > 2)
library(tidytext)
# Creat my own sentiment words
my_sentiment <- rbind(get_sentiments('b'), 
                      data.frame(word=c('no','not',"don't","haven't","won't", 
                                        "never", "can't", "couldn't", "without"),
                                 sentiment='negative'))
# Please note that there are dulpicates in my_sentiment, remove duplicates.
# Then we count how many positive/negative words appear in each summary on average.
my_sentiment <- my_sentiment[!duplicated(my_sentiment$word),]
for(i in 1:26){
  writer_name <- writer_score$writer[i]
  writer_content <- pony_synopsis %>% 
    filter(str_detect(writer,writer_name))
  
  Total_number <- length(writer_content$writer)
  tidy_content <- writer_content %>% 
    unnest_tokens(word,content) %>% 
    inner_join(my_sentiment, by = 'word')
  
  score <- tidy_content %>% count(sentiment)
  writer_score[i,3] <- score$n[2]/Total_number
  writer_score[i,4] <- score$n[1]/Total_number
}
rm(i, Total_number, writer_name, writer_content, tidy_content, score)

new_writer_score <- writer_score %>% 
  mutate(writer =  fct_recode(writer, 'Michael P. Fox & Wil Fox' = 'Fox',
                              'Joanna Lewis & Kristine Songco' = 'Joanna Lewis',
                              'Kevin Burke & Chris "Doc" Wyatt' = 'Kevin Burke')) %>% 
  mutate(writer = paste0(writer, ' (', N, ' episodes)')) %>% 
  mutate(writer = fct_reorder(writer, Positive + Negative))
# We are gonna need a data set for comparison purpose later, we call it question6_data.csv
question6_data <- new_writer_score %>% 
  mutate(score = Positive + Negative) %>% 
  select(1,2,5) %>% 
  mutate(type = 'sentiment_word')
p4 <- new_writer_score %>% 
  plot_ly(x = ~ Positive, y = ~ writer, type = 'bar', name = 'Positive') %>% 
  add_trace(x = ~ Negative, name = 'Negative') %>% 
  layout(barmode = 'stack', 
         title = 'Sentiment Words Usage Frequency Between Writers',
         yaxis = list(title = ''),
         xaxis = list(title = '# Number of Positive/Negative Words per Story Summary'))
saveWidget(p4, 'demo4.html', selfcontained = F)
display_html('<iframe src="demo4.html", width = 1000, height = 700></iframe>')
writer_score <- writer_score_backup
rm(new_writer_score)

# We only focus on those who wrote more than 2 episodes.
writer_score <- writer_score %>% 
  filter(N > 2) %>% 
  mutate('Twilight Sparkle' = 0,
         'Pinkie Pie' = 0,
         'Rarity' = 0,
         'Rainbow Dash' = 0,
         'Applejack' = 0,
         'Fluttershy' = 0)
# We count the average number of positive words minus negative words per line on average.
for(i in 1:26){
  for(j in 1:6)
  {
    Pony <- names(writer_score)[j+2]
    Writer <- writer_score$writer[i]
    
    pony_writer <- clean_dialog %>% 
      filter(str_detect(pony,Pony), str_detect(writer,Writer))
    
    len <- length(pony_writer$title)
    
    tidy_data <- pony_writer %>% 
      unnest_tokens(word,dialog) %>% 
      inner_join(my_sentiment, by = 'word')
    
    sent <- tidy_data %>% count(sentiment)
    
    writer_score[i,(j+2)] <- (sent$n[2]-sent$n[1])/len
  }
}
rm(i, j, len, Pony, Writer, sent, pony_writer, tidy_data)
tidy_sent <- writer_score %>% 
  mutate(writer =  fct_recode(writer, 'Michael P. Fox & Wil Fox' = 'Fox',
                              'Joanna Lewis & Kristine Songco' = 'Joanna Lewis',
                              'Kevin Burke & Chris "Doc" Wyatt' = 'Kevin Burke')) %>% 
  mutate(writer = paste0(writer, ' (', N, ' episodes)')) %>% 
  gather(names(writer_score)[-c(1,2)], key=pony, value=prop)

# We use prop to represent sentiment score
# we have 'Twilight Sparkle', 'Fluttershy', 
# 'Pinkie Pie', 'Rarity', 'Applejack', 'Rainbow Dash'
sentiment_for_pony <- function(pony_1){
  A_R_least<- tidy_sent %>% 
    filter(pony==pony_1) %>% 
    top_n(10, wt=-prop) %>% 
    mutate(color='Top 10 most negative')
  
  A_R_most<- tidy_sent %>% 
    filter(pony==pony_1) %>% 
    top_n(10, wt=prop) %>%
    mutate(color='Top 10 most positive')
  
  p5 <- rbind(A_R_least,A_R_most) %>% 
    mutate(writer = fct_reorder(writer, prop)) %>% 
    plot_ly(x = ~ prop, y = ~ writer, 
            type = 'bar', color = ~ color, 
            colors = c('blue','red')) %>% 
    layout(title = paste0('The Top 10 Writers Who Give ',  pony_1, ' Most Positive And Negative Personality'),
           yaxis = list(title = ''),
           xaxis = list(title = '# Average Number of Positive Words Minus Negative Words per Line'))
  filename <- paste0(pony_1,'demo5.html')
  saveWidget(p5, filename, selfcontained = F)
  my_display <- paste0('<iframe src="', filename, '", width = 900, height = 500></iframe>')
  display_html(my_display)
}
sentiment_for_pony('Twilight Sparkle')
sentiment_for_pony('Fluttershy')
sentiment_for_pony('Pinkie Pie')
sentiment_for_pony('Rarity')
sentiment_for_pony('Applejack')
sentiment_for_pony('Rainbow Dash')
rm(sentiment_for_pony, tidy_sent, my_sentiment)
writer_score <- writer_score_backup


top_writer <- writer_score %>% 
  filter(N>10) %>% 
  mutate(order = 1:n())

pony_character <- c('Twilight',
                    'Applejack', 
                    'Rainbow',
                    'Rarity',
                    'Fluttershy',
                    'Pinkie')
# We use body to keep mane6 relationship matrix
body <- list()
for(i in 1:7){
  new_writer <- top_writer$writer[i]
  pony_matrix <- 
    data.frame(pony = pony_character,
               'Twilight' = NA,
               'Applejack' = NA,
               'Rainbow' = NA,
               'Rarity' = NA,
               'Fluttershy' = NA,
               'Pinkie' = NA)
  for(j in 1:5){
    for(k in (j+1):6){
      j_to <- clean_dialog %>% 
        filter(str_detect(writer, new_writer),
               str_detect(pony, pony_character[j]))
      to_k <- j_to %>% 
        filter(str_detect(dialog, pony_character[k]))
      strength_1 <- nrow(to_k)/nrow(j_to)
      
      k_to <- clean_dialog %>% 
        filter(str_detect(writer, new_writer),
               str_detect(pony, pony_character[k]))
      to_j <- k_to %>% 
        filter(str_detect(dialog, pony_character[j]))
      strength_2 <- nrow(to_j)/nrow(k_to)
      pony_matrix[j,(k+1)] <- mean(strength_1,strength_2)
    }
  }
  body[[i]] <- pony_matrix
}
library(networkD3)
network_plot <- function(my_writer){
  new_location <- top_writer %>% 
    filter(writer == my_writer) %>% 
    pull(order)
  
  pony_data <- body[[new_location]] %>% 
    gather(pony_2, relation, -pony, na.rm = T) %>% 
    rename(source = pony, target = pony_2) %>% 
    mutate(source = fct_recode(source, '0' = 'Twilight',
                               '1' = 'Applejack',
                               '2' = 'Rainbow',
                               '3' = 'Rarity',
                               '4' = 'Fluttershy') %>% 
             as.character() %>% 
             as.integer(),
           target = fct_recode(target, '1' = 'Applejack',
                               '2' = 'Rainbow',
                               '3' = 'Rarity',
                               '4' = 'Fluttershy',
                               '5' = 'Pinkie') %>% 
             as.character() %>% 
             as.integer()) %>% 
    mutate(relation = (100*relation)^2)
  
  pony_node <- data.frame(name = pony_character, 
                          group = 1:length(pony_character)) %>% 
    mutate(name = fct_recode(name, 'Twilight Sparkle' = 'Twilight',
                             'Rainbow Dash' = 'Rainbow',
                             'Pinkie Pie' = 'Pinkie'))
  
  ColourScale <- 'd3.scaleOrdinal()
  .domain(["Twilight Sparkle", "Applejack", "Rainbow Dash", "Rarity", "Fluttershy", "Pinkie Pie"])
  .range(["#6633CC","#FF9933","#00CCFF","#CCCCCC","#FFFF00","#FF66CC"])'
  p6 <- forceNetwork(Links = pony_data, Nodes = pony_node,
                     Source = "source",  Target = "target", 
                     Value = "relation", NodeID = "name",
                     Group = 'group', opacity = 1,
                     fontSize = 10, zoom = TRUE,
                     colourScale  = ColourScale)
  filename <- paste0(my_writer,'demo6.html')
  saveWidget(p6, filename, selfcontained = F)
  my_display <- paste0('<iframe src="', filename, '", width = 900, height = 500></iframe>')
  display_html(my_display)
}
network_plot('Amy Keating Rogers')
network_plot('Cindy Morrow')
network_plot('M. A. Larson')
network_plot('Meghan McCarthy')
network_plot('Josh Haber')
network_plot('Dave Polsky')
network_plot('Michael Vogel')
rm(network_plot, pony_character, writer_score_backup, body, top_writer, clean_dialog)

pony_song_data <- read_csv("pony_song_data.csv")
pony_song_data$Length[c(5,8,9,28,44,46,47,49,56,62,64,80,99)] <- 
  c('0:33','4:13','0:42','1:53','2:59','3:16','2:27','2:16','2:17','3:15','3:26','3:03','3:44')

time_data <- data.frame(do.call(rbind,str_split(pony_song_data$Length,pattern = ':')))

new_song_data <- pony_song_data %>% 
  mutate(minute = as.numeric(time_data$X1),
         second = as.numeric(time_data$X2)) %>% 
  mutate(total_second = 60*minute + second) %>% 
  group_by(title) %>% 
  summarise(total_second=sum(total_second))

rm(pony_song_data,time_data)
episode_song <- pony_synopsis %>% 
  select(1,2) %>% 
  left_join(new_song_data, by = 'title') %>% 
  mutate(total_second = ifelse(is.na(total_second),
                               yes=0,no=total_second))
rm(new_song_data)
writer_score$song_second <- 0
# we only focus on writers who wrote more than 2 episods
writer_score <- filter(writer_score,N > 2)
# we count seconds of songs per episode
for(i in 1:26){
  new_writer <- writer_score$writer[i]
  
  writer_data <- episode_song %>% 
    filter(str_detect(writer,new_writer))
  
  writer_score$song_second[i] <- mean(writer_data$total_second)
}
rm(i, new_writer, writer_data)
writer_score <- writer_score %>% 
  mutate(writer =  fct_recode(writer, 'Michael P. Fox & Wil Fox' = 'Fox',
                              'Joanna Lewis & Kristine Songco' = 'Joanna Lewis',
                              'Kevin Burke & Chris "Doc" Wyatt' = 'Kevin Burke')) %>% 
  mutate(writer = paste0(writer, ' (', N, ' episodes)')) %>% 
  mutate(writer = fct_reorder(writer, song_second))
p7 <- writer_score %>% plot_ly(x = ~ song_second, y = ~ writer, type = 'bar', name = 'song_second') %>% 
  layout(title = 'Seconds of Songs per Episode for Each Writer',
         xaxis = list(title = '# Seconds of Songs per Episode'),
         yaxis = list(title = ''))
saveWidget(p7, 'demo7.html', selfcontained = F)
display_html('<iframe src="demo7.html", width = 900, height = 700></iframe>')
new_writer_score <- writer_score %>% 
  mutate(sentiment_word = question6_data$score) %>% 
  mutate(writer = fct_reorder(writer, sentiment_word))
p8 <- new_writer_score %>% plot_ly(x = ~ song_second, y = ~ writer, type = 'bar', name = 'song_second') %>% 
  add_trace(x = ~ sentiment_word, name = 'sentiment_word') %>% 
  layout(title = 'Comparison Between Song Length and Sentiment Words Count',
         xaxis = list(title = '# Length of Songs (Second) per Episode and Numbers of Sentiment Words per Story Summary'),
         yaxis = list(title = ''))
saveWidget(p8, 'demo8.html', selfcontained = F)
display_html('<iframe src="demo8.html", width = 1000, height = 700></iframe>')
