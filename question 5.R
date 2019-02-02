options(stringsAsFactors = F)
library(tidyverse)
clean_dialog <- read_csv("clean_dialog.csv")
writer_score <- read_csv("writer_score.csv")
top_writer <- writer_score %>% filter(N>10)

pony_character <- c('Twilight','Applejack', 'Rainbow', 'Rarity','Fluttershy','Pinkie')
body <- list()
for(i in 1:7)
{
  new_writer <- top_writer$writer[i]
  pony_matrix <- 
    data.frame(pony=pony_character,
               'Twilight' = 0,
               'Applejack' = 0,
               'Rainbow' = 0,
               'Rarity' = 0,
               'Fluttershy' = 0,
               'Pinkie' = 0)

  for(j in 1:6)
  {
    new_pony <- pony_character[j]
    writer_pony <- clean_dialog %>% filter(writer==new_writer & str_detect(pony,new_pony))
    len <- length(writer_pony$title)
    for(k in 1:6)
    {
      next_pony <- pony_character[k]
      pony_matrix[j,(k+1)] <- sum(str_detect(writer_pony$dialog,next_pony))/len
    }
  }
  pony_relation <- data.frame((pony_matrix[,2:7]+t(pony_matrix[,2:7]))/2)
  for(l in 1:6)
  {
    pony_relation[l,l] <- 0
  }
  pony_relation$pony <- pony_character
  new_relation <- pony_relation %>% 
    gather(pony_character,key=pony2,value=relation) %>% 
    filter(pony!=pony2)
  new_relation$writer=new_writer
  body[[i]] <- new_relation
}

MLP_relation <- do.call(rbind,body)

MLP_relation$pony2[str_detect(MLP_relation$pony2,'Rainbow')] <- 'Rainbow Dash'
MLP_relation$pony2[str_detect(MLP_relation$pony2,'Pinkie')] <- 'pinkie Pie'
MLP_relation$pony2[str_detect(MLP_relation$pony2,'Twilight')] <- 'Twilight Sparkle'

MLP_relation$pony[str_detect(MLP_relation$pony,'Rainbow')] <- 'Rainbow Dash'
MLP_relation$pony[str_detect(MLP_relation$pony,'Pinkie')] <- 'pinkie Pie'
MLP_relation$pony[str_detect(MLP_relation$pony,'Twilight')] <- 'Twilight Sparkle'

library(igraph)
library(ggraph)
# now we plot relationship web map
# use i to track writer and recycle code
# 0 < i < 8
i=7

set.seed(739)
MLP_relation %>% filter(writer==top_writer$writer[i]) %>% 
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = relation, 
                     edge_width = relation), 
                 edge_colour = "darkred") +
  
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  
  theme_void()+
  labs(title = paste0("Mane 6 relationship web of ",
                      top_writer$writer[i]))
print(paste0("Mane 6 relationship web of ", top_writer$writer[i]))
