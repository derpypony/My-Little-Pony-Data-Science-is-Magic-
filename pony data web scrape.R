options(stringsAsFactors = F)
library(Rcrawler)
library(tidyverse)
url <- 'https://mlp.fandom.com/wiki/Friendship_is_Magic_animated_media'
xpath <- '//a[text()="Transcript"]/@href'
all_links <- ContentScraper(Url = url,
                            XpathPatterns = xpath,
                            ManyPerPattern = T)
pony_all_links <- unlist(all_links)
pony_all_links <- paste0('https://mlp.fandom.com',pony_all_links)

# we use rvest to read in the table
library(rvest)
pony_table <- url %>% read_html() %>% html_nodes(xpath = '//*[@id="mw-content-text"]/table') %>% 
  html_table()

pony_data_1 <- do.call(rbind,pony_table[c(1:5,7:8)]) %>% select(2,3,4)
pony_data_2 <- data.frame(pony_table[[6]]) %>% select(2,3,4)
pony_data_3 <- rbind(pony_table[[9]], pony_table[[10]]) %>% select(1,2,3)

names(pony_data_1) <- c('title', 'writer', 'date')
names(pony_data_2) <- c('title', 'writer', 'date')
names(pony_data_3) <- c('title', 'writer', 'date')

pony_data <- rbind(pony_data_1,pony_data_2,pony_data_3) %>% arrange(date)

pony_data[198,] <- pony_data[197,]
pony_data[197,] <- pony_data[165,]
pony_data <- pony_data[-165,]

pony_data$transcript <- pony_all_links[1:197]

pony_data$synopsis <- str_remove(pony_data$transcript, '/Transcripts')

write.csv(pony_data,'pony_data.csv',row.names = F)

pony_synopsis <- pony_data %>% select(1,2,3,5)
pony_synopsis$content <- ''

# we scrape the story synopsis and save them in pony_synopsis
for(i in 1:197)
{
  url <- pony_synopsis$synopsis[i]
  xpath <- '//div[@id="mw-content-text"]/text()|//div[@id="mw-content-text"]/p/text()'
  story <- ContentScraper(Url = url,
                          XpathPatterns = xpath,
                          ManyPerPattern = T)
  story <- unlist(story) %>% str_remove_all('\n')
  pony_synopsis$content[i] <- paste(story,collapse = ' ')
}
names(pony_synopsis)[4] <- 'synopsis_links'

write.csv(pony_synopsis, 'pony_synopsis.csv', row.names = F)
# then we scrape the MLP songs
url <- 'https://mlp.fandom.com/wiki/Songs'
pony_table <- url %>% read_html() %>% html_nodes(xpath = '//*[@id="mw-content-text"]/table') %>% 
  html_table()

pony_songs <- pony_table[c(3,4,5,7,11,13,15,21,23)]

pony_song_data <- data.frame(title = unlist(lapply(pony_songs, '[[', 1)),
                             song = unlist(lapply(pony_songs, '[[', 2)),
                             lenght = unlist(lapply(pony_songs, '[[', 5)))

write.csv(pony_song_data, 'pony_song_data.csv', row.names = F)

pony_transcript <- pony_data %>% select(1:4)
names(pony_transcript)[4] <- 'transcript_links'


body <- list()
for(i in 1:197)
{
  url <- pony_transcript$transcript_links[i]
  xpath <- c('//div[@id="mw-content-text"]/dl/dd')
  episode <- ContentScraper(Url = url,
                            XpathPatterns = xpath,
                            ManyPerPattern = T)
  episode <- unlist(episode)
  position <- which(str_detect(episode,'Dear Princess Celestia,'))
  pony <- str_extract(episode[position], '[a-zA-Z ]+:')
  episode[position+1] <- paste0(pony,' ',episode[position+1])
  body[[i]] <- episode
  
}
new_body <- list()
for(i in 1:197)
{
  len <- length(body[[i]])
  vec1 <- rep(pony_transcript$title[i],len)
  vec2 <- rep(pony_transcript$writer[i],len)
  new_body[[i]] <- data.frame(title=vec1,
                        writer=vec2,
                        dialog=body[[i]])
}
raw_dialog <- do.call(rbind,new_body)
write.csv(raw_dialog,'raw_dialog.csv',row.names = F)

# we can proceed to clean the raw_dialog
# and we call it clean_dialog

clean_dialog <- raw_dialog 

clean_dialog$dialog <- str_replace_all(clean_dialog$dialog,'\n', ' ')

clean_dialog$dialog <- trimws(clean_dialog$dialog)

clean_dialog <- clean_dialog %>% filter(!str_detect(dialog,'\\]$'))

clean_dialog$pony <- ''

clean_dialog$pony <- str_extract(clean_dialog$dialog, "^((\\w| |\\.|\\')+:)")

na_position <- is.na(clean_dialog$pony)

clean_dialog$pony[na_position] <- str_extract(clean_dialog$dialog[na_position], "^\\[(\\w| )+\\]")

na_position <- is.na(clean_dialog$pony)

clean_dialog$pony[na_position] <- 'Others'

clean_dialog$pony <- str_remove_all(clean_dialog$pony, '\\[|\\]|:')

clean_dialog$dialog <- str_remove(clean_dialog$dialog, '^\\[(\\w| )+\\]')

clean_dialog$dialog <- str_remove(clean_dialog$dialog, '(\\w| )+:')

write.csv(clean_dialog,'clean_dialog.csv',row.names = F)
