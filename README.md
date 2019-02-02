# MLP-writer-data-and-R-scripts
It's a project using R and employ statistics and text mining technique to find out that do different writers of MLP have different writing styles.
The data are all scraped from https://mlp.fandom.com/wiki and may not be 100% accurate. There are 7 R scripts and 7 datasets

1. raw_dialog.csv: the transcpits of each episode of the show in raw form (scraped from web and not cleaned).

2. clean_dialog.csv: cleaned version of raw_dialog.csv.

3. pony_song_data.csv: the information of songs in eight seasons plus "Best Gift Ever".

4. writer_score.csv: MLP writers and number of episodes they wrote, you can generate it from R script question 1.R.

5. question6_data.csv: a dataset needed to answer question 6, but you can generate it from the R script question 3.R.

6. pony_synopsis.csv: story synopsis for each episode of MLP

7. pony_data.csv: links of corrosponding MLPwiki page for each episode of MLp

The 6 R scripts called question*.R correspond to the 6 questions I mentioned in the post. pony data web scrape.R contains code I use to scrape data from web. 
