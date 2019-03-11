# MLP-writer-data-and-R-scripts
It's a project using R and employ statistics and text mining technique to find out whether different writers of MLP have different writing styles.
The data are all scraped from https://mlp.fandom.com/wiki and may not be 100% accurate. There are 2 R scripts and 4 datasets

1. clean_dialog.csv: cleaned version of MLP transcript.

2. pony_song_data.csv: the information of songs in eight seasons plus "Best Gift Ever".

3. pony_synopsis.csv: story synopsis for each episode of MLP. You can generate it from R scrpit "pony data web scrape.R".

4. pony_data.csv: links of corrosponding MLPwiki page for each episode of MLP. You can generate it from R scrpit "pony data web scrape.R".

"pony data web scrape.R" contains code I use to scrape data from web. 

"MLP_writer_style.R" contains code I use in kaggle kernel https://www.kaggle.com/liury123/do-writers-have-different-writing-styles-in-mlp
