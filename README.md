# MLP-writer-data-and-R-scripts
It's a project using R and employ statistics and text mining technique to find out that do different writers of MLP have different writing styles.
The data are all scraped from https://mlp.fandom.com/wiki and may not be 100% accurate. There are 6 R scripts and 5 datasets

1. full_mlp.csv: transcripts of each episode of the show (exclude ”Brotherhooves Social”, “A Hearth’s Warming Tail”, “Friendship University” and “What Lies Beneath”). Also missing the lyrics of songs in MLP.

2. full_mlp_script.csv: story synopsis of each episode of the show.

3. mlp_song_data.csv: the information of songs in eight seasons.

4. writer_score.csv: MLP writers and number of episodes they wrote, you can generate it from R script question 1.R.

5. question6_data.csv: a dataset needed to answer question 6, but you can generate it from the R script question 3.R.

The 6 R scripts correspond to the five 6 questions I mentioned in the post.
