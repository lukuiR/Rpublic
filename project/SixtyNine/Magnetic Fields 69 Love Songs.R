library(tidyverse)
library(rvest)

url <- "https://genius.com/The-magnetic-fields-absolutely-cuckoo-lyrics"

# url for tracklist and urls for each track
tracklist_url <- "https://genius.com/albums/The-magnetic-fields/69-love-songs"

# tracklist for album
tracklist <- tracklist_url %>% 
  read_html() %>% 
  html_nodes(".chart_row-content-title") %>% 
  html_text()

# tidy up tracklist
tracklist_clean <- tracklist %>% 
  str_remove("\n") %>% 
  str_remove("Lyrics") %>% 
  str_trim()

tracklist_clean

# vector of urls for each song page
track_urls <- tracklist_url %>% 
  read_html() %>% 
  html_nodes(".u-display_block") %>%
  html_attr('href')

#track_urls <- track_urls[47:69]

song_scrape <- function(url) {
  
  Sys.sleep(2)
  
  readurl <- read_html(url)

# Scrape lyrics
lyrics <- readurl %>% 
  html_node(".lyrics") %>% 
  html_text()

# tidy up lyrics
lyrics_clean <- lyrics %>% 
  # Remove empty lines and spaces at beginning of lines
  str_trim() %>% 
  # New lines are not always separated by \n, they just run into each other
  # hopefully separating out where a lower case is followed by upper case will sort this out!
  str_replace_all("([a-z])([A-Z])", "\\1\n\\2") %>% 
  str_replace_all("([0-9])([A-Z])", "\\1\n\\2") %>% 
  str_replace_all("(\\?|\\!)([A-Z])", "\\1\n\\2") %>% 
  str_replace_all("(\\')([A-Z])", "\n\\1\\2") %>% 
  str_replace_all('([a-z])(\\")([A-Z])', '\\1\\2\n\\3') %>% 
  # Keeping the instructions inside square brackets for now, place on separate line - not sure whether to use or ignore yet
  str_replace_all("(\\[.*\\])", "\n\\1\n") %>% 
  read_lines()

# scrape song title
title <- readurl %>% 
  html_node(".header_with_cover_art-primary_info-title") %>% 
  html_text()

# create df of song title and song lyrics (row for each line)
song_lyric <- tibble(song = title, lyric = lyrics_clean) %>% 
  filter(lyric != "")

}

# loop the track urls through the scraping function
album <- map_df(track_urls, song_scrape)

# number the tracks on the album
album_numbers <- album %>% 
  distinct(song) %>% 
  cbind(song_number = 1:69, 
        disc_number = rep(1:3, each = 23),
        track_number = rep(1:23, by = 3)) %>% 
  mutate(disc_track_no = paste(disc_number, track_number, sep = "."))

# append track numbers onto lyrics
album_numbered <- album %>% 
  inner_join(album_numbers, by = "song")

# save data at this point to rds file
write_rds(album_numbered, "69LoveSongs.rds")
