# Scrape the singer of each song of 69 love songs

library(tidyverse)
library(rvest)

url <- "https://en.wikipedia.org/wiki/69_Love_Songs"

singers <- read_html(url) %>% 
  html_nodes(".tracklist td") %>% 
  html_text()
singers

vol1 <- seq(3, 92, 4)
vol2 <- seq(97, 186, 4)
vol3 <- seq(191, 280, 4)

singer_positions <- c(vol1, vol2, vol3)

singers_69 <- singers[singer_positions]
singers_69

singers_69_df <- tibble(singer = singers_69, song_number = 1:69) %>% 
  mutate(singer = case_when(singer == "Merritt" ~ "Stephin Merritt",
                            singer == "Beghtol" ~ "LD Beghtol",
                            singer == "Gonson" ~ "Claudia Gonson",
                            singer == "Simms" ~ "Shirley Simms",
                            singer == "Klute" ~ "Dudley Klute",
                            TRUE ~ singer))

count(singers_69_df, singer)

write_rds(singers_69_df, "69LoveSongs_Singers.rds")
