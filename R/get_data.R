library(gutenbergr)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggthemes)
library(tibble)
library(jsonlite)
library(tidyverse)
library(tidytext)

# reads in the holy books from the gutenbergr project library.
bible <- gutenberg_download(30) %>%
  mutate(gutenberg_id = "bible")

quran <- gutenberg_download(7440) %>%
  mutate(gutenberg_id = "quran")

df <- bible %>%
  rbind(quran)

# adds extra books.
pride_and_prejudice <- gutenberg_download(1342) %>%
  mutate(gutenberg_id = "pride_and_prejudice")

communist_manifesto <- gutenberg_download(61) %>%
  mutate(gutenberg_id = "communist_manifesto")

sherlock_holmes <- gutenberg_download(1661) %>%
  mutate(gutenberg_id = "sherlock_holmes")

alice_in_wonderland <- gutenberg_download(11) %>%
  mutate(gutenberg_id = "alice_in_wonderland")

the_great_gatsby <- gutenberg_download(64317) %>%
  mutate(gutenberg_id = "the_great_gatsby")

all_books <- df %>%
  rbind(pride_and_prejudice) %>%
  rbind(communist_manifesto) %>%
  rbind(sherlock_holmes) %>%
  rbind(alice_in_wonderland) %>%
  rbind(the_great_gatsby)

names(all_books) <- c("book","text")

# drops all the empty strings. 
all_books <- all_books[all_books$text !="",]

# cleans all digits and punctuation. 
all_books$text <- gsub('[[:punct:]]|[[:digit:]]','',all_books$text)

# adds a biblical/not biblical and line number column. 
final_df <- all_books %>%
  mutate(book_type = ifelse(book %in% c("bible", "quran"), "biblical", "non_biblical"),
         line_no = row_number())

# saves the cleaned books data as a csv file.
write_csv(final_df, "final_df.csv")
