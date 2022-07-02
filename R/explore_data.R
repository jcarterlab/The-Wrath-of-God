library(gutenbergr)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggthemes)
library(tibble)
library(jsonlite)
library(tidyverse)
library(tidytext)

# my personal plot theme for data visualizations. 
my_theme <- theme_economist_white(gray_bg = FALSE) +
  theme(plot.title = element_text(hjust = 0.5,
                                  vjust = 10,
                                  size = 10,
                                  color = "#474747"),
        plot.margin = unit(c(1.5, 1, 1.5, 1), "cm"),
        axis.text = element_text(size = 9,
                                 color = "gray30"),
        axis.text.x=element_text(vjust = -2.5),
        axis.title.x = element_text(size = 9,
                                    color = "gray30",
                                    vjust = -10),
        axis.title.y = element_text(size = 9,
                                    color = "gray30",
                                    vjust = 10),
        legend.direction = "vertical", 
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 11,
                                   color = "gray20"),
        legend.margin=margin(1, -15, 1, 0),
        legend.spacing.x = unit(0.25, "cm"),
        legend.key.size = unit(1, "cm"), 
        legend.key.height = unit(0.75, "cm"),
        strip.text = element_text(hjust = 0.5,
                                  vjust = 1,
                                  size = 10,
                                  color = "#474747"),
        panel.spacing = unit(2, "lines"))

# opens the cleaned books data frame.
df <- read_csv("final_df.csv") %>%
  mutate(book = str_replace_all(book, c("bible" = "the bible", 
                                        "quran" = "the quran")))

# breaks the text down into words and filters 
# out words of less than 2 characters. 
words <- df %>%
  unnest_tokens(word, text) %>%
  filter(nchar(word)>2)

# filters out stopwords.
words <- words %>%
  filter(!word %in% stop_words$word)

# joins the words with their sentiment values. 
sentiment <- inner_join(words, get_sentiments("nrc"))


# visualizes the top ten words for each book. 
nums <- words %>%
  filter(book_type == "biblical") %>%
  group_by(book) %>%
  count(word, sort=TRUE) %>%
  group_by(book) %>%
  mutate(rank = rank(n))

nums %>%
  group_by(book) %>%
  mutate(n = (n/sum(n))*100,
         word = str_to_title(word),
         book = str_to_title(str_replace_all(book, "_", " "))) %>%
  filter(rank %in% (max(rank)-9):max(rank)) %>%
  ggplot(aes(x=word,y=n,fill=book)) +
  ggtitle("Popular Words") +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#00B6EB", "#FB61D7")) +
  facet_wrap(~book,
             scales = "free_x") +
  scale_x_discrete(guide = guide_axis(n.dodge=2)) +
  xlab("") +
  ylab("Words (%)") +
  my_theme

# visualizes how many words are in each book.
total_words <- words %>%
  group_by(book) %>%
  summarize(Total = length(word)/10^3)

total_words_df <- words[,c("book","book_type")] %>% 
  left_join(total_words, by = "book") %>%
  distinct() %>%
  mutate(book_type = str_to_title(str_replace_all(book_type, "_", " ")),
          book = str_to_title(str_replace_all(book, "_", " ")))

total_words_df %>%
  ggplot(aes(x=book,y=Total,fill=book)) +
  geom_bar(stat="identity", position="dodge") +
  ggtitle("Word Count") +
  facet_wrap(~book_type,
             scales="free_x") +
  xlab("") +
  ylab("Words (000s)") +
  my_theme +
  theme(legend.position = "none") +
  scale_x_discrete(guide = guide_axis(n.dodge=2))


# visualizes how many distinct words are in each book.
distinct_words <- words %>%
  group_by(book) %>%
  summarize(Distinct = length(unique(word))/10^3)

distinct_words_df <- words[,c("book","book_type")] %>% 
  left_join(distinct_words, by = "book") %>%
  distinct() %>%
  mutate(book_type = str_to_title(str_replace_all(book_type, "_", " ")),
         book = str_to_title(str_replace_all(book, "_", " ")))

distinct_words_df %>%
  ggplot(aes(x=book,y=Distinct,fill=book)) +
  geom_bar(stat="identity", position="dodge") +
  ggtitle("Distinct Words") +
  facet_wrap(~book_type,
             scales="free_x") +
  xlab("") +
  ylab("Words (000s)") +
  my_theme +
  theme(legend.position = "none") +
  scale_x_discrete(guide = guide_axis(n.dodge=2))


# visualizes how emotional the books are. 
get_hyperbole <- function(book) {
  emotions <- length(sentiment[sentiment$book==book,]$sentiment)
  total <- length(words[words$book==book,]$word)
  ratio <- (emotions/total)*100
  return(ratio)
}

books <- unique(sentiment$book)

hyperbole <- tibble(book = books,
                    hyperbole = unlist(lapply(books, get_hyperbole)))

hyperbole_df <- words[,c("book","book_type")] %>% 
  left_join(hyperbole, by = "book") %>%
  distinct() %>%
  mutate(book_type = str_to_title(str_replace_all(book_type, "_", " ")),
         book = str_to_title(str_replace_all(book, "_", " ")))

hyperbole_df %>%
  ggplot(aes(x=book,y=hyperbole,fill=book)) +
  ggtitle("Hyperbole") +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~book_type,
             scales = "free_x") +
  xlab("") +
  ylab("Words (%)") +
  my_theme +
  theme(legend.position = "none") +
  scale_x_discrete(guide = guide_axis(n.dodge=2))


# visualizes net sentiment. 
net_senitment <- sentiment %>%
  group_by(book) %>%
  mutate(total = length(sentiment)) %>%
  summarize(Net=((sum(sentiment=="positive")/total)-((sum(sentiment=="negative"))/total))*100) %>%
  distinct()

net_senitment_df <- sentiment[,c("book", "book_type")] %>% 
  left_join(net_senitment,  by = "book") %>%
  distinct() %>%
  mutate(book_type = str_to_title(str_replace_all(book_type, "_", " ")),
         book = str_to_title(str_replace_all(book, "_", " ")))

net_senitment_df %>%
  ggplot(aes(x=book,y=Net,fill=book)) +
  ggtitle("Net Sentiment") +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~book_type,
             scales = "free_x") +
  xlab("") +
  ylab("Sentiment (%)") +
  my_theme +
  theme(legend.position = "none") +
  scale_x_discrete(guide = guide_axis(n.dodge=2))

# visualizes anger. 
anger <- sentiment %>%
  group_by(book) %>%
  summarize(Anger = (sum(sentiment=="anger")/length(sentiment)*100)) 

anger_df <- sentiment[,c("book", "book_type")] %>% 
  left_join(anger,  by = "book") %>%
  distinct() %>%
  mutate(book_type = str_to_title(str_replace_all(book_type, "_", " ")),
         book = str_to_title(str_replace_all(book, "_", " ")))

anger_df %>%
  ggplot(aes(x=book,y=Anger,fill=book)) +
  ggtitle("Anger") +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~book_type,
             scales = "free_x") +
  xlab("") +
  ylab("Words (%)") +
  my_theme +
  theme(legend.position = "none") +
  scale_x_discrete(guide = guide_axis(n.dodge=2))


# visualizes anger for specific topics.
get_lines <- function(term) {
  lines <- words %>%
    summarize(words[words$word == term,"line_no"]) %>%
    mutate(label = term)
  return(lines)
}

get_multiple_terms <- function(terms) {
  lines <- list()
  for(i in 1:length(terms))
    lines[[i]] <- get_lines(terms[i])
  df <- tibble(rbind_pages(lines))
  return(df)
}

get_anger <- function(data, terms) {
  plot <- data %>% 
    filter(book_type == "biblical") %>%
    left_join(terms, by = "line_no") %>%
    filter(line_no %in% terms$line_no) %>%
    mutate(label = str_to_title(str_replace_all(label, "_", " ")),
           book = str_to_title(book)) %>%
    group_by(book, label) %>%
    mutate(total = length(sentiment)) %>%
    summarize(Anger = ((sum(sentiment=="anger")/total)*100)) %>%
    unique() %>%
    ggplot(aes(x=book,y=Anger,fill=book)) +
    ggtitle(str_to_title(str_replace_all(deparse(substitute(terms)),"_"," "))) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~label) +
    xlab("") +
    ylab("Sentiment (%)") +
    my_theme +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
  return(plot)
}

# jews. 
the_jews <- get_multiple_terms(c("jew","israel","hebrew",
                                 "jerusalem","benjamin"))
get_anger(sentiment, the_jews)


israelite_figures <- get_multiple_terms(c("abraham","moses","david",
                                          "jacob","joseph","solomon",
                                          "isaac","joshua","samuel"))
get_anger(sentiment, israelite_figures)



# sexual sins. 
sex <- get_multiple_terms(c("adultery","sodom","gomorrah",
                            "harlot","whore","fornication"))
get_anger(sentiment, sex)



sex <- get_multiple_terms(c("love"))
get_anger(sentiment, sex)




