library(dplyr)
library(ggplot2)
library(rvest)
library(lubridate)
library(janitor)
library(ggthemes)
library(plotly)
library(tidytext)
library(tm)
library(qdap)
library(forcats)
library(wordcloud)
library(textdata)
library(gridExtra)


# Get relevant IMBD data for each Jane the Virgin season
jtv_episodes <- c()
for(k in 1:5){
  url <- paste0("https://www.imdb.com/title/tt3566726/episodes?season=", k)
  jtv_html <- read_html(url)
  episode_number <- jtv_html %>%
    html_nodes('.image') %>%
    html_text()
  episode_airdate <- jtv_html %>%
    html_nodes('.airdate') %>%
    html_text()
  episode_title <- jtv_html %>%
    html_nodes('.info strong a') %>%
    html_attr("title")
  episode_rating <- jtv_html %>%
    html_nodes('.ipl-rating-star.small .ipl-rating-star__rating') %>%
    html_text()
  episode_votes <- jtv_html %>%
    html_nodes('.ipl-rating-star__total-votes') %>%
    html_text()
  episode_summary <- jtv_html %>%
    html_nodes('.item_description') %>%
    html_text()
  assign(paste("jtv_episodes", k, sep = "_"), cbind(episode_number, episode_airdate, episode_title, episode_rating, episode_votes, episode_summary)) 
}
# Collect episodes for each season in the same df
jtv_episodes <- rbind(jtv_episodes_1, jtv_episodes_2, jtv_episodes_3, jtv_episodes_4, jtv_episodes_5)
jtv_episodes <- as.data.frame(jtv_episodes)
# Clean the data frame (data type, add separate column for season and episode)
jtv_episodes <-jtv_episodes %>%
  mutate(episode_airdate = dmy(episode_airdate),
         episode_rating = as.numeric(as.character(episode_rating)),
         episode_votes = parse_number(as.character(episode_votes)),
         episode_summary = as.character(episode_summary),
         number = c(1:100)) 
jtv_episodes <- jtv_episodes %>%
  separate(col = episode_number, into = c("season", "episode"), sep = ",")

jtv_episodes <- jtv_episodes %>%
  mutate(episode_number = parse_number(episode),
         season = str_replace(season, "S", "Season "))


#Get wikipedia table with viewers
urlw <- ("https://en.wikipedia.org/wiki/List_of_Jane_the_Virgin_episodes")
s1 <- urlw %>%
  read_html() %>%
  html_node(xpath = '//*[@id="mw-content-text"]/div[1]/table[2]') %>%
  html_table(fill = TRUE)
s2 <- urlw %>%
  read_html() %>%
  html_node(xpath = '//*[@id="mw-content-text"]/div[1]/table[3]') %>%
  html_table(fill = TRUE)
s3 <- urlw %>%
  read_html() %>%
  html_node(xpath = '//*[@id="mw-content-text"]/div[1]/table[4]') %>%
  html_table(fill = TRUE)
s4 <- urlw %>%
  read_html() %>%
  html_node(xpath = '//*[@id="mw-content-text"]/div[1]/table[5]') %>%
  html_table(fill = TRUE)
s5 <- urlw %>%
  read_html() %>%
  html_node(xpath = '//*[@id="mw-content-text"]/div[1]/table[6]') %>%
  html_table(fill = TRUE)

#Join and clean dataframes
jtv_episodes_wiki <- rbind(s1, s2, s3, s4, s5)
jtv_episodes_wiki <- as.data.frame(jtv_episodes_wiki)
jtv_episodes_wiki <- jtv_episodes_wiki %>%
  clean_names()
jtv_episodes_wiki <- jtv_episodes_wiki %>%
  mutate(viewers = parse_number(u_s_viewers_millions)) %>%
  select(-u_s_viewers_millions, -no_inseason, -title, -original_air_date)

# merge Wikipedia and IMBD data in single data frame
jtv <- left_join(jtv_episodes, jtv_episodes_wiki, by = c("number" = "no_overall"))


# Text analysis to count number of mention/character
# Use tinytext function unnest_tokens to transform a list of text documents into a list of words
characters_jtv <- jtv %>%
  unnest_tokens(word, episode_summary) %>% 
  filter(word %in% c("jane", "michael", "xo", "alba", "rafael", "mateo", "petra", "rogelio")) %>%
  count(word) %>%
  arrange(desc(n))



#Static plots
# ggplot of ratings as done by Katie segreti
jtv %>%
  ggplot(aes(x = episode_number, y = episode_rating, fill = season, size = episode_votes)) +
  geom_point(shape = 22) +
  geom_text(aes(label = episode_title), size = 2.5, angle = -90, hjust = 0, nudge_y = -0.08) +
  facet_grid(~season) +
  theme_wsj() +
  ylim(3, 10) +
  labs(x = " ",
       y = " ",
       title = "Jane the Virgin episodes ranked by IMDB",
       subtitle = "Square size is proportional to number of votes") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "gray87"),
        plot.background = element_rect(fill = "gray87"),
        strip.background = element_rect(fill = "gray87"),
        axis.text.x = element_blank(),
        axis.line.x.bottom = element_blank(),
        axis.ticks.x.bottom = element_blank(),
        plot.subtitle = element_text(size = 12),
        plot.title = element_text(size = 20))

#plot for episode views done with same design
jtv %>%
  arrange(number) %>%
  filter(!is.na(viewers)) %>%
  ggplot(aes(x = episode, y = viewers, fill = season)) +
  geom_col(color = "black") +
  facet_grid(~season, scales = "free_x") +
  theme_wsj() +
  labs(x = " ",
       y = " ",
       title = "Jane the Virgin episodes viewers",
       subtitle = "Million of US viewers on airdate") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "gray87"),
        plot.background = element_rect(fill = "gray87"),
        strip.background = element_rect(fill = "gray87"),
        axis.text.x = element_blank(),
        axis.line.x.bottom = element_blank(),
        axis.ticks.x.bottom = element_blank(),
        plot.subtitle = element_text(size = 12),
        plot.title = element_text(size = 20))

# Characters popularity

characters_jtv %>%
  ggplot(aes(x = fct_reorder(as.factor(word), n, .desc = TRUE), y = n, fill =word)) +
  geom_col(col = "black") +
  theme_wsj() +
  labs(x = " ",
       y = " ",
       title = "Characters popularity",
       subtitle = "Charaters total mentions in IMBD episode summaries") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "gray87"),
        plot.background = element_rect(fill = "gray87"),
        strip.background = element_rect(fill = "gray87"),
        axis.line.x.bottom = element_blank(),
        axis.ticks.x.bottom = element_blank(),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14))
  


# Do text analysis to get main themes of the show
# Prepare data frame to make corpus
jtv_text <- jtv %>%
  select(season, episode, episode_summary)
jtv_text <- jtv_text[, c(2,3,1)]
jtv_text <- jtv_text %>%
  rename(doc_id = episode, text = episode_summary)
# transformdataframe into corpus
jtv_corpus <- VCorpus(DataframeSource(jtv_text))

# Clean the corpus
# Make a function
clean_corpus <- function(corpus) {
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, words = c(stopwords("en"), "isnt", "xos", "rafeal", "jrs", "jorge", "might",  "raphael", "darci", "adam", "luisa", "also", "american", "still", "will", "petras", "michaels", "band", "things","takes", "rogelios", "river", "dier","day", "brett", "make", "decides", "star", "try", "guest", "find", "finds", "doesnt", "finally", "rafaels", "navedo", "coll", "ivonne",  "makes", "jaime", "big", "andrea", "back", "camil", "grobglas", "yael", "get", "gets","jane", "michael", "xo", "alba", "rafael", "mateo", "petra", "rogelio", "janes", "justin", "baldoni", "meanwhile", "rodriguez", "gina"))
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
  }

make_dataframe <- function(x) {
  x <- as.data.frame(x)
  x <- cbind(term = rownames(x), data.frame(x, row.names=NULL))
  return(x)
}

# Word frequency for all series
jtv_m <- as.matrix(TermDocumentMatrix(clean_corpus(jtv_corpus)))
jtv_term_freq <- sort(rowSums(jtv_m), decreasing = TRUE)
jtv_term_freq_df <-data.frame(term = rownames(as.data.frame(jtv_term_freq)), as.data.frame(jtv_term_freq))


ggplot(jtv_term_freq_df[1:15, ], 
       aes(x = fct_reorder(as.factor(term), jtv_term_freq, .desc = TRUE), y = jtv_term_freq)) +
         geom_col(color = "black", fill = "mediumpurple1") +
  theme_wsj() +
  labs(x = " ",
       y = " ",
       title = "Jane the Virgin main themes",
       subtitle = "Top 15 words mentioned in IMBD episode summary") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.line.x.bottom = element_blank(),
        axis.ticks.x.bottom = element_blank(),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14))

#Do a communality cloud
all_1 <- paste(as.data.frame(jtv_episodes_1)$episode_summary, collapse = " ")
all_2 <- paste(as.data.frame(jtv_episodes_2)$episode_summary, collapse = " ")
all_3 <- paste(as.data.frame(jtv_episodes_3)$episode_summary, collapse = " ")
all_4 <- paste(as.data.frame(jtv_episodes_4)$episode_summary, collapse = " ")
all_5 <- paste(as.data.frame(jtv_episodes_5)$episode_summary, collapse = " ")

all <- c(all_1, all_2, all_3, all_4, all_5)
all_m <- as.matrix(TermDocumentMatrix(clean_corpus(VCorpus(VectorSource(all)))))

# Find words in common of 5 seasons 
comm_c <- commonality.cloud(all_m, max.words = 100, colors ="mediumpurple1")
# words not in common
comp_c <- comparison.cloud(all_m, max.words = 100, 
                 colors = c("mediumpurple1", "grey", "gold", "orange", "pink"))

grid.arrange(comm_c, comp_c, ncol = 2)


# Do sentiment analysis with tidytext
# Sentiments in different seasons using the nrc dictionary
jtv_sent_n <- jtv %>%
  group_by(season) %>%
  unnest_tokens(word, episode_summary) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(sentiment)
  

ggplot(jtv_sent_n, aes(x = sentiment, y = n, fill = season)) +
  geom_col() +
  facet_grid(~season) +
  geom_text(aes(label = sentiment), size = 3, angle = 90, hjust = -0.1, nudge_y = -0.08) +
  theme_wsj() +
  ylim(0,90) +
  labs(x = " ",
       y = " ",
       title = "Jane the virgin sentiment analysis (NRC Dictionary)",
       subtitle = "Sentiments frequency in Jane the virgin season") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_blank(),
        axis.line.x.bottom = element_blank(),
        axis.ticks.x.bottom = element_blank(),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14))

# Sentiments in different seasons using the Bing dictionary
jtv_sent_b <- jtv %>%
  unnest_tokens(word, episode_summary) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("bing")) %>%
  count(index = season, sentiment) %>%
  group_by(index) %>%
  mutate(total = sum(n))

ggplot(jtv_sent_b, aes(x = index, y = n/total*100, fill = sentiment)) +
  geom_col(position = "stack", colour = "black") +
#  scale_fill_manual(values = c("mediumpurple1", "black")) +
  theme_wsj() +
  labs(x = " ",
       y = " ",
       title = "Jane the virgin sentiment analysis (Bing dictionary)",
       subtitle = "positive/negative proportion by season") +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        strip.background = element_rect(fill = "white"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.position = "bottom",
        axis.text.x = element_text(size = 12, vjust = 10),
        axis.line.x.bottom = element_blank(),
        axis.ticks.x.bottom = element_blank(),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12))


jtv_sent_b <- jtv %>%
  unnest_tokens(word, episode_summary) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("bing")) %>%
  count(index = season, sentiment) %>%
  group_by(index) %>%
  mutate(total = sum(n))


jtv_sent_b_overall <- jtv %>%
  unnest_tokens(word, episode_summary) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("bing")) %>%
  count(index = season, sentiment) %>%
  spread(sentiment, n) %>%
  mutate(total = positive-negative)

ggplot(jtv_sent_b_overall, aes(x = index, y = total, fill = index)) +
  geom_col(color="black") +
  theme_wsj() +
  labs(x = " ",
       y = " ",
       title = "Jane the virgin sentiment analysis (Bing dictionary)",
       subtitle = "Total sentiment score per season") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(size = 12, vjust = 10),
        axis.line.x.bottom = element_blank(),
        axis.ticks.x.bottom = element_blank(),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12))

# Sentiments in different seasons using the Afinn dictionary
jtv_sent_a <- jtv %>%
  unnest_tokens(word, episode_summary) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(season) %>%
  mutate(sentiment = sum(value)) %>%
  select(season, sentiment) %>%
  distinct()


ggplot(jtv_sent_a, aes(x = season, y = sentiment, fill = season)) +
  geom_col(color="black") +
  theme_wsj() +
  labs(x = " ",
       y = " ",
       title = "Jane the virgin sentiment analysis (Afinn dictionary)",
       subtitle = "Total sentiment score per season") +
  theme(legend.position = "none",
    panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(size = 12, vjust = 10),
        axis.line.x.bottom = element_blank(),
        axis.ticks.x.bottom = element_blank(),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12))
  


# Do a sentiment timeline
# Bing dictionary
jtv_sent_tb <- jtv %>%
  unnest_tokens(word, episode_summary) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("bing")) %>%
  count(index = number, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive-negative) %>%
  select(index, sentiment)

ggplot(jtv_sent_tb, aes(x = index, y = sentiment)) +
 geom_line(color = "mediumpurple1", size = 0.6) +
  geom_point(colour = "mediumpurple1", alpha = 0.6, size = 3) +
  theme_wsj() +
  labs(x = " ",
       y = " ",
       title = "Jane the virgin sentiment timeline (Bing dictionary)",
       subtitle = "Total sentiment score per episode") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_blank(),
        axis.line.x.bottom = element_blank(),
        axis.ticks.x.bottom = element_blank(),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14))
  
# Afinn dictionary 
jtv_sent_ta <- jtv %>%
  select(number, episode_summary) %>%
  unnest_tokens(word, episode_summary) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(number) %>%
  mutate(sentiment = sum(value)) %>%
  select(number, sentiment) %>%
  distinct()

ggplot(jtv_sent_ta, aes(x = number, y = sentiment)) +
  geom_line(color = "mediumpurple1", size = 0.6) +
  geom_point(colour = "mediumpurple1", alpha = 0.6, size = 3) +
  theme_wsj() +
  labs(x = " ",
       y = " ",
       title = "Jane the virgin sentiment timeline (Afinn dictionary)",
       subtitle = "Total sentiment score per episode") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_blank(),
        axis.line.x.bottom = element_blank(),
        axis.ticks.x.bottom = element_blank(),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12))




