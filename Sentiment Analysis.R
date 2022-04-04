install.packages("tidyverse")
install.packages("tidytext")
devtools::install_github("juliasilge/tidytext")
install.packages("wordcloud")
library(tidyverse)
library(tidytext)
library(dplyr)
library(readr)
library(wordcloud)
library(tm)
library(SnowballC)
library(RColorBrewer)
review_data <- read.csv("C:/Users/kwame.adu/Desktop/Sentiment Analysis/Website/website.csv", header = TRUE, stringsAsFactors = FALSE)
View(review_data)
tidy_data <- review_data %>%
  unnest_tokens(word, Comments)
View(tidy_data)

tidy_data %>%
  count(word, sort = TRUE) %>%
  anti_join(new_stops2)


View(tidy_data)

sentiments

get_sentiments("bing")

get_sentiments("nrc")

bing_negative <- get_sentiments("bing") %>%
  filter(sentiment == "negative")



bing_word_counts <- tidy_data %>%
  inner_join(get_sentiments ("bing")) %>%
  count(word,sentiment, sort = TRUE)

bing_word_counts

bing_word_counts %>%
  group_by(sentiment) %>%
  filter(sentiment == "negative") %>%
  top_n(15) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

#creating custom stop words

new_stops <- c("complains", "complained", "assisted", stopwords("en"))

new_stops

new_stops2 <- tibble(joinColumn = new_stops)

view (new_stops2)

tidy_data %>%
    count(word) %>%
  with(wordcloud(word, n,colors=rev(colorRampPalette(brewer.pal(9,"Blues"))(32)), scale=c(5, .5), max.words = 100))


