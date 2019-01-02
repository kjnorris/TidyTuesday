library(janeaustenr)
library(tidyverse)
library(tidytext)
library(tm)
library(scales)

original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(line = row_number()) %>%
  ungroup()

tidy_books <- original_books %>%
  unnest_tokens(word, text)

tidy_books <- tidy_books %>%
  anti_join(get_stopwords())

tidy_books %>%
  count(word, sort = TRUE)

# Word Cloud - Detour from normal processing ...
top_words <- tidy_books %>%
  count(word, sort=TRUE)

pal <- brewer.pal(10, "Dark2")
png("Austen.png", width = 800, height = 600,
    bg = "transparent")
wordcloud(top_words$word, top_words$n, rot.per=0.30,
          random.color=TRUE, colors=pal, scale=c(6,.5),
          min.freq=100, max.words=200)
dev.off()

# Returning to Julia Silge's walk-through ...
get_sentiments("bing")

janeaustensentiment <- tidy_books %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(book, index = line %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(janeaustensentiment, aes(index, sentiment, fill = book)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")

data("AssociatedPress", package = "topicmodels")
AssociatedPress

ap_sentiments <- tidy(AssociatedPress) %>%
  inner_join(get_sentiments("bing"), by = c(term = "word")) %>%
  count(document, sentiment, wt = count) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  arrange(sentiment)

ggplot(ap_sentiments, aes(document, sentiment)) +
  geom_bar(stat = "identity", show.legend = FALSE)

comparison <- tidy(AssociatedPress) %>%
  count(word = term) %>%
  rename(AP = n) %>%
  inner_join(count(tidy_books, word)) %>%
  rename(Austen = n) %>%
  mutate(AP = AP / sum(AP),
         Austen = Austen / sum(Austen))

ggplot(comparison, aes(AP, Austen)) +
  geom_point(alpha = 0.5) +
  geom_text(aes(label = word), check_overlap = TRUE,
            vjust = 1, hjust = 1) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")


