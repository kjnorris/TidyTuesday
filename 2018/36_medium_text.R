library(tidyverse)
library(tidytext)
library(RColorBrewer)
library(wordcloud)
library(scales)

medium_data <- read_csv('./Data/36_medium_datasci.csv')

ai_tags <- medium_data %>%
  mutate(tag_ai_comb = tag_ai + tag_artificial_intelligence) %>%
  filter(tag_ai_comb != 0) %>%
  filter(tag_machine_learning == 0) %>%
  mutate(line = row_number()) %>%
  select(line, title, subtitle, author, tag_ai_comb, tag_machine_learning)

ai_tags$title <- str_replace_na(ai_tags$title, replace=" ")
ai_tags$subtitle <- str_replace_na(ai_tags$subtitle, replace=" ")

ai_tags <- ai_tags %>%
  mutate(text = str_c(title, subtitle, sep=" ")) %>%
  select(line, author, text)

ai_words <- ai_tags %>%
  unnest_tokens(word, text) %>%
  anti_join(get_stopwords()) %>%
  count(word, sort=TRUE)

pal <- brewer.pal(8, "Dark2")
png("./Images/AI_Words.png", width = 800, height = 600,
    bg = "transparent")
wordcloud(ai_words$word, ai_words$n, rot.per=0.30,
          random.color=TRUE, colors=pal, scale=c(8,.6),
          max.words=200)
dev.off()

ml_tags <- medium_data %>%
  mutate(tag_ai_comb = tag_ai + tag_artificial_intelligence) %>%
  filter(tag_machine_learning == 1) %>%
  filter(tag_ai_comb == 0) %>%
  mutate(line = row_number()) %>%
  select(line, title, subtitle, author, tag_ai_comb, tag_machine_learning)

ml_tags$title <- str_replace_na(ml_tags$title, replace=" ")
ml_tags$subtitle <- str_replace_na(ml_tags$subtitle, replace=" ")

ml_tags <- ml_tags %>%
  mutate(text = str_c(title, subtitle, sep=" ")) %>%
  select(line, author, text)

ml_words <- ml_tags %>%
  unnest_tokens(word, text) %>%
  anti_join(get_stopwords()) %>%
  count(word, sort=TRUE)

pal <- brewer.pal(8, "Dark2")
png("./Images/ML_Words.png", width = 800, height = 600,
    bg = "transparent")
wordcloud(ml_words$word, ml_words$n, rot.per=0.30,
          random.color=TRUE, colors=pal, scale=c(8,.6),
          max.words=200)
dev.off()

comparison <- ai_words %>%
  rename(AI = n) %>%
  inner_join(ml_words) %>%
  rename(ML = n) %>%
  mutate(AI = AI / sum(AI),
         ML = ML / sum(ML))

ggplot(comparison, aes(AI, ML)) +
  geom_point(alpha = 0.5) +
  geom_text(aes(label = word), check_overlap = TRUE,
            vjust = 1, hjust = -0.1) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "AI vs. ML - What's in a label?",
       subtitle = "Do we discuss Machine Learning and Artificial Intelligence differently",
       y="% ML Documents",
       x="% AI Documents",
       caption = "Source: medium.com")

ggsave('./Images/36_AIML_WordUsage.png')


