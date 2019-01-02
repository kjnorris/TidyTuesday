library(tidyverse)
library(lubridate)

tidy_tweets <- readRDS('./Data/2019_01_tidytuesday_tweets.rds')
# rstats_tweets <- readRDS('./Data/2019_01_rstats_tweets.rds')

tidy_fav_by_follow <- tidy_tweets %>%
  filter(!is_quote) %>%
  mutate(day_week = wday(created_at, label=TRUE)) %>%
  select(day_week, favorite_count, followers_count, friends_count)

tidy_fav_by_follow %>%
  ggplot(aes(y=favorite_count, x=followers_count)) +
  geom_point(aes(colour=day_week)) +
  geom_smooth(method='lm', se=FALSE, size=0.5, colour="grey") +
  expand_limits(y = c(0, 600)) +
  xlim(0, 35000) +
  scale_y_continuous(breaks = seq(0, 600, 200)) +
  scale_color_discrete(name = "Day of the Week",
                       breaks = c("Sun", "Mon", "Tue", "Wed",
                                  "Thu", "Fri", "Sat")) +
  theme_minimal() +
  theme(legend.position = "right", legend.direction = "vertical") +
  labs(title = "Are #TidyTuesday favorites a popularity contest?",
       subtitle = "Relationship between the number of followers and number of favorites",
       y="Favorites", x="Number of Followers",
       caption = "Source: rwteet.info")

tidy_fav_by_follow %>%
  ggplot(aes(favorite_count, colour=day_week)) +
  geom_freqpoly() +
  facet_wrap(~day_week) +
  expand_limits(y = c(0, 100)) +
  xlim(0, 100) +
  scale_y_continuous(breaks = seq(0, 100, 25)) +
  theme_minimal() +
  scale_color_discrete(name = "Day of the Week",
                       breaks = c("Sun", "Mon", "Tue", "Wed",
                                  "Thu", "Fri", "Sat")) +
  theme(legend.position = "none", legend.direction = "horizontal") +
  labs(title = "Popularity of #TidyTuesday Tweets by Day",
       subtitle = "When should you post to get the most favorites?",
       y="", x="Number of Favorites",
       caption = "Source: rwteet.info")
