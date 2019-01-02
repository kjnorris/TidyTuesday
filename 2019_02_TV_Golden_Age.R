# Load necessary libraries
library(tidyverse)
library(lubridate)
library(ggrepel)

# Read the data from csv
tv_ratings <- read_csv(here::here("Data/2019_02_IMDb_Economist_tv_ratings.csv"))

# Shows we'll look at specifically
# Thanks to @dylanjm for the prototype for the code
tv_shows <- c("The X-Files",
              "Breaking Bad",
              "Game of Thrones")

# This will let us label our plot correctly
# also we need to account for the two reboots in our data
# Thanks to @dylanjm for the prototype for the code
tv_show_dat <- tv_ratings %>%
  filter(title %in% tv_shows)

# Without this we'll get a text label at each highlighted point
# We only want the text to show on the first point for each series
# Thanks to @dylanjm for the prototype for the code
labs <- tv_ratings %>%
  filter(title %in% tv_shows) %>%
  group_by(title) %>%
  filter(row_number(title) == 1)

# Basic manipulation by title:
# calculate number of seasons on air
# find overall mean rating
# find min and max ratings
# tag by decade of first episode
final_ratings <- tv_ratings %>%
  group_by(titleId) %>%
  mutate(last_season = max(seasonNumber)) %>%
  mutate(agg_rating = mean(av_rating)) %>%
  mutate(max_rating = max(av_rating)) %>%
  mutate(min_rating = min(av_rating)) %>%
  mutate(first_air = min(date)) %>%
  mutate(decade = year(floor_date(first_air, years(10))))
final_ratings$decade <- factor(final_ratings$decade)

# Graph showing trend in ratings by time
# showing overall mean ratings (min/max errorbars) for all shows by decade
# size is length of on air run, color by decade
# linear regression line showing trend of annual mean
final_ratings %>%
  ggplot(aes(x=first_air, y=agg_rating)) +
  geom_point(aes(colour=decade, size=last_season),
             show.legend=c(colour = F, size = T)) +
  geom_errorbar(aes(ymin=min_rating, ymax=max_rating, colour=decade),
                width = 0.5, alpha = 0.5, show.legend = c(colour = F)) +
  geom_smooth(method='lm', se=FALSE, size=0.5, colour="grey") +
  coord_cartesian(ylim = c(5.0, 10.0)) +
  scale_y_continuous(breaks = seq(5.0, 10.0, .5), position = "left")+
  scale_x_date(breaks = as.Date(c("1990-01-01",
                                  "1995-01-01", "2000-01-01",
                                  "2005-01-01", "2010-01-01",
                                  "2015-01-01")),
               date_labels = "%Y") +
  scale_colour_brewer(name="Decade", breaks = seq(1990, 2020, 10),
                         palette = "Set2") +
  scale_size_continuous(range = c(1,5), breaks = seq(1, 40, 8),
                        name = "Years on the Air") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.direction = "horizontal") +
  labs(title = "Burn Out or Fade Away",
       subtitle = "Are we getting better shows?\nDo they stay on the air longer?",
       y="Mean Rating\n(all seasons)", x="Date Show First Aired",
       caption = "Source: IMDb.com")

# Graph showing trend in market share by rating
# showing mean rating by season for all shows
# color by decade
# highlights popular show from each decade
final_ratings %>%
  ggplot(aes(y=av_rating, x=share, colour=decade)) +
  geom_point()+
  geom_point(data = tv_show_dat,
             aes(x = share, y = av_rating),
             colour = "black", alpha=0.6, show.legend = FALSE) +
  geom_text_repel(data = tv_show_dat,
                  aes(x=share, y=av_rating, label=year(date)),
                  inherit.aes = FALSE, color = "black",
                  nudge_y = 0.05, fontface = "bold", size = rel(3)) +
  geom_line(data = tv_show_dat,
            aes(x = share, y = av_rating, group = title),
            alpha=0.2, inherit.aes = FALSE, color = "black") +
  geom_text_repel(data = labs,
                  aes(x = share, y = av_rating, label = title),
                  inherit.aes = FALSE, nudge_y = -.15, color = "black",
                  alpha = 0.6, fontface = "bold", size = rel(4)) +
  scale_x_continuous(breaks = seq(0, 60, 15), limits=c(0,60)) +
  coord_cartesian(ylim = c(5.0, 10.0)) +
  scale_y_continuous(breaks = seq(5.0, 10.0, .5), position = "left")+
  scale_colour_brewer(name="Decade", palette = "Set2") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.direction = "horizontal") +
  labs(title = "Critical Acclaim or Mass Market",
       subtitle = "High profile shows are getting better.\nHow many people are watching?",
       x="Share", y="Ratings",
       caption = "Source: IMDb.com")

# Finding the longest running shows
final_ratings %>%
  arrange(desc(last_season)) %>%
  group_by(title) %>%
  filter(row_number(title) == 1) %>%
  ungroup() %>%
  select(title, date, last_season, agg_rating) %>%
  # top_n(50, last_season) %>%
  filter(last_season > 1) %>%
  View()


