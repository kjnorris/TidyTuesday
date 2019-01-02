library(tidyverse)
library(lubridate)

movie_profit <- read_csv('./Data/movie_profit.csv') %>%
  select(-X1)

movie_ratings <- c("G", "PG", "PG-13", "R", "NC-17")

movie_profit$mpaa_rating <- ifelse(movie_profit$mpaa_rating %in% movie_ratings,
       movie_profit$mpaa_rating, "Not Rated")

# movie_profit$release_date <- mdy(movie_profit$release_date)

profit_by_genre_year <- movie_profit %>%
  filter(movie != "Frozen") %>%
  mutate(domestic_profit = domestic_gross - production_budget) %>%
  mutate(worldwide_profit = worldwide_gross - production_budget) %>%
  mutate(release_year = year(mdy(release_date))) %>%
  select(release_year, genre, domestic_profit, worldwide_profit) %>%
  filter(release_year >= 1970, release_year <= 2017) %>%
  group_by(release_year, genre) %>%
  summarise(Domestic = mean(domestic_profit)/1000000,
            Worldwide = mean(worldwide_profit)/1000000) %>%
  gather(Domestic, Worldwide, key="region", value="profit")

# The plot thickens
# Graph mean profit by genre and year of release
# Facet by region - domestic and worldwide
profit_by_genre_year %>%
  ggplot(aes(x=release_year, colour=genre)) +
  geom_line(aes(y=profit)) +
  facet_grid(region ~ .) +
  expand_limits(y = c(0, 500), x = c(1970, 2017)) +
  scale_y_continuous(breaks = seq(0, 500, 100)) +
  scale_x_continuous(breaks=seq(1970, 2017, 5)) +
  guides(colour=guide_legend(title="Genre")) +
  theme_minimal() +
  theme(legend.position = "top") +
  labs(title = "Film Profit by Genre - Domestic and Global",
       subtitle = "Annual average profitability by movie genre",
       y="Average profit (millions of $)",
       x="",
       caption = "Source: the-numbers.com")


worldwide_profit_decade <- movie_profit %>%
  mutate(worldwide_profit = (worldwide_gross - production_budget)/1000000) %>%
  mutate(release_year = year(mdy(release_date))) %>%
  select(movie, release_year, genre, worldwide_profit) %>%
  # filter(release_year >= 1970, release_year <= 2017) %>%
  filter(release_year >= 1970, release_year <= 2018) %>%
  filter(movie != "Frozen") %>%
  group_by(genre)

# Total global profit by decade
worldwide_profit_decade %>%
  ggplot(aes(x=release_year, y=worldwide_profit, fill = genre)) +
  stat_summary_bin(binwidth=10,
                   fun.y="sum", geom="bar",
                   position=position_dodge2(padding=0.2)) +
  expand_limits(y = c(0, 40000), x = c(1970, 2018)) +
  scale_y_continuous(breaks = seq(0, 40000, 10000)) +
  scale_x_continuous(breaks=seq(1970, 2018, 10)) +
  scale_color_discrete(name = "Genre",
                       breaks = c("Action", "Adventure", "Comedy",
                                  "Drama", "Horror")) +
  guides(fill=guide_legend(title="Genres")) +
  theme_minimal() +
  theme(legend.position = "top") +
  labs(title = "Worldwide Film Market by Decade",
       subtitle = "Total profit by genre in each decade since 1970",
       y="Total profit (millions of $)",
       x="",
       caption = "Source: the-numbers.com")



# Data check - find the most profitable movies worldwide by genre
# Update genre filter as needed: Action, Adventure, Comedy, Drama, Horror
movie_profit %>%
  mutate(worldwide_profit = worldwide_gross - production_budget) %>%
  mutate(release_year = year(mdy(release_date))) %>%
  filter(release_year >= 1970, release_year <= 2017) %>%
  filter(genre == "Adventure") %>%
  select(movie, release_year, worldwide_profit) %>%
  arrange(desc(worldwide_profit))

# Data check - count movies made by genre
movie_profit %>%
  mutate(worldwide_profit = worldwide_gross - production_budget) %>%
  mutate(release_year = year(mdy(release_date))) %>%
  filter(release_year >= 1970, release_year <= 2017) %>%
  select(genre) %>%
  table()


# Movie rating by month of release

abbrevMonth = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

movie_profit %>%
  filter(mpaa_rating %in% movie_ratings) %>%
  mutate(release_year = year(mdy(release_date))) %>%
  filter(release_year >= 1970, release_year <= 2017) %>%
  distinct(movie, .keep_all = TRUE) %>%
  mutate(release_month = factor(month(mdy(release_date)),
                                   levels = seq(1:12),
                                   labels = abbrevMonth)) %>%
  mutate(mpaa = ordered(mpaa_rating,
                       levels = c("G", "PG", "PG-13", "R", "NC-17"))) %>%
  select(release_month, mpaa, movie) %>%
  ggplot(aes(x=release_month, fill=mpaa, order=mpaa)) +
  geom_bar(position=position_dodge2(padding=0.2)) +
  scale_fill_manual(values=c("#009E73", "#56B4E9",
                             "#F0E442", "#D55E00", "#999999")) +
  expand_limits(y = c(0, 210)) +
  scale_y_continuous(breaks = seq(0, 210, 25)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(title="MPAA Rating")) +
  labs(title = "Film Ratings by Month",
       subtitle = "Totasl number of MPAA rated films released each month since 1970",
       y="Films released",
       x="",
       caption = "Source: the-numbers.com")
