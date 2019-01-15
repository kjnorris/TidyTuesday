# Load necessary libraries
library(tidyverse)
library(lubridate)

# Load the relevant files
launches <- read_csv(here::here("Data/2019_03_launches.csv"))
agencies <- read_csv(here::here("Data/2019_03_agencies.csv"))

# Correct mis-keyed date
launches[launches["tag"] == "2018-F01", "launch_date"] <- "2018-10-11"

# Process data for plotting
# add column for decade of launch as ordered factor (1950, 1960, etc)
# add column for day of week of launch as ordered factor (Sun, Mon, etc)
# change category to factor (success/failure)
# change agency_type to factor (state, private, startup)
decade_launches <- launches %>%
  filter(!is.na(launch_date)) %>%
  mutate(decade = year(floor_date(ymd(launch_date), years(10)))) %>%
  mutate(day_of_week = wday(launch_date, label=TRUE, abbr=TRUE)) %>%
  select(tag, agency_type, category, day_of_week, decade) %>%
  mutate(decade = fct_relevel(as_factor(as.character(decade)),
                              c("1950", "1960", "1970", "1980",
                                "1990","2000", "2010"))) %>%
  mutate(category = fct_recode(as_factor(category),
                               success = "O",
                               failure = "F")) %>%
  mutate(agency_type = as_factor(agency_type))

# Plot!
# bar chart to count launches
# colour by success/failure (category variable)
# facet by agency_type and decade
decade_launches %>%
  ggplot(aes(x = day_of_week)) +
  geom_bar(aes(fill = category)) +
  facet_grid(rows = vars(agency_type), cols = vars(decade)) +
  scale_fill_brewer(name="Success", palette = "Set1", direction = -1) +
  theme_minimal() +
  theme(legend.position = "right", legend.direction = "vertical",
        axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(title = "Evolution of the Space Race",
       subtitle = "Who launches rockets into space, and when?",
       x="Day of the Week", y="Number of Launches",
       caption = "Source: The Economist")



