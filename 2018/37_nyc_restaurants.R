library(tidyverse)

nyc_rest <- read_csv('./Data/nyc_restaurants.csv')

# Identify top 10 cuisines by record count
top_cuisine <- nyc_rest %>%
  group_by(cuisine_description) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  top_n(10) %>%
  select(cuisine = cuisine_description)
# Clean up/simplify names
top_cuisine$cuisine
top_cuisine$cuisine[3] <- "Cafe"
top_cuisine$cuisine[5] <- "Latin"
cuisine_list <- top_cuisine$cuisine

top_rest <- nyc_rest %>%
  filter(cuisine_description %in% cuisine_list)

top_rest$cuisine_description <- factor(top_rest$cuisine_description,
                                       levels = c("American", "Chinese",
                                                  "Pizza", "Italian",
                                                  "Mexican", "Japanese",
                                                  "Caribbean", "Bakery"))

top_rest %>%
  filter(grade != "Not Yet Graded") %>%
  filter(grade != "NA") %>%
  ggplot(aes(cuisine_description)) +
  geom_bar(aes(fill=grade)) +
  scale_y_continuous(breaks = seq(0, 15000, 5000)) +
  scale_fill_discrete(name = "Grades") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "NYC Restaurant Inspection Grades",
       subtitle = "Food safety for popular cuisines",
       y="Grades", x="Cuisine (self-reported)",
       caption = "Source: fivethirtyeight.com") +
  facet_wrap(boro ~ .)

top_rest %>%
  filter(critical_flag != "Not Applicable") %>%
  ggplot(aes(cuisine_description)) +
  geom_bar(aes(fill=critical_flag)) +
  scale_y_continuous(breaks = seq(0, 40000, 10000)) +
  scale_fill_discrete(name = "Violation Criticality") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "NYC Restaurant Violations",
       subtitle = "Food safety for popular cuisines",
       y="Violations", x="Cuisine (self-reported)",
       caption = "Source: fivethirtyeight.com") +
  facet_wrap(boro ~ .)

