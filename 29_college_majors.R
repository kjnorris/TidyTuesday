library(tidyverse)

recent_grads <- read_csv('./Data/recent-grads.csv')

# Basic graph showing overall relationship between median salary
# and gender for broad professional categories
recent_grads %>%
  mutate(percMale = round(100 * Men / Total, 0)) %>%
  select(Major, Total, Men, Women, percMale, Major_category, Median) %>%
  group_by(Major_category) %>%
  summarise(meanMale = mean(percMale),
            expSalary = median(Median)) %>%
  filter(!is.na(meanMale)) %>%
  mutate(mostlyMen = ifelse(meanMale < 30, "Mostly Women",
                            ifelse(meanMale > 60, "Mostly Men", "Mixed"))) %>%
  ggplot(aes(x=meanMale, y=expSalary, colour = mostlyMen)) +
  geom_point() +
  expand_limits(y = c(0, 90000), x = c(0, 100)) +
  scale_y_continuous(breaks = seq(0, 90000, 20000)) +
  scale_x_continuous(breaks=seq(10, 90, 10)) +
  scale_color_discrete(name = "Gender Balance",
                       breaks = c("Mostly Women", "Mixed", "Mostly Men")) +
  theme_minimal() +
  theme(legend.position = "top", legend.direction = "horizontal") +
  labs(title = "Expected Salary by Education Category",
       subtitle = "Mapping income to relative gender balance",
       y="Median salary ($)", x="Percentage of degrees earned by males",
       caption = "Source: fivethirtyeight.com")

# Trend graphs showing pay gender imbalance by category of college major
# (faceted by Major_category)
recent_grads %>%
  # filter(Major_category == "Industrial Arts & Consumer Services") %>%
  select(Major_category, Major, ShareWomen, Median) %>%
  arrange(ShareWomen) %>%
  ggplot(aes(x = round(100 * (1-ShareWomen), 0), y = Median)) +
  geom_point(aes(colour=Major_category)) +
  facet_wrap(Major_category ~ .) +
  expand_limits(y = c(0, 90000), x = c(0, 100)) +
  scale_y_continuous(breaks = seq(0, 90000, 30000)) +
  scale_x_continuous(breaks=seq(0, 90, 25)) +
  theme_minimal() +
  theme(legend.position = "none") +
  stat_smooth(method = "lm", formula = y ~ x,
              size = 0.5, color = "grey50", se = FALSE) +
  labs(title = "Expected Salary by Education Category",
       subtitle = "Mapping income to relative gender balance",
       y="Median salary ($)", x="Percentage of degrees earned by males",
       caption = "Source: fivethirtyeight.com")


