library(tidyverse)
library(gghighlight)
library(gganimate)

# Load the relevant files
# Basic GitHub Content: https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/
# Available Files:
# jobs_gender.csv : Workers in various fielkds by gender
# earnings_female.csv : Female earnings asx a % of male earnings
# employed_gender.csv : Full-time/Part-time employment by gender

jobs_gender <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/jobs_gender.csv")
earnings_female <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/earnings_female.csv")
employed_gender <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/employed_gender.csv")

# Find the average pay gap by year
mean_disparity <- jobs_gender %>%
  group_by(year) %>%
  summarise(mean = mean(wage_percent_of_male, na.rm=TRUE))

# Find the average female participation % by year
mean_participation <- jobs_gender %>%
  group_by(year) %>%
  summarise(mean = mean(percent_female, na.rm=TRUE))

# Focus on engineering specialties
all_engineers <- jobs_gender %>%
  filter(minor_category == "Architecture and Engineering")

# Plot the pay gap (% of male salary) by year for each engineering specialty
# Highlight Architects (low), Mechanical Engineers (high), Industrial Engineers
# Add mean pay gap for reference
all_engineers %>%
  ggplot(aes(x = year, y = wage_percent_of_male)) +
  geom_line(aes(colour=occupation)) +
  gghighlight(occupation %in% c("Industrial engineers, including health and safety",
                                "Architects, except naval",
                                "Mechanical engineers"),
              use_direct_label = FALSE, unhighlighted_colour = "grey80",
              use_group_by = FALSE) +
  geom_line(data = mean_disparity, aes(x = year, y = mean),
            linetype = "dashed", colour = "black") +
  scale_colour_manual(values = c("red", "blue", "orange")) +
  annotate("text", x = 2015.77, y = 95,
           label = "Industrial Engineers",
           colour = "blue") +
  annotate("text", x = 2015.74, y = 100,
           label = "Mechanical Engineers",
           colour = "orange") +
  annotate("text", x = 2015.86, y = 75.5,
           label = "Architects",
           colour = "red") +
  annotate("text", x = 2015.85, y = 85.5,
           label = "US Average",
           colour = "black") +
  theme_minimal() +
  theme(legend.position = "none", legend.direction = "horizontal") +
  scale_y_continuous(limits = c(75, 100), breaks = seq(75, 100, 5)) +
  theme(legend.position = "none", legend.direction = "horizontal") +
  labs(title = "Underpaid: Female Engineers",
       subtitle = "Gender pay disparity varies by engineering specialty",
       y="Female Salary\nPercentage of Male Salary", x="Year",
       caption = "Source: Census Bureau")

# Plot the participation rate(% of female engineers) by year for each engineering specialty
# Highlight Architects (low), Mechanical Engineers (high), Industrial Engineers
# Add mean participation rate for reference
all_engineers %>%
  ggplot(aes(x = year, y = percent_female)) +
  geom_line(aes(colour=occupation)) +
  gghighlight(occupation %in% c("Industrial engineers, including health and safety",
                                "Architects, except naval",
                                "Mechanical engineers"),
              use_direct_label = FALSE, unhighlighted_colour = "grey80",
              use_group_by = FALSE) +
  geom_line(data = mean_participation, aes(x = year, y = mean),
            linetype = "dashed", colour = "black") +
  annotate("text", x = 2015.76, y = 21.5,
           label = "Industrial Engineers",
           colour = "blue") +
  annotate("text", x = 2015.74, y = 6,
           label = "Mechanical Engineers",
           colour = "orange") +
  annotate("text", x = 2015.86, y = 25,
           label = "Architects",
           colour = "red") +
  annotate("text", x = 2015.83, y = 37.5,
           label = "US Average",
           colour = "black") +
  scale_colour_manual(values = c("red", "blue", "orange")) +
  theme_minimal() +
  theme(legend.position = "none", legend.direction = "horizontal") +
  theme(legend.position = "none", legend.direction = "horizontal") +
  scale_y_continuous(limits = c(0, 38), breaks = seq(0, 38, 10)) +
  theme(legend.position = "none", legend.direction = "horizontal") +
  labs(title = "Help Wanted: Female Engineers",
       subtitle = "Engineering is still male dominated",
       y="Percentage of Female Engineers", x="Year",
       caption = "Source: Census Bureau")
