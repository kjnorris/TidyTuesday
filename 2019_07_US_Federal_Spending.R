# Load necessary libraries
library(tidyverse)
library(lubridate)


# Load the relevant files
# Basic GitHub Content: https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-12/
# Available Files:
# fed_r_d_spending.csv : Total Federal R&D Spending by agency/deparment
# energy_spending.csv : Energy Departments Data
# climate_spending.csv : Global Climate Change Research Program Spending

fed_rd <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-12/fed_r_d_spending.csv")
energy_spend <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-12/energy_spending.csv")
climate_spend <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-12/climate_spending.csv")

# Setup the big spenders
big_spenders <- fed_rd %>%
  filter(year >= 1981, year <= 2016) %>%
  filter(department %in% c("DOD", "HHS")) %>%
  mutate(rd_budget = rd_budget/1.0e+10,
         budget_size = "Large") %>%
  select(department, year, rd_budget, budget_size)

# Do the same for the small fries
small_spenders <- fed_rd %>%
  filter(year >= 1981, year <= 2016) %>%
  filter(department %in% c("DHS", "EPA")) %>%
  mutate(rd_budget = rd_budget/1.0e+9,
         budget_size = "Small") %>%
  select(department, year, rd_budget, budget_size)

# Combine the two spenders
interested_fed <- bind_rows(big_spenders, small_spenders)

# Graph
interested_fed %>%
  ggplot(aes(x = year, y = rd_budget)) +
  geom_line(aes(colour=department, linetype=budget_size), size=1,
            show.legend=c(colour = F, linetype = T)) +
  annotate("rect", xmin = 1981, xmax = 1989, ymin = 0, ymax = Inf,
           alpha = .1, fill = "red", colour = "grey70") +
  annotate("text", x = 1985, y = 9.5, label = "Ronald\nReagan") +
  annotate("rect", xmin = 1989, xmax = 1993, ymin = 0, ymax = Inf,
           alpha = .1, fill = "red", colour = "grey70") +
  annotate("text", x = 1991, y = 9.5, label = "George H.\nBush") +
  annotate("rect", xmin = 1993, xmax = 2001, ymin = 0, ymax = Inf,
           alpha = .1, fill = "blue", colour = "grey70") +
  annotate("text", x = 1997, y = 9.5, label = "Bill\nClinton") +
  annotate("rect", xmin = 2001, xmax = 2009, ymin = 0, ymax = Inf,
           alpha = .1, fill = "red", colour = "grey70") +
  annotate("text", x = 2005, y = 9.5, label = "George W.\nBush") +
  annotate("rect", xmin = 2009, xmax = Inf, ymin = 0, ymax = Inf,
           alpha = .1, fill = "blue", colour = "grey70") +
  annotate("text", x = 2013, y = 9.5, label = "Barack\nObama") +
  annotate("text", x = 2015, y = 8.0, label = "DOD", colour = "#CC0000") +
  annotate("text", x = 2015, y = 4.0, label = "HHS", colour = "#0000CC") +
  annotate("text", x = 2015, y = 1.5, label = "DHS", colour = "#CC0000") +
  annotate("text", x = 2016, y = 0.5, label = "NIH", colour = "#0000CC") +
  theme_minimal() +
  scale_colour_manual(name = "Department",
                      values = c("#CC0000", "#CC0000",
                                 "#0000CC", "#0000CC")) +
  scale_linetype_manual(name = "Budget Size",
                        values = c("solid", "dotted"),
                        labels = c("Big Budget", "Normal Budget")) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2.5),
                     sec.axis = sec_axis(~.*10,
                                         name = "R&D Budget - Big Budget\n(US$ Billions)",
                                         breaks = seq(0, 100, 25))) +
  scale_x_continuous(limits = c(1981, 2016),
                     breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015)) +
  theme(legend.position = "bottom", legend.direction = "horizontal") +
  labs(title = "Partisan Spending",
       subtitle = "Are Republican administrations more bellicose? Democrats more nurturing?",
       y="R&D Budget\n(US$ Billions)", x="Year",
       caption = "Source: AAAS")

fed_rd %>%
  filter(year >= 1981, year <= 2016) %>%
  filter(department %in% c("NIH", "EPA")) %>%
  mutate(rd_budget = rd_budget/1.0e+9) %>%
  ggplot(aes(x = year, y = rd_budget)) +
  geom_line(aes(colour=department), size=1) +
  annotate("rect", xmin = 1981, xmax = 1989, ymin = 0, ymax = Inf,
           alpha = .1, fill = "red", colour = "grey70") +
  annotate("text", x = 1985, y = 9.5, label = "Ronald\nReagan") +
  annotate("rect", xmin = 1989, xmax = 1993, ymin = 0, ymax = Inf,
           alpha = .1, fill = "red", colour = "grey70") +
  annotate("text", x = 1991, y = 9.5, label = "George H.\nBush") +
  annotate("rect", xmin = 1993, xmax = 2001, ymin = 0, ymax = Inf,
           alpha = .1, fill = "blue", colour = "grey70") +
  annotate("text", x = 1997, y = 9.5, label = "Bill\nClinton") +
  annotate("rect", xmin = 2001, xmax = 2009, ymin = 0, ymax = Inf,
           alpha = .1, fill = "red", colour = "grey70") +
  annotate("text", x = 2005, y = 9.5, label = "George W.\nBush") +
  annotate("rect", xmin = 2009, xmax = Inf, ymin = 0, ymax = Inf,
           alpha = .1, fill = "blue", colour = "grey70") +
  annotate("text", x = 2013, y = 9.5, label = "Barack\nObama") +
  theme_minimal() +
  scale_colour_manual(name = "Department",
                      values = c("#CC0000", "#0000CC")) +
  # scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2.5)) +
  scale_x_continuous(limits = c(1981, 2016),
                     breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015)) +
  theme(legend.position = "bottom", legend.direction = "horizontal") +
  labs(title = "Partisan Spending",
       subtitle = "Are Republican administrations more bellicose?",
       y="R&D Budget", x="Year",
       caption = "Source: AAAS")
