# Load necessary libraries
library(tidyverse)
library(gghighlight)
library(lubridate)


# Load the relevant files
# Basic GitHub Content: https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-19/
# Available Files:
# phd_by_field.csv : PhDs awarded in the US by field (broad, major, fine) and year

phd_field <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-19/phd_by_field.csv")

total_phds <- function(get_year) {
  total <- phd_field %>%
    filter(year == get_year) %>%
    summarise(total = sum(n_phds, na.rm=TRUE)) %>%
    pull(total)
  return(total)
}

phd_share <- function(get_year) {
  total <- phd_field %>%
    filter(year == get_year) %>%
    group_by(broad_field) %>%
    summarise(field_phds = sum(n_phds, na.rm=TRUE)) %>%
    mutate(share = round(100 * field_phds / total_phds(get_year), 0)) %>%
    select(field = broad_field, share)
  total$year <- get_year
  return(total)
}


y_loc_2008 <- c(44, 39, 35, 25, 13, 5)
y_loc_2017 <- c(50, 46, 42, 28, 15, 5)

perc_2008 <- phd_share(2008) %>%
  filter(field != "Other") %>%
  mutate(share = paste0(share, "%")) %>%
  bind_cols(select(.), y_loc = y_loc_2008)

perc_2017 <- phd_share(2017) %>%
  filter(field != "Other") %>%
  mutate(share = paste0(share, "%")) %>%
  bind_cols(select(.), y_loc = y_loc_2017)

phd_labels <- tibble(field = c("Education", "Engineering",
                               "Humanities/Arts", "Life Sci.",
                               "Math/Comp. Sci.",
                               "Psych./Social Sci."),
                     x_loc = rep(2017, 6),
                     y_loc = y_loc_2017)

phd_field %>%
  group_by(broad_field, year) %>%
  mutate(n_phds = n_phds / 1000) %>%
  summarise(total = sum(n_phds, na.rm = TRUE)) %>%
  filter(broad_field != "Other") %>%
  select(broad_field, year, total) %>%
  ggplot() +
  geom_area(aes(x=year, y=total, fill=broad_field),
            colour="black", size=.2, alpha=.4) +
  geom_text(aes(x = year, y = y_loc, label = share),
            data=perc_2008, nudge_x = 0.5) +
  geom_text(aes(x = year, y = y_loc, label = share),
            data=perc_2017, nudge_x = -0.5) +
  geom_text(aes(x = x_loc, y = y_loc, label = field),
            data = phd_labels, hjust = 0, nudge_x = 0.05) +
  scale_fill_brewer(palette="Blues") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 55), breaks = seq(0, 55, 10)) +
  scale_x_continuous(limits = c(2008, 2019),
                     breaks = seq(2008, 2017, 2)) +
  theme(legend.position = "none", legend.direction = "horizontal") +
  labs(title = "PhDs Awarded: 2008 - 2017",
       subtitle = "More graduates with minor shifts in major",
       y="Thousands of Graduates", x="Year",
       caption = "Source: NSF",
       fill = "Field of Study")

# Engineering analysis
phd_field %>%
  filter(broad_field == "Engineering") %>%
  select(field, year, n_phds) %>%
  filter(!is.na(n_phds)) %>%
  ggplot(aes(x=year, y=n_phds)) +
  geom_line(aes(colour=field)) +
  gghighlight(field %in% c("Systems engineering",
                          "Operations research (engineering)",
                          "Engineering management, administration"),
              use_direct_label = FALSE, unhighlighted_colour = "grey90",
              use_group_by = FALSE) +
  facet_wrap(~field) +
  scale_colour_brewer(palette="Dark2") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 500), breaks = seq(0, 500, 100)) +
  scale_x_continuous(limits = c(2008, 2017), breaks = seq(2008, 2017, 2)) +
  theme(legend.position = "none", legend.direction = "horizontal") +
  labs(title = "Engineering PhDs Awarded: 2008 - 2017",
       subtitle = "Industrial Engineering specialties underrepresented but growing",
       y="Graduates", x="Year",
       caption = "Source: NSF",
       fill = "Field of Study")

