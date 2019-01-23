# Load necessary libraries
library(tidyverse)

# Load the relevant files
# pretrial_summary <- read_csv(here::here("Data/2019_04_pretrial_summary.csv"))
pretrial_population <- read_csv(here::here("Data/2019_04_pretrial_population.csv"))
prison_population <- read_csv(here::here("Data", "2019_04_prison_population.csv"))

pa_pretrial <- pretrial_population %>%
  filter(!is.na(pretrial_population) & pretrial_population > 0) %>%
  filter(state == "PA" &
           county_name %in% c("Philadelphia County", "Allegheny County") &
           pop_category %in% c("Male", "Female")) %>%
  mutate(total_pretrial = round(population/100000*pretrial_population,0)) %>%
  select(year, county_name, pop_category, population,
         pretrial_population, total_pretrial)

pa_total <- pretrial_population %>%
  filter(!is.na(pretrial_population) & pretrial_population > 0) %>%
  filter(state == "PA" &
           county_name %in% c("Philadelphia County", "Allegheny County") &
           pop_category == "Total") %>%
  mutate(total_pretrial = round(population/100000*pretrial_population,0)) %>%
  select(year, county_name, pop_category, population,
         pretrial_population, total_pretrial)

pa_pretrial %>%
  filter(year < 2010) %>%
  ggplot(aes(x=year, colour=pop_category)) +
  geom_line(aes(y=total_pretrial)) +
  scale_y_continuous(limits = c(0, 25000), breaks = seq(0, 20000, 5000)) +
  scale_x_continuous(limits = c(1970, 2010), breaks = seq(1970, 2010, 5)) +
  facet_wrap(~county_name) +
  theme_minimal() +
  scale_color_manual(name = "",
                     breaks = c("Male", "Female"),
                     values = c("black", "blue")) +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(title = "Pretrial Population in PA's Largest Cities",
       subtitle = "Sometimes the wheels of justice grind slowly",
       y="People Awaiting Trial", x="Year",
       caption = "Source: Vera Institute")

# prison population
pa_prison <- prison_population %>%
  filter(!is.na(prison_population) & prison_population > 0) %>%
  filter(state == "PA" &
           county_name %in% c("Philadelphia County", "Allegheny County") &
           pop_category %in% c("Male", "Female")) %>%
  mutate(total_prison = round(population/100000*prison_population,0)) %>%
  select(year, county_name, pop_category, population,
         prison_population, total_prison)

pa_prison_total <- prison_population %>%
  filter(!is.na(prison_population) & prison_population > 0) %>%
  filter(state == "PA" &
           county_name %in% c("Philadelphia County", "Allegheny County") &
           pop_category == "Total") %>%
  mutate(total_prison = round(population/100000*prison_population,0)) %>%
  select(year, county_name, pop_category, population,
         prison_population, total_prison)

pa_prison %>%
  ggplot(aes(x=year, colour=pop_category)) +
  geom_line(aes(y=total_prison)) +
  geom_line(aes(y=population/20, colour=pop_category), data=pa_prison_total) +
  scale_y_continuous(limits = c(0, 80000), breaks = seq(0, 75000, 10000),
                     sec.axis = sec_axis(~.*20, name = "Population",
                                         breaks = c(0, 250000, 500000,
                                                    750000, 1000000, 1250000)))+
  scale_x_continuous(limits = c(1983, 2015), breaks = seq(1985, 2015, 5)) +
  facet_wrap(~county_name) +
  theme_minimal() +
  scale_color_manual(name = "",
                     breaks = c("Male", "Female", "Total"),
                     labels = c("Male Inmates", "Female Inmates", "Total Population"),
                     values = c("black", "blue", "red")) +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(title = "Prison Population in PA's Largest Cities",
       subtitle = "Increasing prison populations while overall populations decline",
       y="Inmates", x="Year",
       caption = "Source: Vera Institute")



# Utility to check summary statistics
pa_prison_total %>%
  group_by(county_name) %>%
  summarize(min = min(population), mean = mean(population), max = max(population),
            min_p = min(total_prison), mean_p = mean(total_prison),
            max_p = max(total_prison))
