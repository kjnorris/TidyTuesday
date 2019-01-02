# Load libraries
library(tidyverse)
library(ggmap)
library(RColorBrewer)

# Read the data into a data file
us_wind <- read_csv('./Data/us_wind.csv')

# Process the data for all installations with valid date and capacity
# Select only relevant variables related to location and capacity
# Only withinthe continental US (lower 48 states)
# Calculate decade from year operational
wind_cap <- us_wind %>%
  distinct(t_fips, .keep_all = TRUE) %>%
  select(state = t_state, county = t_county,
         year = p_year, turbines = p_tnum,
         capacity = p_cap, turbine = t_cap,
         lng = xlong, lat = ylat) %>%
  filter(state != "AK" & state != "HI" & state != "PR" & state != "GU") %>%
  filter(capacity > 0) %>% filter(turbine > 0) %>%
  mutate(decade = case_when(year >= 1980 & year < 1990 ~ "1980s",
                            year >= 1990 & year < 2000 ~ "1990s",
                            year >= 2000 & year < 2010 ~ "2000s",
                            year >= 2010               ~ "2010s",
                            TRUE                       ~ "Unknown"))

# Process the data for all installations with invalid date and capacity
# Select only relevant variables related to location and capacity
# Only withinthe continental US (lower 48 states)
# Calculate decade from year operational
wind_unknown <- us_wind %>%
  distinct(t_fips, .keep_all = TRUE) %>%
  select(state = t_state, county = t_county,
         year = p_year, turbines = p_tnum,
         capacity = p_cap, lng = xlong, lat = ylat) %>%
  filter(state != "AK" & state != "HI" & state != "PR" & state != "GU") %>%
  filter(capacity < 0) %>%
  mutate(decade = case_when(year >= 1980 & year < 1990 ~ "1980s",
                            year >= 1990 & year < 2000 ~ "1990s",
                            year >= 2000 & year < 2010 ~ "2000s",
                            year >= 2010               ~ "2010s",
                            TRUE                       ~ "Unknown"))


# Set scope for map plot - continental US
states <- c(left = -125, bottom = 25, right = -65, top = 50)

# Read map tiles from stamen mapping service
map <- get_stamenmap(states, zoom = 5, maptype = "terrain-background")

# Plot the map
# Add points for all installations - color by decade, size by capacity
# Add + for all invalid installations (unknown year/capacity)
ggmap(map) +
  geom_point(aes(x=lng, y=lat,
                 colour=factor(decade, levels=c("1980s", "1990s", "2000s",
                                                "2010s", "Unknown")),
                 size=capacity),
             data=wind_cap) +
  geom_point(aes(x=lng, y=lat), shape=3, colour = "grey60",
             size=3, data=wind_unknown) +
  scale_colour_manual(values = colorRampPalette(brewer.pal(9,"Blues"))(10)[3:8]) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(title="Decade Operational"),
         size="none") +
  labs(title = "Developing Wind Power in the United States",
       subtitle = "Installed growth by decade and capacity",
       y="",
       x="",
       caption = "Source: usgs.gov")

# Create graph with2 y-axes to track changes in total capacity & turbine output
# Summarise total capacity and average turbine capacity (efficiency)
# Plot total output as a bar chart, turbine efficiency as a line graph
wind_cap %>%
  group_by(year) %>%
  filter(year <= 2017 & year >= 2000) %>%
  summarise(annual_capacity = sum(capacity),
            average_turbine = mean(turbine)) %>%
  ggplot(aes(x=year)) +
  geom_bar(aes(y=annual_capacity), stat="identity", fill="#3333FF") +
  geom_line(aes(y=average_turbine), color="#990000") +
  expand_limits(y = c(0, 7000), x = c(2000, 2017)) +
  scale_y_continuous(breaks = seq(0, 7000, 1000),
                     sec.axis = sec_axis(~.,
                                         name="Average Turbine Capacity (kW)",
                                         breaks = seq(0, 3000, 1000),
                                         labels = seq(0, 3000, 1000)),) +
  scale_x_continuous(breaks=seq(2000, 2017, 3)) +
  theme_minimal() +
  theme(legend.position = "top", legend.direction = "horizontal") +
  labs(title = "Rising Output and Efficiency Since 2000",
       subtitle = "Total capacity installed vs. average turbine output per year",
       y="Total Capacity (MW)", x="",
       caption = "Source: usgs.gov")
