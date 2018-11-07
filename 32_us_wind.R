library(tidyverse)
library(ggmap)
library(RColorBrewer)

us_wind <- read_csv('./Data/us_wind.csv')

wind_cap <- us_wind %>%
  distinct(t_fips, .keep_all = TRUE) %>%
  select(state = t_state, county = t_county,
         year = p_year, turbines = p_tnum,
         capacity = p_cap, lng = xlong, lat = ylat) %>%
  filter(state != "AK" & state != "HI" & state != "PR" & state != "GU") %>%
  filter(capacity > 0) %>%
  mutate(decade = case_when(year >= 1980 & year < 1990 ~ "1980s",
                            year >= 1990 & year < 2000 ~ "1990s",
                            year >= 2000 & year < 2010 ~ "2000s",
                            year >= 2010               ~ "2010s",
                            TRUE                       ~ "Unknown"))

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


# states <- c(left = -125, bottom = 25.75, right = -67, top = 49)
states <- c(left = -125, bottom = 25, right = -65, top = 50)
map <- get_stamenmap(states, zoom = 5, maptype = "terrain-background")
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
