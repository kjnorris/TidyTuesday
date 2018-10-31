library(tidyverse)
library(broom)
library(lubridate)
library(jsonlite)
library(maptools)
library(RColorBrewer)

tuesday_downloads <- read_csv('./Data/r-downloads.csv')

# Select the top 25 nations by download count - HK will be dropped
top_nations <- tuesday_downloads %>%
  filter(!is.na(country)) %>%
  group_by(country) %>%
  tally() %>%
  top_n(25) %>%
  arrange(desc(n)) %>%
  select(iso2 = country, n)

# Get information including name and ISO code for each country
urls <- paste0('https://restcountries.eu/rest/v2/alpha/', top_nations$iso2)
country_details <- urls %>% map(fromJSON)

# Get the regions and names for the top countries
top_regions <- unlist(map(country_details, `$`, 'region'))
top_names <- unlist(map(country_details, `$`, 'name'))

# Update the data frame to include name and region
top_nations <- top_nations %>%
  bind_cols(data_frame(region = unlist(map(country_details, `$`, 'region'))),
            data_frame(name = unlist(map(country_details, `$`, 'nativeName'))))

# Simplemaps back link: https://simplemaps.com/data/world-cities
# Load capital city data
world_cities <- read_csv('./Data/worldcities.csv') %>%
  filter(capital == 'primary') %>%
  distinct(iso2, .keep_all = TRUE) %>%
  select(iso2, city, lat, lng, country)

# Combine with top_nations data
top_nations <- top_nations %>%
  left_join(world_cities, by = "iso2") %>%
  select(iso2, downloads = "n", name, city, lat, long = "lng", country) %>%
  filter(iso2 != "HK")

# Jerusalem, Israel is not in the world cities database, add it manually
top_nations[top_nations$iso2 == "IL", "lat"] <- 31.771959
top_nations[top_nations$iso2 == "IL", "long"] <- 35.217018
top_nations[top_nations$iso2 == "IL", "country"] <- "Israel"


# Mapping - prepare a basic map frame for plotting
# includes latitude and longitude for borders, ISO2 code
# additional fields for order, hole, piece, and group (unused)
data("wrld_simpl")
new_wrld <- wrld_simpl %>%
  tidy(region="ISO2")

# Add region_num to color map by region/continent
new_wrld2 <- data_frame(id = as.character(wrld_simpl$ISO2),
                        region_num = as.character(wrld_simpl$REGION)) %>%
  full_join(new_wrld, by='id')

top_nations <- new_wrld2 %>%
  distinct(id, .keep_all = T) %>%
  select(id, region_num) %>%
  right_join(top_nations, by=c("id"="iso2"))


new_wrld2 %>%
  ggplot(aes(x=long, y=lat, map_id = id)) +
  geom_map(map=new_wrld2, color="grey60", fill="White") +
  geom_point(data=top_nations, aes(size = downloads*10, colour=factor(country))) +
  geom_text(data=top_nations, aes(label=downloads),
             colour="Black", fontface="bold", check_overlap=TRUE) +
  scale_colour_manual(values = colorRampPalette(brewer.pal(8,"Set2"))(25)) +
  scale_y_continuous(breaks = seq(-60, 80, 20), limits = c(-60, 80)) +
  scale_x_continuous(breaks = seq(-180, 180, 60), limits = c(-180, 180)) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "lightblue"),
        panel.grid = element_blank(),
        legend.position = "right") +
  guides(size = "none", colour = guide_legend(title="Country")) +
  labs(title = "R Downloads by Country",
       subtitle = "Top 24 download locations on Tuesday, October 23, 2018",
       y="",
       x="",
       caption = "Source: cran-logs.rstudio.com")


