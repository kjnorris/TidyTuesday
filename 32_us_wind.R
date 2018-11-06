library(tidyverse)

us_wind <- read_csv('./Data/us_wind.csv')

us_wind %>%
  distinct(t_fips, .keep_all = TRUE) %>%
  select(state = t_state, county = t_county,
         year = p_year, turbines = p_tnum,
         capacity = p_cap, lng = xlong, lat = ylat)

