library(tidyverse)

us_wind <- read_csv('./Data/us_wind.csv')

us_wind %>%
  distinct(t_fips, .keep_all = TRUE) %>%
  select(t_state, t_county, p_year, p_tnum, p_cap, xlong, ylat)

