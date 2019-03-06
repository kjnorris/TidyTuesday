# Load necessary libraries
library(tidyverse)


# Load the relevant files
# Basic GitHub Content: https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-26/
# Available Files:
# full_trains.csv : full data set for French train delays
# small_trains.csv : reduced data set for French train delays, tidying started

trains_raw <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-26/full_trains.csv")
small_trains <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-26/small_trains.csv")
