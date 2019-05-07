library(tidyverse)
library(lubridate)

# Load the relevant files
# Basic GitHub Content: https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-29/
# Available Files:
# seattle_pets.csv : pet registrations in Seattle
seattle_pets <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-26/seattle_pets.csv")
