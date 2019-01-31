library(tidyverse)
library(patchwork)

state_milk_production <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-29/state_milk_production.csv")

# Bonus: milk production by state and region
state_milk <- state_milk_production %>%
  mutate(pounds = milk_produced / 1000000) %>%
  group_by(region, state) %>%
  mutate(State = fct_reorder(state, pounds)) %>%
  ungroup() %>%
  select(region, State, year, pounds)

makeplot <- function(df) {
  ggplot(aes(x=year, y=pounds), data = df) +
    geom_line(aes(colour=State), size=1) +
    facet_wrap(vars(region))+
    scale_x_continuous(limits = c(1970, 2017), breaks = seq(1970, 2017, 5)) +
    theme_minimal() +
    theme(legend.position = "right", legend.direction = "vertical",
          axis.text.x = element_text(angle = 60, hjust = 1)) +
    labs(x = "", y = "Pounds (in millions)")
}

state_milk %>%
  filter(region == "Southern Plains") %>%
  makeplot() +
  labs(title = "Milk, It Does A Market Good",
       subtitle = "Sales Slow as Production Climbs",
       caption = "Source: USDA")

southern_plains <- state_milk %>%
  filter(region == "Southern Plains") %>%
  makeplot()

northeast <- state_milk %>%
  filter(region == "Northeast") %>%
  makeplot()

lake_states <- state_milk %>%
  filter(region == "Lake States") %>%
  makeplot() +
  labs(title = "Milking Growth by Region")

pacific <- state_milk %>%
  filter(region == "Pacific") %>%
  makeplot()

mountain <- state_milk %>%
  filter(region == "Mountain") %>%
  makeplot()

blank_square <- ggplot() +
  labs(caption = "Source: USDA")

(lake_states | pacific | southern_plains) / (mountain | northeast) +
  plot_layout(ncol = 3, widths = c(1, 1, 1))

(lake_states / mountain) + pacific + (southern_plains / northeast)

lake_states + pacific + southern_plains +
  mountain + northeast + blank_square +
  plot_layout(ncol=3)

