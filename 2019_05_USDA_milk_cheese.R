# Load necessary libraries
library(tidyverse)

# Load the relevant files
# Basic GitHub Content: https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-29/
# Available Files:
# milkcow_facts.csv : farm counts and costs
# fluid_milk_sales.csv : sales data by type
# milk_product_facts.csv : products by type and volume (pounds) per person
# clean_cheese.csv : milk used to produce cheese by type per person
# stats_milk_production.csv : production by state and region

raw_fluid_sales <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-29/fluid_milk_sales.csv")
raw_milk_cow <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-29/milkcow_facts.csv")
raw_products <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-29/milk_products_facts.csv")

# Clean up milk sales data including scaling sales volume
# Order milk_type factor by sales volume
fluid_sales <- raw_fluid_sales %>%
  mutate(pounds = pounds/1000000) %>%
  filter(!str_detect(milk_type, "Flavored|Eggnog|Buttermilk")) %>%
  select(year, milk_type, pounds)




# Clean up production data including scaling volums
milk_cow <- raw_milk_cow %>%
  mutate(pounds = milk_production_lbs/5000000) %>%
  select(year, pounds)

milk_cow$milk_type <- "Total Produced"

# Combine production and sales
total_milk <- fluid_sales %>%
  bind_rows(milk_cow) %>%
  mutate(milk_type = fct_reorder(milk_type, pounds))

total_milk$milk_type <- recode(total_milk$milk_type,
                               'Total Production' = 'Total Sales',
                               'Low Fat (1%)' = 'Low Fat 1%',
                               'Reduced Fat (2%)' = 'Reduced Fat 2%')

# Line plot of sales and production data
total_milk %>%
  ggplot(aes(x=year, y=pounds)) +
  geom_line(aes(colour=milk_type, linetype=milk_type), size=1,
            show.legend = c("colour" = TRUE, "linetype" = FALSE)) +
  scale_y_continuous(limits = c(0, 60000), breaks = seq(0, 60000, 10000),
                     sec.axis = sec_axis(~.*5,
                                         name = "Pounds Produced (in millions)",
                                         breaks = seq(125000, 225000, 25000)))+
  scale_x_continuous(limits = c(1975, 2017), breaks = seq(1975, 2017, 5)) +
  scale_linetype_manual(values = c(rep("solid", 4), "dashed", "solid")) +
  theme_minimal() +
  scale_color_brewer(name = " ", palette = "Set1") +
  theme(legend.position = "bottom", legend.direction = "horizontal") +
  labs(title = "Milk, It Does A Market Good",
       subtitle = "Sales Slow as Production Climbs",
       y="Pounds Sold (in millions)", x="",
       caption = "Source: USDA")

# Clean up milk product data
# Aggregate categories (Cheese, Ice Cream)
# Order product factor by sales volume
milk_products <- raw_products %>%
  mutate(Cheese = cheese_american + cheese_other + cheese_cottage,
         `Ice Cream` = frozen_ice_cream_regular + frozen_ice_cream_reduced_fat +
           frozen_sherbet + frozen_other,
         Butter = butter,
         Milk = fluid_milk,
         Yogurt = fluid_yogurt) %>%
  select(year, Milk, Yogurt, Cheese, `Ice Cream`, Butter) %>%
  gather("Product", "Consumption", -year) %>%
  mutate(Product = fct_reorder(Product, Consumption))

# Line plot of product consumption per person by year
milk_products %>%
  ggplot(aes(x=year, y=Consumption)) +
  geom_line(aes(colour=Product), size=1) +
  scale_y_continuous(limits = c(0, 250), breaks = seq(0, 250, 50)) +
  scale_x_continuous(limits = c(1975, 2017), breaks = seq(1975, 2017, 5)) +
  theme_minimal() +
  scale_color_brewer(name = " ", palette = "Set1") +
  theme(legend.position = "bottom", legend.direction = "horizontal") +
  labs(title = "America's Changing Tastes",
       subtitle = "Less milk, more cheese and yogurt",
       y="Annual Pounds per Person", x="",
       caption = "Source: USDA")



