library(tidyverse)
library(RColorBrewer)

meals <- read_csv('./Data/thanksgiving_meals.csv')

cranberry_make_or_buy <- meals %>%
  filter(celebrate == 'Yes') %>%
  filter(main_dish == 'Turkey') %>%
  filter(cranberry == "Canned" | cranberry == "Homemade" | cranberry=="None") %>%
  filter(!is.na(age)) %>%
  select(main_dish, cranberry, community_type, age, gender,
         family_income, us_region)
cranberry_make_or_buy$main_dish <- factor(cranberry_make_or_buy$main_dish)
cranberry_make_or_buy$gender <- factor(cranberry_make_or_buy$gender)
cranberry_make_or_buy$age <- factor(cranberry_make_or_buy$age,
                                    levels=c("18 - 29", "30 - 44",
                                             "45 - 59", "60+"))

cranberry_make_or_buy$cranberry <- factor(cranberry_make_or_buy$cranberry,
                                          levels=c("Canned", "Homemade",
                                                   "None", "Other (please specify"),
                                          labels=c("Canned", "Homemade",
                                                   "None", "Other"))

cranberry_make_or_buy %>%
  ggplot(aes(cranberry)) +
  geom_bar(aes(fill=age)) +
  facet_wrap(aes(community_type)) +
  theme_minimal() +
  scale_fill_manual(values = colorRampPalette(brewer.pal(9,"Spectral"))(20)[1:4]) +
  theme(legend.position = "top", legend.direction = "horizontal") +
  labs(title = "Giving Thanks for Cranberries",
       subtitle = "Preparation by age group and location",
       y="Number of Households Serving", x="",
       caption = "Source: fivethirtyeight.com")
