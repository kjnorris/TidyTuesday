# Load necessary libraries
library(tidyverse)


# Load the relevant files
# Basic GitHub Content: https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-05/
# Available Files:
# milkcow_facts.csv : farm counts and costs
# state_hpi.csv : housing price index data from Freddie Mac
# mortgage_rates.csv : mortgage rate data - 30 yr, 15 yr, 5-1 hybrid
# recession_dates.csv : recessions from WikiPedia

# Column specifier for mortgage_rates - guesses logical for columns with
# lots of leading NA values
mortgage_cols <- cols(
  date = col_date(),
  fixed_rate_30_yr = col_double(),
  fees_and_pts_30_yr = col_double(),
  fixed_rate_15_yr = col_double(),
  fees_and_pts_15_yr = col_double(),
  adjustable_rate_5_1_hybrid = col_double(),
  fees_and_pts_5_1_hybrid = col_double(),
  adjustable_margin_5_1_hybrid = col_double(),
  spread_30_yr_fixed_and_5_1_adjustable = col_double()
)

state_hpi <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-05/state_hpi.csv")
mortgage_rates <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-05/mortgage.csv",
                                  col_types = mortgage_cols)
recession_dates <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-05/recessions.csv")
cpi_rates <- read_csv(here::here('Data', '2019_06_CPI_1971-2018.csv'),
                      skip = 1,
                      col_names = c("SeriesID", "Year", "Period", "Label",
                                    "Value", "Monthly",
                                    "Quarterly", "Annual"))
# Read CPI and prepare
cpi_rates$day <- "01"
cpi_rates <- cpi_rates %>%
  unite("Date", Label, day, sep=" ") %>%
  mutate(Date = as_date(Date)) %>%
  select(Date, Monthly, Quarterly, Annual)

# Monthly average for mortgage rates
mortgage_rates <- mortgage_rates %>%
  mutate(Year = year(date),
         Month = month(date)) %>%
  group_by(Year, Month) %>%
  summarise(Fixed30 = mean(fixed_rate_30_yr),
            Fixed15 = mean(fixed_rate_15_yr),
            ARM5 = mean(adjustable_margin_5_1_hybrid)) %>%
  mutate(Day = "01") %>%
  unite("Date", Year, Month, Day, sep="-") %>%
  mutate(Date = as_date(Date))

# Join mortgage and CPI by date (month and year)
combined_rates <-  mortgage_rates %>%
  left_join(cpi_rates, by="Date") %>%
  gather(index, rate, -Date)

# Plot fixed mortgage rates against annual CPI
combined_rates %>%
  filter(index %in% c("Fixed30", "Fixed15", "Annual")) %>%
  ggplot(aes(x=Date, y=rate)) +
  geom_line(aes(colour=index)) +
  scale_y_continuous(limits = c(-2, 19), breaks = seq(-2, 19, 2)) +
  scale_x_date(limits = c(as_date("1975-01-01"), as_date("2018-12-01")),
               # date_breaks = "10 years",
               date_labels = "%b %Y") +
  theme_minimal() +
  scale_color_manual(name = " ", values = c("blue", "grey70", "orange"),
                     labels = c("CPI Annual Rate", "15 Year Fixed Mortgage",
                                "30 Year Fixed Mortgage")) +
  theme(legend.position = "bottom", legend.direction = "horizontal") +
  labs(title = "Fixed Mortgage Rates and Annual CPI",
       subtitle = "Mortgage rates track CPI loosely, impact and lag may be changing",
       y="Rate (%)", x="Year",
       caption = "Source: Freddie Mac and BLS")

# House prices - minimum, maximum, and US average per month
state_hpi_trans <- state_hpi %>%
  arrange(year, month) %>%
  group_by(year, month) %>%
  summarise(Maximum = max(price_index),
            Minimum = min(price_index),
            USAverage = mean(us_avg)) %>%
  mutate(Day = "01") %>%
  unite("Date", year, month, Day, sep="-") %>%
  mutate(Date = as_date(Date)) %>%
  arrange(Date) %>%
  mutate(prev_year = lag(USAverage, n=12)) %>%
  mutate(HPIAnnualChange = 100*(USAverage - prev_year)/prev_year) %>%
  select(Date, Maximum, Minimum, USAverage, HPIAnnualChange)

cpi_rates_trans <- cpi_rates %>%
  mutate(CPIAnnual = ifelse(Annual < 0, 0, log(Annual)))

# Join pricing and CPI by date (month and year)
combined_prices <-  state_hpi_trans %>%
  left_join(cpi_rates, by="Date") %>%
  mutate(CPIAnnual = Annual) %>%
  gather(index, rate, -Date, -Minimum, -Maximum)

# Plot house price index against annual CPI
combined_prices %>%
  filter(index %in% c("HPIAnnualChange", "CPIAnnual")) %>%
  ggplot(aes(x=Date, y=rate)) +
  # geom_ribbon(aes(ymin=Minimum, ymax=Maximum), fill="grey70", alpha=0.2) +
  geom_line(aes(colour=index)) +
  scale_y_continuous(limits = c(-15, 16), breaks = seq(-15, 15, 5)) +
  scale_x_date(limits = c(as_date("1975-01-01"), NA),
               # date_breaks = "10 years",
               date_labels = "%b %Y") +
  theme_minimal() +
  scale_colour_manual(name = " ", values = c("blue", "orange"),
                      labels = c("CPI Annual Rate",  "HPI Annual Rate")) +
  theme(legend.position = "bottom", legend.direction = "horizontal") +
  labs(title = "Housing Price Index Unlinking from Annual CPI",
       subtitle = "House prices no longer seem to presage CPI changes",
       y="% Change", x="Year",
       caption = "Source: Freddie Mac and BLS")










