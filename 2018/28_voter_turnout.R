library(tidyverse)

voter_turnout <- read_csv('./Data/voter_turnout.csv')

president_years <- seq(1980, 2012, 4)
midterm_years <- seq(1982, 2014, 4)

voter_turnout <- voter_turnout %>%
    mutate(election_type = case_when(year %in% president_years ~ "President",
                                     year %in% midterm_years ~ "Midterm")) %>%
    mutate(perc = round(100 * votes/eligible_voters, 0)) %>%
    select(year, state, election_type, perc, alphanumeric_state_code)

US <- voter_turnout %>%
  filter(alphanumeric_state_code == 0)

top <- voter_turnout %>%
    group_by(year) %>%
    top_n(1, wt=perc)

bottom <- voter_turnout %>%
    group_by(year) %>%
    top_n(-1, wt=perc)

best_worst <- top %>%
    full_join(bottom, by=c("year", "election_type")) %>%
    select(year, election_type, best = perc.x, worst = perc.y)

best_worst %>%
    ggplot(aes(x=year)) +
    geom_point(aes(y=best, colour=election_type), alpha=0.4) +
    geom_point(aes(y=worst, colour=election_type), alpha=0.4) +
    geom_segment(aes(y=worst, yend=best, xend=year), color="grey50") +
    geom_smooth(data=US, aes(y=perc), col="grey50", linetype='dashed', se=FALSE) +
    annotate("text", x = 2012, y = 30, label = "US Average\n(dashed line)",
             col = "grey50") +
    theme_minimal() +
    theme(legend.position = "bottom") + guides(size=FALSE) +
    expand_limits(y=0) +
    scale_y_continuous(labels = paste0(seq(00,100,20),"%"),
                       breaks = seq(0, 100, 20)) +
    scale_x_continuous(breaks=seq(1982, 2012, 4)) +
    scale_colour_manual(values=c("blue", "red"),
                        name="Election Type") +
    labs(title = "Voter Turnout (1980 to 2014)",
         subtitle = "Impact of the presidential race on voter engagement",
         y="Turnout (% of Eligible Voters) ", x="",
         caption = "Source: data.world")


