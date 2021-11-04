# Script to produce Figure 4 in paper

# Load dependencies ------------
library(tidyverse)
library(rio)
# library(viridis)
# library(broom)
library(devtools)
source_url("https://raw.githubusercontent.com/tobiasnowacki/RTemplates/master/plottheme.R")
source("code/0_functions.R")

# Load data -------------------
df <- import("output/mod_data/candidates_cabinet.csv")

# Restrict to timeframe of interest 
attempts <- df %>% 
  filter(year < 1911, year >= 1802, mp_switch_between == FALSE) 

# Analysis -------------------

# Summarise stats by year
sitting_by_year <- attempts %>%
  filter(patronal == 0, is_ire_val == TRUE) %>%  
  filter(party %in% c("C", "L"), mp_switch_between == FALSE) %>%
  group_by(year, winner) %>%
  summarise(run_val = mean(again),
            run_low = get_ci(again, "lower"),
            run_high = get_ci(again, "upper"),
            win_val = mean(win_fut, na.rm = TRUE),
            win_low = get_ci(win_fut, "lower"),
            win_high = get_ci(win_fut, "upper"),
            no = n()) %>%
  ungroup() %>%
  mutate(elected = sum(no[winner == 1]),
         winner = case_when(
            winner == 0 ~ "Lost in t",
            winner == 1 ~ "Won in t",
            TRUE ~ ""
         )) 
  
# Proportion of candidates running again
run_win_again <- sitting_by_year %>%
  pivot_longer(c(run_val, run_low, run_high, win_val, win_low, win_high),
              names_to = c("type")) %>%
  mutate(type_gr = str_extract(type, "_(.*)"),
         type = str_extract(type, "(.*)_")) %>%
  pivot_wider(names_from = type_gr, 
              values_from = value,
              ) %>%
  rename(val = `_val`, low = `_low`, high = `_high`) %>%
  mutate(type = case_when(
    type == "run_" ~ "Running Again in t+1",
    type == "win_" ~ "Winning in t+1 | Running Again in t+1"
  ),
        lgrp = case_when(
    year < 1832 ~ "1",
    year %in% 1832:1866 ~ "2",
    year %in% 1867:1880 ~ "3", 
    year >= 1885 ~ "4",
    TRUE ~ "5"
  )) %>%
  filter(year != 1831, year != 1880, year < 1911)

# Plot data
plot_run_win <- ggplot(run_win_again, aes(x = year)) +
  geom_ribbon(aes(ymin = low, ymax = high, group = interaction(winner, lgrp)), alpha = 0.3) +
  geom_line(aes(y = val, lty = winner, group = interaction(winner, lgrp))) +
  facet_wrap(. ~ type) +
  geom_vline(xintercept = c(1832, 1867, 1885), lty = "dotted", lwd = 0.5, colour = "grey50") + 
  labs(x = "Year", y = "Proportion", lty = "Type") +
  theme_tn()
ggsave(plot_run_win, 
  file = "output/figures/running_again.pdf", width = 8, height = 5,
  device = cairo_pdf)
