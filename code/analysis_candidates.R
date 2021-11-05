# Replication files for 'The Emergence of Party-Based Political Careers in the UK, 1801-1918'
# Cox & Nowacki (Journal of Politics, forthcoming)
# analysis_candidates.R: Perform candidate-level analyses

# Dependencies ------------------------------------------------
library(tidyverse)
library(rio)
library(viridis)
library(lfe)
library(broom)
library(devtools)
source_url("https://raw.githubusercontent.com/tobiasnowacki/RTemplates/master/plottheme.R")
source("code/0_functions.R")


# Load data --------------------------------------------------
df <- import("output/mod_data/candidates_cabinet.csv")

# Aggregate data ---------------------------------------------
attempts <- df %>%
  filter(year < 1911, year >= 1802, mp_switch_between == FALSE)

# First attempt running --------------------------------------
# Only get the first time a candidate is running
firstc <- attempts %>%
  filter(patronal == 0, is_ire_val == TRUE) %>%
  filter(attempt == 1) %>%
  filter(party %in% c("C", "L")) %>%
  ungroup()

# Making sure that we only run this on constituencies that are not patronal
# table(firstc$patronal, !is.na(firstc$fitted4))
# table(firstc$year, !is.na(firstc$fitted4))
# table(firstc$year, firstc$patronal)

# Get mean c_hat by year
means <- firstc %>%
  group_by(constituency.name) %>%
  filter(patronal == 0, is_ire_val == TRUE) %>%
  arrange(year) %>%
  mutate(fit_lg = lag(fitted4_ire)) %>%
  ungroup() %>%
  group_by(year, party) %>%
  summarise(
    mean_fit = mean(fitted4_ire, na.rm = TRUE),
    mean_unopp = mean(unopposed_lg, na.rm = TRUE),
    mean_win = mean(winner),
    mean_run_again_loser = mean(again[winner == 0], na.rm = TRUE),
    mean_run_again_winner = mean(again[winner == 1], na.rm = TRUE)
  ) %>%
  mutate(lgrp = case_when(
    year < 1832 ~ "1",
    year %in% 1832:1866 ~ "2",
    year %in% 1867:1885 ~ "3",
    year > 1885 ~ "4",
    TRUE ~ "5"
  )) %>%
  filter(party %in% c("C", "L")) %>%
  filter(year != 1832 & year != 1885)

# Get winning proportion across parties for new candidates
means_cross_party <- firstc %>%
  group_by(constituency.name) %>%
  filter(patronal == 0) %>%
  arrange(year) %>%
  ungroup() %>%
  group_by(year) %>%
  summarise(
    mean_win = mean(winner),
    win_low = get_ci(winner, "lower"),
    win_high = get_ci(winner, "upper"),
    sd_win = sd(winner)
  ) %>%
  mutate(lgrp = case_when(
    year <= 1831 ~ "1",
    year %in% 1832:1867 ~ "2",
    year %in% 1868:1884 ~ "3",
    year >= 1885 ~ "4",
    TRUE ~ "5"
  )) %>%
  filter(year != 1832 & year != 1885)

# Calculate the difference in score between parties
diff_by_year <- firstc %>%
  group_by(constituency.name) %>%
  filter(patronal == 0, is_ire_val == TRUE, year > 1802) %>%
  arrange(year) %>%
  mutate(fit_lg = lag(fitted4)) %>%
  ungroup() %>%
  group_by(year) %>%
  summarise(
    mean_fit_diff = mean(fitted4_ire[party == "L"], na.rm = TRUE) - mean(fitted4_ire[party == "C"], na.rm = TRUE),
    mean_fit_diff_low = t.test(fitted4_ire[party == "L"], fitted4_ire[party == "C"], conf = 0.95)$conf[1],
    mean_fit_diff_high = t.test(fitted4_ire[party == "L"], fitted4_ire[party == "C"], conf = 0.95)$conf[2]
  ) %>%
  mutate(lgrp = case_when(
    year <= 1831 ~ "1",
    year %in% 1832:1867 ~ "2",
    year %in% 1868:1884 ~ "3",
    year >= 1885 ~ "4",
    TRUE ~ "5"
  )) %>%
  filter(year != 1832 & year != 1885)

# Proportion of *new* candidates winning (across parties)
plot_newcross <- ggplot(means_cross_party, aes(year, mean_win)) +
  geom_vline(xintercept = c(1832, 1867, 1885), lty = "dashed") +
  geom_ribbon(aes(
    ymin = win_low,
    ymax = win_high,
    group = lgrp
  ),
  alpha = 0.3
  ) +
  geom_point() +
  geom_line(aes(group = lgrp)) +
  scale_colour_manual(values = c("blue", "orange")) +
  labs(x = "Year", y = "Proportion of New Candidates Winning") +
  theme_tn()

ggsave("output/figures/new_win.pdf",
  width = 4, height = 4,
  device = cairo_pdf
)

# Summarise difference between parties, by year.
plot_diff <- ggplot(diff_by_year, aes(year, mean_fit_diff)) +
  geom_hline(yintercept = 0, colour = "grey") +
  geom_ribbon(aes(
    ymin = mean_fit_diff_low,
    ymax = mean_fit_diff_high,
    group = lgrp
  ),
  alpha = 0.3
  ) +
  geom_line(aes(group = lgrp)) +
  geom_point(shape = 21) +
  geom_vline(xintercept = c(1832, 1867, 1885), lty = "dashed") +
  labs(x = "Year", y = expression(paste("Party Difference in ", hat(C)[t]))) +
  theme_tn()
ggsave(plot_diff,
  file = "output/figures/new_partydiff.pdf",
  width = 4,
  height = 4,
  device = cairo_pdf
)