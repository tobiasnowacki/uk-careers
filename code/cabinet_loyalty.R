# Load dependencies
library(tidyverse)
library(rio)
library(lubridate)
library(lfe)
library(devtools)
library(stringr)
source_url("https://raw.githubusercontent.com/tobiasnowacki/RTemplates/master/plottheme.R")

source("code/0_functions.R")

## ------------
## LOAD DATA
## ------------

whip2 <- import("output/mod_data/whip_es.csv") %>%
  select(-V1)
# cand_cab <- import("output/mod_data/candidates_cabinet.csv")
steps = import("output/mod_data/stepping_offices_by_term.csv")

## --------------
## PREPARE DATA
## --------------

# Create indicator whether MP will ever have high office
mg = steps %>%
  left_join(whip2) %>%
  group_by(member_id) %>%
  mutate(ever_high_office = ifelse(
    sum(ever_cabinet == 1, na.rm = TRUE) > 0 | 
    sum(ever_step == 1, na.rm = TRUE) > 0, 
    1, 0))

high_office_df <- mg %>%
        filter(already_cabinet == FALSE & 
              is_govt == TRUE, 
               year <= 1918) %>%
        group_by(year, ever_high_office) %>%
        summarise(
                  cohesion_score = mean(cohesion, na.rm = TRUE),
                  elec_mean = mean(win_fut, na.rm = TRUE),
                  # score_mean = mean(score_inverse, na.rm = TRUE),
                 n = n()) %>%
        ungroup() %>%
        filter(year != 1885) %>%
        mutate(yrgrp = ifelse(year < 1885, 0, 1))

table(mg$year, mg$is_govt)

## -------------
## PLOT
## -------------



plot_high <- ggplot(high_office_df, aes(year, cohesion_score)) +
 geom_smooth(aes(lty = as.factor(ever_high_office)), se = FALSE,
   colour = "black", lwd = 0.5) +
  # geom_line(aes(lty = as.factor(ever_high_office_uncond)), 
    # colour = "black", lwd = 0.5) +
  geom_line(aes(group = year), alpha = 0.1) +
  geom_point(aes(shape = as.factor(ever_high_office)), alpha = 0.5) +
  geom_vline(xintercept = c(1867.5, 1884.5), lty = "dotted") +
  theme_tn() +
  xlim(1835, 1911) +
  labs(x = "Year", y = "Mean Agreement Rate", lty = "High Office Now Or in Future?", shape = "High Office Now Or in Future?")
ggsave(plot_high, 
  file = "output/figures/stepping_whip.pdf",
  width = 4, height = 4, device = cairo_pdf)


plot_highval <-ggplot(high_office_df, aes(year, elec_mean)) +
 geom_smooth(aes(lty = as.factor(ever_high_office)), se = FALSE,
   colour = "black", lwd = 0.5) +
  geom_line(aes(group = year), alpha = 0.1) +
  geom_point(aes(shape = as.factor(ever_high_office)), alpha = 0.5) +
  geom_vline(xintercept = c(1867.5, 1884.5), lty = "dotted") +
  theme_tn() +
  xlim(1835, 1920) +
  labs(x = "Year", y = "Share Re-Elected", lty = "High Office Now Or in Future?", shape = "High Office Now Or in Future?")
ggsave(plot_highval, 
  file = "output/figures/stepping_reelec.pdf",
  width = 4, height = 4, device = cairo_pdf)

