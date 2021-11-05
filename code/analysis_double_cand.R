# Replication files for 'The Emergence of Party-Based Political Careers in the UK, 1801-1918'
# Cox & Nowacki (Journal of Politics, forthcoming)
# analysis_double_cand.R: Check for double candidates


### ------------
### DEPENDENCIES
### ------------

library(tidyverse)
library(rio)
source("code/0_functions.R")

### ------------
### LOAD DATA
### ------------

df <- import("output/mod_data/candidates_stats.csv")

### ------------
### AGGREGATE
### ------------

# Check how often the same candidate would run multiple times per year
double_by_year <- df %>%
    filter(year < 1911) %>%
    group_by(year, name, grp) %>%
    summarise(count = n()) %>%
    group_by(year, grp) %>%
    filter(!is.na(grp)) %>%
    summarise(share = sum(count > 1) / n())

### ------------
### PLOT
### ------------

plot_double <- ggplot(
    double_by_year,
    aes(year, share)
) +
    geom_point() +
    geom_line(aes(group = grp)) +
    theme_tn() +
    ref_years() +
    labs(x = "Year", y = "Share of Candidates With Multiple Nominations")

ggsave(plot_double,
    file = "output/figures/share_multiple.pdf",
    width = 4, height = 4, device = cairo_pdf
)