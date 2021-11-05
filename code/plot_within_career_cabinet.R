# Replication files for 'The Emergence of Party-Based Political Careers in the UK, 1801-1918'
# Cox & Nowacki (Journal of Politics, forthcoming)
# plot_within_career_cabinet.R: Analyse seat safety before and after Cabinet appt

# Dependencies --------------
library(tidyverse)
library(devtools)
library(rio)
source_url("https://raw.githubusercontent.com/tobiasnowacki/RTemplates/master/plottheme.R")

# Load data ----------------
within_dat <- import("output/mod_data/within_career.csv") %>%
    filter(!is.na(grp), !is.na(party_pruned), year >= 1832, year < 1918)
steps <- import("output/mod_data/stepping_offices_by_term.csv")

# Merge data ---------------
mergedat <- within_dat %>%
    left_join(steps, by = c("member_id", "year")) %>%
    group_by(member_id) %>%
    # Identify and keep those who ever served in Cabinet whilst elected
    mutate(ever_cabinet = sum(in_cabinet, na.rm = TRUE) > 0) %>%
    filter(ever_cabinet == TRUE) %>%
    mutate(first_cabinet = min(year[in_cabinet == TRUE], na.rm = TRUE)) %>%
    mutate(
        term = row_number(),
        cab_term = term[year == first_cabinet],
        grp_term = grp[year == first_cabinet],
        term_diff = term - cab_term
    ) %>%
    # Only keep instances in the same era
    filter(grp == grp_term)

# Aggregate up -----------
aggdat <- mergedat %>%
    group_by(grp, term_diff) %>%
    summarise(
        avg_safety = mean(party_safety, na.rm = TRUE)
    ) %>%
    filter(abs(term_diff) < 3) %>%
    filter(grp != "1868 - 1880") # Exclude noisy middle

# Plot -------------------
ggplot(aggdat, aes(x = term_diff, y = avg_safety)) +
    geom_vline(xintercept = 0, lty = "dotted") +
    geom_line(aes()) +
    geom_point(aes()) +
    facet_wrap(. ~ grp) +
    labs(x = "Terms Since Cabinet", y = "Avg Safety") +
    scale_y_continuous(limits = c(0, 1)) +
    theme_tn()
# Export plot
ggsave("output/figures/within_cabinet_safety.pdf",
    width = 6, height = 4,
    device = cairo_pdf
)