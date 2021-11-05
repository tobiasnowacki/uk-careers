# Replication files for 'The Emergence of Party-Based Political Careers in the UK, 1801-1918'
# Cox & Nowacki (Journal of Politics, forthcoming)
# analysis_nomvalue.R: Analyse distribution of seat safety values

# -------------
# DEPENDENCIES
# -------------
source("code/0_functions.R")
library(extrafont)
library(RColorBrewer)

# --------------
# LOAD DATA
# --------------
dat <- import("output/mod_data/phat_vals.csv")[, -c(1:2)] %>%
    filter(year < 1918, year > 1802)

# -------------
# DENSITY PLOT
# -------------

density_plot <- ggplot(dat %>% filter(year != "1885" & year != "1832" & !is.na(grp))) +
    geom_density(aes(x = fitted4_ire),
        colour = "grey40", fill = "grey40"
    ) +
    facet_wrap(~grp) +
    theme_tn() +
    labs(x = "Expected Seat Value", y = "Density")
# Save plot
ggsave(density_plot,
    file = "output/figures/density.pdf",
    width = 5, height = 5,
    device = cairo_pdf
)

# -----------
# CHECK IF TWO-SEATERS ALSO BECOME MORE BIMODAL
# -----------

identify_twoseaters <- dat %>%
    group_by(constituency.id) %>%
    filter(grp == "1886 - 1910", total_seats > 1) %>%
    select(constituency.id) %>%
    distinct() %>%
    mutate(two_seat_flag = TRUE)

two_seater_df <- dat %>%
    inner_join(identify_twoseaters) %>%
    filter(total_seats == 2, patronal == 0)

density_plot_twoseater <- ggplot(two_seater_df %>% filter(year != "1885" & year != "1832" & !is.na(grp))) +
    geom_density(aes(x = fitted4_ire),
        colour = "grey40", fill = "grey40"
    ) +
    facet_wrap(~grp) +
    theme_tn() +
    labs(x = "Expected Seat Value", y = "Density")

# Save plot
ggsave(density_plot_twoseater,
    file = "output/figures/density_two.pdf",
    width = 5, height = 5,
    device = cairo_pdf
)