
# Script to produce nomination value plots

# -------------
# DEPENDENCIES
# -------------
source("code/0_functions.R")
library(extrafont)
library(RColorBrewer)
# ---------------------------------
# PATRONAL CONSTITUENCIES ANALYSIS
# ---------------------------------

# LOAD PATRONAL ONLY DATA
dat_pat <- import("output/mod_data/phat_vals_patronal_only.csv")[, -1]

# DENSITY PLOT
plot_patdens <- ggplot(dat_pat %>% filter(year != "1885" & year != "1832" & !is.na(grp))) +
    geom_density(aes(x = fitted4),
        colour = "grey40", fill = "grey40"
    ) +
    facet_wrap(~grp) +
    theme_tn() +
    labs(x = "Expected Seat Value", y = "Density")
ggsave("output/figures/density_patronal.pdf",
    width = 4, height = 4,
    device = cairo_pdf
)

# SEAT SAFETY FOR PATRONAL SEATS
# Test deviation from 0.5
absdf_pat <- dat_pat %>%
    group_by(year) %>%
    mutate(
        safe = abs(mean(fitted4, na.rm = TRUE) - fitted4),
        safe_middle = abs(0.5 - fitted4),
        seat_type = case_when(
            Con > 0.5 ~ "Con",
            Con < 0.5 ~ "Lib",
            Con == 0.5 ~ "Tied"
        )
    ) %>%
    summarise(
        mean_middle = mean(safe_middle, na.rm = TRUE),
    ) %>%
    filter(year != 1885 & year != 1832) %>%
    mutate(line_group = case_when(
        year < 1832 ~ "1",
        year %in% 1832:1866 ~ "2",
        year %in% 1867:1885 ~ "3",
        year > 1885 ~ "4",
        TRUE ~ "5"
    ))

plot_patsafe <- ggplot(
    absdf_pat,
    aes(x = year, y = mean_middle)
) +
    geom_path(aes(group = line_group)) +
    geom_point(shape = 21) +
    geom_vline(xintercept = c(1832, 1867, 1885), lty = "dashed") +
    scale_colour_viridis_d(end = 0.8) +
    theme_tn() +
    labs(x = "Year", y = "Average distance of C_hat to 0.5 in year t")
ggsave(plot_patsafe,
    filename = "output/figures/safety_patronal.pdf",
    device = cairo_pdf,
    height = 3.5, width = 3.5
)