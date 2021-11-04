
# Script to produce nomination value plots

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


# Prepare data to plot
absdf <- dat %>%
    group_by(year) %>%
    mutate(
        safe = abs(mean(fitted4_ire, na.rm = TRUE) - fitted4_ire),
        safe_middle = abs(0.5 - fitted4_ire),
        safe_binary = fitted4_ire > 0.75 | fitted4_ire < 0.25,
        seat_type = case_when(
            Con > 0.5 ~ "Con",
            Con < 0.5 ~ "Lib",
            Con == 0.5 ~ "Tied"
        )
    ) %>%
    summarise(
        mean_middle = mean(safe_middle, na.rm = TRUE),
        mm_lower = get_ci(safe_middle, "lower"),
        mm_upper = get_ci(safe_middle, "upper")
    ) %>%
    filter(year != 1885 & year != 1832) %>%
    mutate(line_group = case_when(
        year < 1832 ~ "1",
        year %in% 1832:1866 ~ "2",
        year %in% 1867:1885 ~ "3",
        year > 1885 ~ "4",
        TRUE ~ "5"
    ))

# Plot absolute distance to 0.5
plot_safe <- ggplot(
    absdf,
    aes(x = year, y = mean_middle)
) +
    geom_ribbon(aes(
        ymin = mm_lower,
        ymax = mm_upper,
        group = line_group
    ),
    alpha = 0.2
    ) +
    geom_path(aes(group = line_group)) +
    geom_point(shape = 21) +
    geom_vline(xintercept = c(1832, 1867, 1885), lty = "dashed") +
    scale_colour_viridis_d(end = 0.8) +
    theme_tn() +
    labs(x = "Year", y = "Average distance of C_hat to 0.5 in year t")
ggsave(plot_safe,
    filename = "output/figures/safety.pdf",
    device = cairo_pdf,
    height = 5, width = 5
)
