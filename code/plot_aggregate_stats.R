# Replication files for 'The Emergence of Party-Based Political Careers in the UK, 1801-1918'
# Cox & Nowacki (Journal of Politics, forthcoming)
# plot_aggregate_stats.R: Plot era-level statistics

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

# Aggregate statistics by election
elec_stats <- dat %>%
    filter(year > 1802) %>%
    group_by(year) %>%
    summarise(
        patronal_agg = sum(
            patronal == 1,
            na.rm = TRUE
        ) / n(),
        contested = sum(unopposed == 0, na.rm = TRUE) / n(),
        safe = sum(
            fitted4_ire[unopposed == 0] > 0.75 |
                fitted4_ire[unopposed == 0] < 0.25,
            na.rm = TRUE
        ) / sum(unopposed == 0 & !is.na(fitted4_ire), na.rm = TRUE)
    ) %>%
    mutate(
        grp = case_when(
            year < 1832 ~ "1806 - 1831",
            year > 1832 & year < 1868 ~ "1835 - 1865",
            year >= 1868 & year < 1885 ~ "1868 - 1880",
            year > 1885 ~ "1886 - 1910"
        ),
        split_voters = case_when(
            grp == "1806 - 1831" ~ 22,
            grp == "1835 - 1865" ~ 14,
            grp == "1868 - 1880" ~ 4.9,
            grp == "1886 - 1910" ~ 2.7
        ) / 100
    ) %>%
    pivot_longer(c(patronal_agg, contested, safe, split_voters)) %>%
    group_by(name, grp) %>%
    summarise(value = mean(value, na.rm = TRUE)) %>%
    filter(!is.na(grp)) %>%
    mutate(
        name = case_when(
            name == "contested" ~ "Contested",
            name == "patronal_agg" ~ "Patronal",
            name == "safe" ~ "Safe",
            name == "split_voters" ~ "Split Voters"
        )
    )

# Plot aggregated election statistics
plot_agg <- ggplot(
    elec_stats,
    aes(x = grp, y = value)
) +
    geom_point(aes(colour = name, shape = name)) +
    geom_line(aes(colour = name, lty = name, group = name)) +
    labs(
        x = "Era",
        y = "Share",
        colour = "Statistic",
        shape = "Statistic",
        lty = "Statistic"
    ) +
    scale_colour_brewer(type = "qual", palette = "Dark2") +
    theme_tn() +
    theme(legend.direction = "vertical", legend.position = "right") +
    scale_x_discrete()

# Save plot
ggsave(plot_agg,
    filename = "output/figures/pat_by_year.pdf",
    width = 6, height = 2.5, device = cairo_pdf
)