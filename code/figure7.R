# Replication files for 'The Emergence of Party-Based Political Careers in the UK, 1801-1918'
# Cox & Nowacki (Journal of Politics, forthcoming)
# figure7.R: Analysing candidate progression


# Dependencies ------------------------------
library(tidyverse)
library(devtools)
library(rio)
library(patchwork)
source_url(
    "https://raw.githubusercontent.com/tobiasnowacki/RTemplates/master/plottheme.R"
)
options(tibble.width = Inf)

# Load data -------------------------------
mpdat_edit <- read_csv(
    "output/mod_data/within_career.csv"
)

##
## PANEL A -- AVG SEAT SAFETY BY ELECTION ATTEMPT
##

# Group by era and election attempt
mpdat_byera <- mpdat_edit %>%
    group_by(grp, term) %>%
    summarise(
        avg_sfty = mean(party_safety, na.rm = TRUE),
        delta_sfty = mean(chg_safety, na.rm = TRUE),
        avg_same = mean(is_same_const, na.rm = TRUE),
        avg_chg = mean(chg_const, na.rm = TRUE)
    ) %>%
    filter(term < 7) # tail too noisy

# Create plot
p1 <- ggplot(mpdat_byera, aes(x = term, y = avg_sfty)) +
    geom_line(aes(color = grp, size = grp)) +
    geom_point(aes(color = grp), size = 2) +
    theme_tn() +
    scale_color_manual(
        values = c("grey75", "grey50", "grey25", "#000000")
    ) +
    scale_size_manual(values = c(0.8, 1, 1.2, 1.5)) +
    labs(
        x = "Attempt Number",
        y = "Average Seat Safety",
        colour = "Era",
        size = "Era",
        title = "Avg Seat Safety"
    ) +
    theme(legend.direction = "vertical", legend.position = c(0.85, 0.3))

##
## PANEL B -- SHARE OF FIRST-TIMERS
##

# Classify first-time MPs by time of election
type_class <- mpdat_edit %>%
    filter(term == first_win_term) %>%
    mutate(type = case_when(
        term == 1 ~ "Outright",
        term > 1 & const_no == 1 ~ "1st Const, 2+ attempts",
        term > 1 & const_no == 2 ~ "2nd Const, 2+ attempts",
        TRUE ~ "Other"
    ))

# Group first-time types by election year
type_by_year <- type_class %>%
    group_by(year, type, grp) %>%
    summarise(
        ncount = n()
    ) %>%
    group_by(year, grp) %>%
    mutate(prop = ncount / sum(ncount)) %>%
    filter(
        type != "Other",
        type == "2nd Const, 2+ attempts",
        year > 1802
    )

# Plot panel
p2 <- ggplot(type_by_year, aes(x = year, y = prop)) +
    geom_line(aes(
        group = grp
    )) +
    geom_point(aes()) +
    geom_vline(xintercept = c(1832, 1867, 1885), lty = "dotted") +
    labs(
        x = "Year",
        y = "Share",
        title = "First Elected In 2nd Const."
    ) +
    theme_tn() +
    theme(legend.direction = "horizontal")

##
## PANEL C -- BOXPLOT
##

# Restrict attention to those winning in second constituency
second_winners <- type_class %>%
    filter(type == "2nd Const, 2+ attempts")

# Plot safety change when changing constituency
p3 <- ggplot(second_winners, aes(x = grp, y = chg_after_first)) +
    geom_boxplot(width = 0.5) +
    geom_hline(yintercept = 0.0, lty = "dotted") +
    labs(
        x = "Era",
        y = "Δ Safety After Constituency Change",
        title = "Switching To Safer Seats"
    ) +
    theme_tn()

##
## PATCH PANELS TOGETHER AND EXPORT
##

# Patch plots together
combine_plots <- p1 + p2 + p3 +
    plot_annotation(tag_levels = "A")

ggsave(combine_plots,
    filename = "output/figures/combined_within_career.pdf",
    width = 12,
    height = 5,
    device = cairo_pdf
)