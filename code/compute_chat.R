# Replication files for 'The Emergence of Party-Based Political Careers in the UK, 1801-1918'
# Cox & Nowacki (Journal of Politics, forthcoming)
# compute_chat.R: Fit seat safety models


# Dependencies -----------------------------------------------
source("code/0_functions.R")
library(extrafont)

# Load data ------------------------------------------------
mg_df <- import("output/mod_data/seat_shares.csv")

# Compute scores -------------------------------------------

# Prepare data
score_df <- mg_df %>%
    filter(year <= 1930) %>%
    mutate(year_fac = as.factor(year_fac))

# Run regressions
# On all seats
m4_full <- lm(Con ~ Con_lg + unopposed_lg +
    year_fac + year_fac * Con_lg + unopposed_lg * year_fac + unopposed_lg * Con_lg * year_fac,
score_df,
na.action = na.exclude
)

# On non-patronal seats
m4_no_patr <- lm(Con ~ Con_lg + unopposed_lg +
    year_fac + year_fac * Con_lg + unopposed_lg * year_fac + unopposed_lg * Con_lg * year_fac,
score_df %>% filter(patronal == 0),
na.action = na.exclude
)

# On non-patronal seats outside Ireland
m4_no_patr_no_ire <- lm(Con ~ Con_lg + unopposed_lg +
    year_fac + year_fac * Con_lg + unopposed_lg * year_fac + unopposed_lg * Con_lg * year_fac,
score_df %>% filter(patronal == 0, country != "Ireland"),
na.action = na.exclude
)

# Only patronal seats
m4_patr <- lm(Con ~ Con_lg + unopposed_lg +
    year_fac + year_fac * Con_lg + unopposed_lg * year_fac + unopposed_lg * Con_lg * year_fac,
score_df %>% filter(patronal == 1),
na.action = na.exclude
)

# Merge scores with data ---------------------------------------------
# Non-patronal only
fit_vals_0 <- score_df %>%
    filter(patronal == 0) %>%
    ungroup() %>%
    mutate(
        fitted4 = fitted(m4_no_patr),
        fitted4_np = fitted(m4_no_patr),
        fitted4_p = NA,
        grp = case_when(
            year < 1832 ~ "1806 - 1831",
            year > 1832 & year < 1868 ~ "1835 - 1865",
            year >= 1868 & year < 1885 ~ "1868 - 1880",
            year > 1885 ~ "1886 - 1910"
        )
    ) %>%
    dplyr::select(-c(year_fac, V1))

write.csv(fit_vals_0, "output/mod_data/phat_vals_no_patronal.csv")

# Patronal only
fit_vals_1 <- score_df %>%
    filter(patronal == 1) %>%
    ungroup() %>%
    mutate(
        fitted4 = fitted(m4_patr),
        fitted4_np = NA,
        fitted4_p = fitted(m4_patr),
        grp = case_when(
            year < 1832 ~ "1806 - 1831",
            year > 1832 & year < 1868 ~ "1835 - 1865",
            year >= 1868 & year < 1885 ~ "1868 - 1880",
            year > 1885 ~ "1886 - 1910"
        )
    ) %>%
    dplyr::select(-c(year_fac, V1))

# Joint (non-patronal and patronal jointly)
joint_df <- rbind(fit_vals_0, fit_vals_1) %>%
    mutate(
        is_ireland = country == "Ireland",
        is_ire_val = case_when(
            country != "Ireland" & patronal == 0 ~ TRUE,
            TRUE ~ FALSE
        ),
        fitted4_ire = NA,
    )

joint_df$fitted4_ire[joint_df$is_ire_val == TRUE] <- fitted(m4_no_patr_no_ire)

# ------------------------
# Predict Conservative share for
# full Conservative and full Liberal seat in t-1
# ------------------------

# Build prediction dataset
yrs <- unique(score_df$year_fac)[-1] %>% factor()
pred_dat <- tibble(Con_lg = 1, unopposed_lg = 0, year_fac = yrs)
pred_dat2 <- tibble(Con_lg = 0, unopposed_lg = 0, year_fac = yrs)

pred_df <- rbind(pred_dat, pred_dat2)
pred_df$val <- predict(m4_no_patr_no_ire, pred_df)
pred_df <- pred_df %>%
    mutate(
        Con_lg = as.factor(Con_lg),
        year = as.numeric(as.character(year_fac))
    ) %>%
    filter(year < 1918)

# Create plot
plot_pred <- ggplot(pred_df, aes(year, val)) +
    geom_point(
        aes(shape = Con_lg)
    ) +
    geom_line(
        aes(lty = Con_lg, group = Con_lg)
    ) +
    theme_tn() +
    labs(
        x = "Year",
        y = "Predicted Seat Value",
        lty = "Con Share in t-1",
        shape = "Con Share in t-1"
    )
ggsave(plot_pred,
    file = "output/figures/chat_over_time.pdf",
    device = cairo_pdf,
    width = 4, height = 4
)


# Export for further processing ----------------------------------
write.csv(fit_vals_1, "output/mod_data/phat_vals_patronal_only.csv")
write.csv(joint_df, "output/mod_data/phat_vals.csv")