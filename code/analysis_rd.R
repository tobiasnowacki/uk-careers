# Replication files for 'The Emergence of Party-Based Political Careers in the UK, 1801-1918'
# Cox & Nowacki (Journal of Politics, forthcoming)
# analysis_rd.R: Perform all RD-related analyses

# Dependencies --------------------------------------------------
pkgs <- c(
  "tidyverse",
  "rio",
  "lubridate",
  "devtools",
  "rdrobust"
)
lapply(pkgs, library, character.only = TRUE, quietly = TRUE)
library(kableExtra)
source_url("https://raw.githubusercontent.com/tobiasnowacki/RTemplates/master/plottheme.R")


# Load data ----------------------------------------------
returns <- import("output/mod_data/candidates_cabinet.csv")
ge_dates <- unique(returns$year)

# Estimation functions -------------------------------------

#' Function to estimate RD
#'
#' For a given interval of years, select the appropriate sample
#' and run the RD.
estimate_rd <- function(interval, data = rdd_df, poly = 1, plot_ready = TRUE) {
  data <- data %>%
    filter(year >= interval[1] & year <= interval[2]) %>%
    ungroup()
  out_cand <- rdrobust(y = data$again, x = data$running, p = poly)
  out_win <- rdrobust(y = data$win_fut, x = data$running, p = poly)
  # out_share <- rdrobust(y = data$vote_fut, x = data$running)
  # out_fitted <- rdrobust(y = data$fitted4, x = data$fitted4)
  out_unopp <- rdrobust(y = data$unopp_fut, x = data$running, p = poly)
  results_list <- list(
    out_cand, out_win,
    # out_share, out_fitted,
    out_unopp
  )
  names(results_list) <- c(
    "Running in t+1", "Winning in t+1",
    # "Share", "Chat",
    "Unopposed"
  )
  if (plot_ready != TRUE) {
    return(results_list)
  }
  outdf <- map_dfr(results_list, ~ tibble(
    estimate = .x$coef[1, 1],
    stderr = .x$se[3, 1],
    ci_lower = .x$ci[3, 1],
    ci_upper = .x$ci[3, 2],
    obs = .x$N[1] + .x$N[2],
    bwl = .x$bws[1, 1],
    bwr = .x$bws[1, 2]
  ),
  .id = "Outcome"
  )
  return(outdf)
}

# Function to get automated election intervals
create_intervals <- function(vec, inter) {
  interval_list <- list()
  for (j in 1:length(vec)) {
    upper_bound <- ifelse(j + inter > length(vec) - 1,
      valid <- 0, valid <- 1
    )
    if (valid == 1) {
      name_vec <- paste0(vec[j], " - ", vec[j + inter])
      interval_list[[name_vec]] <- c(vec[j], vec[j + inter]) %>% as.numeric()
    }
  }
  return(interval_list)
}

# Prepare data ----------------------------------------------

rdd_df <- returns %>%
  filter(party %in% c("L", "C")) %>%
  # make winning unconditional
  mutate(win_fut = ifelse(is.na(win_fut), 0, win_fut)) %>%
  filter(unopposed == 0 | is.na(unopposed)) %>%
  filter(
    patronal == 0,
    is_ire_val == TRUE,
    mp_switch_between == FALSE
  ) %>%
  group_by(year, election_id) %>%
  mutate(
    votes_pct = votes / sum(votes),
    thres = max(votes_pct[winner == 0]) +
      (abs(max(votes_pct[winner == 0] - min(votes_pct[winner == 1])) / 2)),
    running = votes_pct - thres
  ) %>%
  # Drop obs with no competition / not enough candidates to calc margin
  filter(running != -Inf & running != Inf) %>%
  group_by(name) %>%
  ungroup()

# Define intervals
unique_years <- substr(ge_dates, 1, 4) %>% as.numeric()
split <- which(unique_years == "1880")
unique_years_old <- unique_years[1:(split - 1)]
unique_years_new <- unique_years[split:length(unique_years)]

unique1 <- unique_years[unique_years < 1831]
unique2 <- unique_years[unique_years > 1831 & unique_years < 1880]
unique3 <- unique_years[unique_years >= 1885]

inter_1 <- create_intervals(unique1, 5)
inter_2 <- create_intervals(unique2, 5)
inter_3 <- create_intervals(unique3, 5)

inter <- c(inter_1, inter_2, inter_3)



# Fit RD models -------------------------------------------------
results <- map_dfr(inter[1:13], ~ estimate_rd(.x), .id = "interval") %>%
  filter(Outcome %in% c("Running in t+1", "Winning in t+1"))

results_second <- map_dfr(inter[1:13], ~ estimate_rd(.x, poly = 2), .id = "interval") %>%
  filter(Outcome %in% c("Running in t+1", "Winning in t+1"))

# Report and export results -----------------------------

# Main RD plot
plot_rd <- ggplot(results, aes(interval, estimate)) +
  geom_point(aes(colour = Outcome),
    size = 3,
    position = position_dodge(1)
  ) +
  geom_errorbar(aes(
    ymin = ci_lower,
    ymax = ci_upper,
    colour = Outcome
  ),
  width = 0.2,
  position = position_dodge(1)
  ) +
  geom_vline(xintercept = c(2.5, 7.5), lty = "dotted") +
  geom_hline(yintercept = 0, colour = "grey70") +
  scale_colour_viridis_d(end = 0.7) +
  scale_y_continuous(breaks = seq(-1, 0.5, by = 0.25)) +
  theme_tn() +
  facet_wrap(. ~ Outcome) +
  labs(x = "Year Interval", y = "Coefficient") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ggsave(plot_rd,
  file = "output/figures/rd_updated_ppr.pdf",
  width = 7,
  height = 4,
  device = cairo_pdf
)

# Plot with second-order polynomials (Figure 17)
plot_rd2 <- ggplot(results_second, aes(interval, estimate)) +
  geom_point(aes(colour = Outcome),
    size = 3,
    position = position_dodge(1)
  ) +
  geom_errorbar(aes(
    ymin = ci_lower,
    ymax = ci_upper,
    colour = Outcome
  ),
  width = 0.2,
  position = position_dodge(1)
  ) +
  geom_vline(xintercept = c(2.5, 7.5), lty = "dotted") +
  geom_hline(yintercept = 0, colour = "grey70") +
  scale_colour_viridis_d(end = 0.7) +
  scale_y_continuous(breaks = seq(-1, 0.5, by = 0.25)) +
  theme_tn() +
  facet_wrap(. ~ Outcome) +
  labs(x = "Year Interval", y = "Coefficient") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ggsave(plot_rd2,
  file = "output/figures/rd_updated_ppr_sq.pdf",
  width = 7,
  height = 4,
  device = cairo_pdf
)

# Report results in table (Table 4)
rd_tab <- results %>%
  select(Outcome, interval, estimate, stderr, obs, bwl) %>%
  rename(
    Interval = interval,
    Estimate = estimate,
    SE = stderr,
    N = obs,
    BW = bwl
  ) %>%
  arrange(Outcome)

rd_tab_tex <- kable(rd_tab,
  "latex",
  caption = "Regression Discontinuity Estimates. \\label{tab:rd_est}",
  booktabs = TRUE,
  linesep = "",
  digits = 2
) %>%
  footnote(
    threeparttable = TRUE,
    general_title = "",
    fixed_small_size = FALSE,
    footnote_as_chunk = TRUE,
    general = c("All estimates are with robust standard errors. The estimates report the effect of barely winning the election at the threshold on the reported outcome.")
  ) %>%
  row_spec(13, extra_latex_after = "\\midrule")
# row_spec(8, extra_latex_after = '\\midrule')
# str_replace_all(fixed("\\textbackslash{}"), "\\") %>%
# str_replace_all(fixed("\\$"), "$") %>%
cat(rd_tab_tex, file = "output/tables/models_rd.tex")

# Fit RD by year models ----------------------------------

unique_list <- map(unique_years, ~ rep(.x, times = 2))

# Catch years where estimation not possible...
rd_list <- list()
for (i in 5:length(unique_list)) {
  print(i)
  rd_list[[i - 4]] <- tryCatch(
    estimate_rd(unique_list[[i]]) %>%
      mutate(year = unique_list[[i]][1]) %>%
      filter(Outcome %in% c("Running in t+1", "Winning in t+1")),
    error = function(x) NA
  )
}

rd_list <- rd_list[!is.na(rd_list)]
rd_df <- do.call(rbind, rd_list) %>%
  filter(!year %in% c(1831, 1865, 1880, 1924, 1929))

plot_rdyr <- ggplot(rd_df, aes(year, estimate)) +
  geom_hline(yintercept = 0, lty = "dotted") +
  geom_point(aes(colour = Outcome)) +
  geom_vline(xintercept = c(1831.5, 1867, 1884), lty = "dashed") +
  geom_errorbar(aes(
    ymin = ci_lower,
    ymax = ci_upper,
    colour = Outcome
  ),
  alpha = 0.4
  ) +
  geom_smooth(
    method = "loess",
    se = FALSE,
    aes(colour = Outcome)
  ) +
  facet_wrap(~Outcome) +
  labs(x = "Year", y = "Estimate") +
  theme_tn()
ggsave(plot_rdyr,
  file = "output/figures/rd_by_year.pdf",
  device = cairo_pdf, width = 8, height = 3
)

# Plot
rd_by_year_tbl <- rd_df %>%
  arrange(Outcome, year) %>%
  select(Outcome, year, estimate, stderr) %>%
  pivot_wider(names_from = Outcome, values_from = estimate:stderr)
rd_by_year_tbl <- rd_by_year_tbl[, c(1, 2, 4, 3, 5)]
names(rd_by_year_tbl) <- c("Year", "Estimate", "SE", "Estimate", "SE")

# Table
rd_mod <- kable(rd_by_year_tbl,
  booktabs = TRUE,
  digits = 2,
  caption = "\\textbf{RD Estimates Fitted On Each Election Year Separately}. With Robust Standard Errors.",
  "latex"
) %>%
  add_header_above(c(" " = 1, "Running Again" = 2, "Winning" = 2)) %>%
  kable_styling(latex_options = "hold_position")
cat(rd_mod, file = "output/tables/rd_by_year.tex")

# Density check -----------------------

# Density plot by year (check sorting)
rd_density <- ggplot(rdd_df %>% filter(abs(running) < 0.05, year < 1924)) +
  geom_density(aes(x = running)) +
  facet_wrap(. ~ year) +
  geom_vline(xintercept = 0) +
  theme_tn() +
  scale_x_continuous(breaks = c(-0.05, 0, 0.05)) +
  labs(x = "Vote Share Margin", y = "Density")
ggsave(rd_density,
  file = "output/figures/rd_sorting.pdf",
  device = cairo_pdf, width = 8, height = 6
)