# Replication files for 'The Emergence of Party-Based Political Careers in the UK, 1801-1918'
# Cox & Nowacki (Journal of Politics, forthcoming)
# analysis_candidates_did.R: generating diff-in-diff table

# Dependencies ----------------------------------------------------------
library(tidyverse)
library(rio)
library(viridis)
library(lfe)
library(broom)
library(devtools)
library(modelsummary)
library(kableExtra)
source_url("https://raw.githubusercontent.com/tobiasnowacki/RTemplates/master/plottheme.R")
source("code/0_functions.R")

# Load data --------------------------------------------

df <- import("output/mod_data/candidates_cabinet.csv")

attempts <- df %>%
  filter(year <= 1911)

# Only get the first time a candidate is running
firstc <- attempts %>%
  filter(
    patronal == 0,
    is_ire_val == TRUE,
    mp_switch_between == FALSE
  ) %>%
  filter(attempt == 1) %>%
  filter(party %in% c("C", "L")) %>%
  ungroup()

# DiD Analysis -------------------------------------------

# Function to extract tidy model names
tidy_names <- function(x, ncol, rows, add_bottom) {
  idx <- stringr::str_detect(x$statistic, "statistic\\d*$")
  x <- x %>%
    dplyr::mutate(term = ifelse(idx, "", term))
  idx_row <- match("gof", x$group)
  x <- x %>%
    dplyr::select(-statistic, -group) %>%
    dplyr::rename(`       ` = term) # HACK: arbitrary 7 spaces to avoid name conflict
  idx_col <- ncol(x)
  names(x)[2:idx_col] <- names(x)[2:idx_col] %>%
    str_replace_all("Model ", "(")
  # paste0(., ")")
  if (!is.null(add_bottom)) {
    names(add_bottom) <- names(x)
    x <- rbind(x, add_bottom)
  }
  # Get these
  names(x)[2:ncol] <- map_chr(names(x)[2:ncol], ~ paste0("\\multicolumn{1}{c}{", .x, "}"))
  x[rows, 2:ncol] <- x[rows, 2:ncol] %>% map_df(~ paste0("\\multicolumn{1}{c}{", .x, "}"))
  return(list(x, idx_row))
}


# Prepare data
ddframe <- firstc %>%
  filter(year %in% c(1826:1831, 1835:1841)) %>%
  mutate(treat = year > 1832)

ddframe2 <- firstc %>%
  filter(year %in% c(1857:1865, 1868:1880)) %>%
  mutate(treat = year > 1867)

ddframe3 <- firstc %>%
  filter(year %in% c(1868:1880, 1886:1895)) %>%
  mutate(treat = year > 1885)

# Fit models
did_mod2 <- felm(fitted4_ire ~ party + party:treat + treat | constituency.name + year | 0 | constituency.name,
  data = ddframe
)
did_mod2 %>% tidy()

did_mod3 <- felm(fitted4_ire ~ party + party:treat + treat | constituency.name + year | 0 | constituency.name,
  data = ddframe2
)
did_mod3 %>% tidy()

did_mod4 <- felm(fitted4_ire ~ party + party:treat + treat | constituency.name + year | 0 | constituency.name,
  data = ddframe3
)
did_mod4 %>% tidy()

# Extract and save
f1 <- c("Unit FEs", rep("Y", 3))
f2 <- c("Year FEs", rep("Y", 3))
cmap <- c(
  "partyL" = "Liberal",
  "partyL:treatTRUE" = "Liberal * post-Reform"
)

mods <- list(did_mod2, did_mod3, did_mod4)
names(mods) <- c("1832 Reform Act", "1868 Reform Act", "1885 Reform Act")

rows <- tribble(
  ~term, ~`1832 Reform Act`, ~`1868 Reform Act`, ~`1885 Reform Act`,
  "Constituency FEs", "Y", "Y", "Y",
  "Year FEs", "Y", "Y", "Y"
)
attr(rows, "position") <- c(5, 6)

# Export models as table
did_out <- modelsummary(
  mods,
  coef_map = cmap,
  add_rows = rows,
  gof_omit = "R2 ",
  output = "latex",
  caption = "\\textbf{Two-way Fixed Effects Model Estimates.} Estimates show the effect of being a Liberal candidate on the seat value compared to a Conservative candidate. \\label{tab:newcand_did}",
  booktabs = TRUE,
  linesep = c("", "\\addlinespace", rep("", 8)),
  # align = c("l", rep("c", 3)),
  digits = 1,
  escape = FALSE
)


did_out2 <- did_out %>%
  footnote(
    threeparttable = TRUE,
    general_title = "",
    fixed_small_size = FALSE,
    footnote_as_chunk = TRUE,
    general = c("All estimates reported with robust standard errors clustered at the constituency level in parentheses. Each observation is a first-time Conservative or Liberal candidate running in a non-patronal constituency in England, Scotland or Wales in one of the three elections around the respective Reform Act (with 1832 and 1885 excluded due to redistricting and data changes). Liberal is a binary variable set to 1 if the candidate ran for the Liberal party. Post-reform is a binary variable set to 1 if the candidacy occured in the three elections after the reform act.")
  ) %>%
  row_spec(4, extra_latex_after = "\\midrule")

# Export
cat(did_out2, file = "output/tables/models1.tex")