# Replication files for 'The Emergence of Party-Based Political Careers in the UK, 1801-1918'
# Cox & Nowacki (Journal of Politics, forthcoming)
# table_within_career.R: Script to produce FE tables

# Load packages ----
library(tidyverse)
library(devtools)
library(rio)
library(fixest)
library(modelsummary)
library(kableExtra)
source_url(
    "https://raw.githubusercontent.com/tobiasnowacki/RTemplates/master/plottheme.R"
)
options(tibble.width = Inf)

# Load data ----
mpdat_edit <- read_csv(
    "output/mod_data/within_career.csv"
)

# Prepare table rows ----
gofs <- tribble(
    ~raw, ~clean, ~fmt,
    "nobs", "N", 0,
    "r.squared", "R2", 2,
    "FE: name_shortened", "Cand FE", 0,
    "FE: year", "Year FE", 0
)

candidates <- length(unique(mpdat_edit$name_shortened)) %>%
    round() %>%
    as.character()
years <- length(unique(mpdat_edit$year)) %>%
    round() %>%
    as.character()

xrows <- tribble(
    ~term, ~`Model 1`, ~`Model 2`, ~`Model 3`,
    "Candidates", candidates, candidates, candidates,
    "Years", years, years, years
)

# Run models -----
mod1 <- feols(
    winner ~ (attempt + const_no),
    data = mpdat_edit,
    cluster = "name_shortened"
)

mod2 <- feols(
    winner ~ (attempt + const_no) |
        year + name_shortened,
    data = mpdat_edit,
    cluster = "name_shortened"
)

mod3 <- feols(
    winner ~ (attempt + const_no):grp |
        year + name_shortened,
    data = mpdat_edit,
    cluster = "name_shortened"
)

mlist <- list(mod1, mod2, mod3)

# Coefficient map
cmap <- c(
    attempt = "Attempt",
    const_no = "Constituency No.",
    `attempt:grp1806 - 1831` = "Attempt (1806-1831)",
    `attempt:grp1835 - 1865` = "Attempt (1835-1865)",
    `attempt:grp1868 - 1880` = "Attempt (1868-1880)",
    `attempt:grp1886 - 1910` = "Attempt (1886-1910)",
    `const_no:grp1806 - 1831` = "Const No. (1806-1831)",
    `const_no:grp1835 - 1865` = "Const No. (1835-1865)",
    `const_no:grp1868 - 1880` = "Const No. (1868-1880)",
    `const_no:grp1886 - 1910` = "Const No. (1886-1910)"
)

# Put together into table and export --------
did_out <- modelsummary(mlist,
    gof_map = gofs,
    output = "latex",
    coef_map = cmap,
    booktabs = TRUE,
    caption = "\\textbf{Candidates who switched constituencies became more likely to win}",
    booktabs = TRUE,
    stars = FALSE,
    add_rows = xrows,
    escape = FALSE
) %>%
    footnote(
        threeparttable = TRUE,
        general_title = "",
        fixed_small_size = TRUE,
        footnote_as_chunk = TRUE,
        general = c("Robust standard errors clustered by candidate in parentheses. Each observation is a Conservative or Liberal candidate running in a non-patronal constituency in England, Scotland, or Wales between 1802 and 1911.")
    )

cat(did_out, file = "output/tables/within_model.tex")