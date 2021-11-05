# Replication files for 'The Emergence of Party-Based Political Careers in the UK, 1801-1918'
# Cox & Nowacki (Journal of Politics, forthcoming)
# prepare_within_career.R: Tidy data for career-level analyses

# Dependencies ----
library(tidyverse)
library(devtools)
library(rio)
library(patchwork)
source_url(
    "https://raw.githubusercontent.com/tobiasnowacki/RTemplates/master/plottheme.R"
)
options(tibble.width = Inf)

# Load data ----

mpdat <- import("output/mod_data/candidates_cabinet.csv") %>%
    filter(
        patronal == 0,
        year < 1918,
        is_ire_val == TRUE,
        !is.na(party_pruned),
        mp_switch_between == FALSE,
        unopposed == 0
    )

# Prepare data ----
# Group by member_id; count terms
mpdat_edit <- mpdat %>%
    group_by(name_shortened) %>%
    arrange(year) %>%
    mutate(
        # Number election attempts
        term = row_number(),
        first_win_term = min(term[winner == 1]),
        # amend party safety
        party_safety = ifelse(
            party_pruned == "L",
            1 - fitted4_ire,
            fitted4_ire
        ),
        const_no = match(
            constituency.name,
            unique(constituency.name)
        ),
        lag_win = lag(winner)
    ) %>%
    # Within career, within-term
    # omits changes relative to redistricting years
    group_by(name_shortened, grp) %>%
    mutate(
        begin_const = constituency.name[1],
        is_same_const = constituency.name == begin_const,
        chg_const = constituency.name != lag(constituency.name),
        chg_const = ifelse(is.na(chg_const), FALSE, chg_const),
        chg_safety = party_safety - lag(party_safety)
    ) %>%
    filter(!is.na(grp), year > 1802) %>%
    group_by(name_shortened) %>%
    mutate(
        # safety change after first/second const change
        chg_after_first = chg_safety[const_no == 2][1],
        chg_after_second = chg_safety[const_no == 3][1]
    )

# Save for future figures + tables
write_csv(
    mpdat_edit,
    "output/mod_data/within_career.csv"
)