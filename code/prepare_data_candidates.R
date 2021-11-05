# Replication files for 'The Emergence of Party-Based Political Careers in the UK, 1801-1918'
# Cox & Nowacki (Journal of Politics, forthcoming)
# prepare_data_candidates.R: Tidy data for candidate-level analysis


# ------------
# DEPENDENCIES
# ------------

library(tidyverse)
library(rio)
library(lubridate)
library(lfe)
library(devtools)
library(stringr)
source_url("https://raw.githubusercontent.com/tobiasnowacki/RTemplates/master/plottheme.R")

source("code/0_functions.R")

# ---------
# LOAD DATA
# ---------

d_orig <- import("data/eggers_spirling/elections.csv")
f <- import("data/eggers_spirling/election_returns.csv")
m <- import("data/eggers_spirling/mps.csv")
s <- import("data/eggers_spirling/services.csv")
of <- import("data/eggers_spirling/officeholdings.csv")
ofid <- import("data/eggers_spirling/offices.csv")
af <- import("data/aidt-franck/pre-reform.dta")
hss <- import("data/hss/hopt-v3.dta")
cdum <- import("data/eggers_spirling/constituencies.dta")
fit_vals_0 <- import("output/mod_data/phat_vals.csv") %>%
    select(-V1)

# ----------
# CLEAN DATA
# ----------

# Define party labels (same as in previous file)
con_labels <- c("C", "C (Ind C)", "C (Nat P)", "C (Nat P) (C)", "C*", "Ind L/Crf (LU)", "LU", "LU (C)", "LU (Nat P) (C)", "LU*", "U", "Co C")
lib_labels <- c("L", "Ind L (L)", "LU (L)", "Co L")

# Prepare pre-1832 data for merging (HSS)
hss_cand <- hss %>%
    filter(bye == 0) %>%
    mutate(
        constituency.name = constituency,
        name_on_return = candidate,
        party = case_when(
            party == "T" ~ "C",
            party == "W" ~ "L"
        ),
        member_id = NA
    ) %>%
    left_join(fit_vals_0, by = c("constituency" = "constituency.name", "year")) %>%
    dplyr::select(year, constituency.name, name_on_return, votes, party, winner, election_id, Con, Lib, Con_lg, Con_lg2, unopposed, unopposed_lg2, const_type, country, patronal, member_id)

# Prepare post-1832 data for merging (E-S)
f_merge <- f %>%
    mutate(party = case_when(
        party %in% con_labels ~ "C",
        party %in% lib_labels ~ "L",
        TRUE ~ "O"
    )) %>%
    left_join(fit_vals_0) %>%
    filter(!is.na(date), !is.na(constituency.id)) %>%
    dplyr::select(year, constituency.name, name_on_return, votes, party, winner, election_id, Con, Lib, Con_lg, Con_lg2, unopposed, unopposed_lg2, const_type, country, patronal, member_id)

# Slice into pre and post
pre_cands <- f_merge %>%
    filter(as.numeric(year) < 1832)
candidates <- f_merge %>%
    filter(as.numeric(year) >= 1832)

# Check which constituencies are inconsistent
hss_const <- unique(hss_cand$constituency.name) %>%
    as_tibble() %>%
    mutate(is_match = value %in% pre_cands$constituency.name)

# Merge pre-1832 and post-1832 data
# Clean names and add chat values
cand_mg <- rbind(hss_cand, candidates) %>%
    mutate(name = name_on_return %>% clean_name()) %>%
    left_join(fit_vals_0) %>%
    mutate(
        name_shortened = shorten_name(name),
        name = ifelse(as.numeric(year) >= 1885, name_shortened, name),
        year = as.numeric(year)
    )

### -------------
### COALESCING MEMBER IDS
### -------------

# Are there any names that are duplicates across different member IDs?
# Get unique list of HSS names
hss_names <- hss %>%
    filter(winner == 1) %>%
    dplyr::select(candidate) %>%
    rename(name_on_return = candidate) %>%
    unique() %>%
    unlist() %>%
    clean_name() %>%
    shorten_name()

# Get names of pre-1832 winners in E-S data and check if they turn up in HSS
id_merge <- pre_cands %>%
    filter(winner == 1) %>%
    dplyr::select(year, name_on_return, member_id, constituency.name) %>%
    unique() %>%
    mutate(
        name = clean_name(name_on_return) %>% shorten_name(),
        is_match = name %in% hss_names
    ) %>%
    filter(is_match == TRUE) %>%
    rename(
        member_id2 = member_id,
        const_alt = constituency.name
    ) %>%
    dplyr::select(name, member_id2, year, const_alt)

# Check if there are any duplicate instances
id_merge %>%
    select(year, member_id2, name) %>%
    unique() %>%
    group_by(name, year) %>%
    summarise(ids = length(member_id2 %>% unique())) %>%
    arrange(-ids)
# # Manual check suggests that they all run in different constituencies.

# Merge memberIDs onto pre-1832 data
cand_mg <- cand_mg %>%
    left_join(id_merge, by = c("name_shortened" = "name", "year", "constituency.name" = "const_alt")) %>%
    mutate(member_id = coalesce(member_id, member_id2))

# ------------------------------
# CHECK CANDIDACIES ACROSS YEARS
# ------------------------------

# Identify election years
ge_dates <- unique(cand_mg$year)

# Prepare election returns data
returns <- cand_mg %>%
    group_by(election_id, year) %>%
    mutate(share = votes / sum(votes)) %>%
    ungroup()

# Unify names for the same officeholder
# We know it's the same person thanks to the member ID
returns <- returns %>%
    group_by(member_id) %>%
    mutate(name = case_when(
        !is.na(member_id) ~ name[1],
        TRUE ~ name
    ))

# Prepare variables of interest
returns$again <- NA
returns$win_fut <- NA
returns$vote_fut <- NA
returns$unopp_fut <- NA
returns$phat <- NA
returns$again1 <- NA
returns$win_fut1 <- NA
returns$vote_fut1 <- NA
returns$party_fut <- NA

# Loop over elections to fill out
for (i in 1:length(ge_dates)) {
    elecdate <- ge_dates[i]
    nextelecdate <- ge_dates[i + 1]
    yeardf <- returns %>% filter(year == elecdate)
    nextdf <- returns %>% filter(year == nextelecdate)

    # check whether turns up in next election again
    logvec <- yeardf$name %in% nextdf$name
    returns$again[returns$year == elecdate] <- logvec
    candidates_again <- yeardf$name[logvec]
    returns$win_fut[returns$year == elecdate][logvec] <- map_int(
        candidates_again,
        ~ sum(nextdf$winner[nextdf$name == .x], na.rm = TRUE) > 0
    ) %>% unlist()
    returns$party_fut[returns$year == elecdate][logvec] <- map_chr(
        candidates_again,
        ~ nextdf$party[nextdf$name == .x][1]
    )
    returns$vote_fut[returns$year == elecdate][logvec] <- map_dbl(
        candidates_again,
        ~ mean(nextdf$share[nextdf$name == .x], na.rm = TRUE)
    ) %>% unlist()
    returns$unopp_fut[returns$year == elecdate][logvec] <- map_dbl(
        candidates_again,
        ~ sum(nextdf$unopposed[nextdf$name == .x], na.rm = TRUE) > 0
    ) %>% unlist()
    returns$phat[returns$year == elecdate][logvec] <- map_dbl(
        candidates_again,
        ~ mean(nextdf$fitted4[nextdf$name == .x], na.rm = TRUE)
    ) %>% unlist()
    logvec1 <- yeardf$name_on_return %in% nextdf$name_on_return
    returns$again1[returns$year == elecdate] <- logvec
    candidates_again1 <- yeardf$name_on_return[logvec1]
    returns$win_fut1[returns$year == elecdate][logvec1] <- map_int(
        candidates_again1,
        ~ sum(nextdf$winner[nextdf$name_on_return == .x], na.rm = TRUE) > 0
    ) %>% unlist()
    returns$vote_fut1[returns$year == elecdate][logvec1] <- map_dbl(
        candidates_again1,
        ~ mean(nextdf$share[nextdf$name_on_return == .x], na.rm = TRUE)
    ) %>% unlist()
}

# winning unconditionally
returns <- returns %>%
    mutate(win_fut_uncond = ifelse(is.na(win_fut), 0, win_fut))

# check if ever switched party
returns <- returns %>%
    group_by(name) %>%
    mutate(switched_party = length(unique(party)) > 1)

# Select variables for export
returns <- returns %>%
    dplyr::select(year, constituency.name, party, winner, election_id, votes, Con, Con_lg, unopposed, unopposed_lg, const_type, country, patronal, member_id, name, name_shortened, date, fitted4, fitted4_np, fitted4_p, fitted4_ire, is_ire_val, grp, switched_party, again, win_fut, win_fut_uncond, vote_fut, unopp_fut, again1, win_fut1, vote_fut1, party_fut)

write_csv(returns, "output/mod_data/candidates_stats.csv")