# Replication files for 'The Emergence of Party-Based Political Careers in the UK, 1801-1918'
# Cox & Nowacki (Journal of Politics, forthcoming)
# prepare_data_government.R: Tidy data for government-level analysis

# ---------
# DEPENDENCIES
# ---------
library(tidyverse)
library(rio)
library(lubridate)
library(devtools)
library(stringr)
source_url("https://raw.githubusercontent.com/tobiasnowacki/RTemplates/master/plottheme.R")

source("code/0_functions.R")

# ---------
# LOAD DATA
# ---------

d_orig <- import("data/eggers_spirling/elections.csv")

# List of government intervals
govlist <- read_csv("data/19c_governments.csv") %>% 
  rename(govt_party = party) %>%
  dplyr::select(date_from, date_to, govt_party) %>%
  mutate(span = interval(dmy(date_from), dmy(date_to)))

# List of candidates running
cand_cab <- read.csv("output/mod_data/candidates_stats.csv")

# ----------------
# CLEAN DATA
# ----------------

# Add attempt number
cand_cab <- cand_cab %>%
    group_by(name) %>%
    arrange(year) %>%
    mutate(attempt = row_number())

# Remove candidates with multiple candidacies in one year
mult <- cand_cab %>%
  group_by(name) %>%
  arrange(year) %>%
  summarise(multiple = sum(lead(year) == year, na.rm = TRUE)) %>%
  arrange(-multiple) %>%
  filter(multiple > 0, na.rm = TRUE)

cand_cab <- cand_cab %>% 
  filter(!(name %in% mult$name))

cand_cab <- cand_cab %>%
   # pruned party indicator (either C/L or nothing) 
  mutate(party_pruned = ifelse(party %in% c("C", "L"), party, NA)) %>%
  filter(year <= 1929)

# Flag cands who ever switched (between two major parties)
cand_cab <- cand_cab %>%
  group_by(name) %>%
  mutate(mp_switch = length(unique(party_pruned)) > 1,
         mp_switch_between = length(unique(party_pruned[!is.na(party_pruned)])) > 1,
         ever_elected = sum(winner == 1) > 0)

# Merge with government party
pty <- c("C", "L")
elecs2 <- d_orig %>%
  mutate(date = as.Date(date),
    year = year(date)) %>%
  filter(date > as.Date("1800-01-01") & date < as.Date("1930-01-01")) %>%
  mutate(year = ifelse(date == as.Date("1910-12-03"), 1911, year)) %>%
  filter(by_election == 0)

# Create helper dataframe w/ GE dates & elected gov't
ge_tibble <- unique(elecs2 %>% dplyr::select(year, date)) %>%
  mutate(
      date_end = lead(date),
      inter = interval(date, date_end),
      p_l = "L",
      p_c = "C"
  ) %>%
  pivot_longer(p_l:p_c, values_to = "party") %>%
  dplyr::select(-name) %>%
  rowwise() %>%
  mutate(
      is_govt = check_govt_status(inter, party)
  ) %>%
  dplyr::select(year, party, is_govt) %>%
  filter(year < 1918)

# Join into candidate data
cand_cab_final <- cand_cab %>% 
  left_join(ge_tibble) 

# Export
write_csv(cand_cab_final, "output/mod_data/candidates_cabinet.csv")
