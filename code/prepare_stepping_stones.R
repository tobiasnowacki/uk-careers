# Replication files for 'The Emergence of Party-Based Political Careers in the UK, 1801-1918'
# Cox & Nowacki (Journal of Politics, forthcoming)
# prepare_stepping_stones.R: Tidy data for stepping stone analyses

## ------------------------
## DEPENDENCIES
## ------------------------

pkgs <- c(
  "tidyverse",
  "rio",
  "lubridate",
  "devtools",
  "rdrobust"
)
lapply(pkgs, library, character.only = TRUE, quietly = TRUE)
source_url("https://raw.githubusercontent.com/tobiasnowacki/RTemplates/master/plottheme.R")

source("code/0_functions.R")


## ------------------------
## LOAD DATA
## ------------------------

# candidate data
cand_cab <- read.csv("output/mod_data/candidates_cabinet.csv")
# office data
cab <- import("output/mod_data/office_by_year_member.csv")
cab_term <- import("output/mod_data/office_by_term_member.csv")
ofid <- import("data/eggers_spirling/offices.csv")

## ------------------------
## PREPARE AND MERGE
## ------------------------

# Prepare cand_cab DF for merger w/ offices
mp_df <- cand_cab %>%
  filter(winner == 1, is_ire_val == TRUE) %>%
  filter(year > 1832, year < 1929) %>%
  filter(party %in% c("C", "L"), mp_switch_between == FALSE)

# Transform into MP by year dataset
mp_by_year_list <- list()
for (i in 1:(length(unique(mp_df$year)) - 1)) {
  yr <- unique(mp_df$year)[i]
  yr_nxt <- unique(mp_df$year)[i + 1]
  for (j in yr:(yr_nxt - 1)) {
    mp_by_year_list[[paste0(j)]] <- mp_df %>%
      filter(year == yr) %>%
      mutate(year_annual = j)
  }
}
mp_by_year_df <- do.call(rbind, mp_by_year_list)

# Transform NAs into 0 (no offices)
merged_df <- mp_by_year_df %>%
  left_join(cab, by = c("member_id", "year_annual" = "year")) %>%
  mutate(across(office_id_1:in_cabinet, ~ ifelse(is.na(.x), 0, .x)))

# Indicator whether any office for given MP-year
is_office <- merged_df %>%
  dplyr::select(starts_with("office_id_")) %>%
  rowSums()

# Compute office / cabinet statistics by member_id and year
## ever_cab etc. are only for those stats where MP served in cabinet at the same time as being an MP
mg <- merged_df %>%
  ungroup() %>%
  mutate(
    office = is_office > 0
  ) %>%
  group_by(member_id) %>%
  arrange(member_id, year_annual) %>%
  mutate(
    ever_cabinet = sum(in_cabinet, na.rm = TRUE) > 0,
    earliest_cabinet = min(year_annual[in_cabinet == TRUE], na.rm = TRUE),
    earliest_cabinet = ifelse(earliest_cabinet == Inf, 9999, earliest_cabinet),
    already_cabinet = year_annual >= earliest_cabinet,
    fut_cabinet = lead(in_cabinet, default = 0),
    ever_office = sum(office, na.rm = TRUE) > 0,
    earliest_office = min(year_annual[office == TRUE], na.rm = TRUE),
    earliest_office = ifelse(earliest_office == Inf, 9999, earliest_office),
    already_office = year_annual >= earliest_office,
    fut_office = lead(office, default = 0)
  ) %>%
  group_by(member_id) %>%
  mutate(
    min_year = min(year_annual),
    years_served = year_annual - min_year,
    years_binned = case_when(
      years_served %in% 1:5 ~ "1-5",
      years_served %in% 6:10 ~ "6-10",
      years_served %in% 11:15 ~ "11-15",
      years_served > 15 ~ "beyond15"
    )
  )

## ------------------------
## IDENTIFY STEPPING STONES
## ------------------------

# Transform DF to long
stepping_stones <- mg %>%
  filter(is_govt == TRUE & already_cabinet == FALSE) %>%
  group_by(ever_cabinet) %>%
  summarise(across(starts_with("office_id_"), ~ mean(.x, na.rm = TRUE))) %>%
  pivot_longer(starts_with("office_id_"),
    names_prefix = "office_id_",
    names_to = "office_id",
    names_transform = list(office_id = as.numeric)
  ) %>%
  left_join(ofid) %>%
  arrange(-value)

stepping_stones_diff <- stepping_stones %>%
  group_by(office_id, name) %>%
  summarise(diff = value[ever_cabinet == TRUE] -
    value[ever_cabinet == FALSE]) %>%
  filter(diff != 0)

# Save for further analysis
write.csv(stepping_stones_diff,
  file = "output/mod_data/stepping_stones_diff.csv"
)
## ------------------------
## STEPPING STONES OVER TIME
## ------------------------

# Identify offices that were high in diff
keepid <- stepping_stones_diff$office_id[stepping_stones_diff$diff > 0.015]
keep_strings <- paste0("office_id_", keepid)
is_step <- rowSums(mg[keep_strings]) > 0

mg_steps <- mg %>%
  ungroup() %>%
  mutate(is_step = is_step) %>%
  group_by(member_id) %>%
  mutate(ever_step = sum(is_step == TRUE, na.rm = TRUE) > 0)

# Prep and save
mg_out <- mg_steps %>%
  dplyr::select(member_id, year, patronal, year_annual, name, party, is_step, ever_step, ever_cabinet, already_cabinet, in_cabinet, is_govt, fut_cabinet) %>%
  group_by(member_id) %>%
  mutate(
    earliest_step = min(year[is_step == 1], na.rm = TRUE),
    already_step = year >= earliest_step,
    already_step = ifelse(is.na(already_step), 0, already_step)
  )

write.csv(mg_out, "output/mod_data/stepping_offices.csv")

## ------------------------
## TERM-BY-MP UNITS
## ------------------------

# Also create DF with observations term-by-MP
mg_term <- mp_df %>%
  filter(year > 1832) %>%
  left_join(cab_term, by = c("year" = "year_elec", "member_id"))

# Mutate to update ever_cabinet etc
# Note that this only becomes relevant iff we agree that those in Cabinet later but not serving in Parliament at the same time should not be marked as "ever Cabinet" [and it might make sense to adjust for the opposite otherwise]
mg_term <- mg_term %>%
  group_by(member_id) %>%
  mutate(
    ever_cabinet = sum(in_cabinet, na.rm = TRUE) > 0,
    earliest_cabinet = min(year[in_cabinet == TRUE], na.rm = TRUE),
    earliest_cabinet = ifelse(earliest_cabinet == Inf, 9999, earliest_cabinet),
    already_cabinet = year >= earliest_cabinet,
    fut_cabinet = lead(in_cabinet, default = 0)
  )

# Indicate whether they hold stepping stone office at the time
is_step_term <- rowSums(mg_term[keep_strings]) > 0

mg_steps_term <- mg_term %>%
  ungroup() %>%
  mutate(is_step = is_step_term) %>%
  group_by(member_id) %>%
  mutate(ever_step = sum(is_step == TRUE, na.rm = TRUE) > 0)

mg_out_term <- mg_steps_term %>%
  dplyr::select(member_id, year, name, party, win_fut, is_step, ever_step, ever_cabinet, already_cabinet, in_cabinet, fut_cabinet, is_govt) %>%
  group_by(member_id) %>%
  mutate(
    earliest_step = min(year[is_step == 1], na.rm = TRUE),
    already_step = year >= earliest_step,
    already_step = ifelse(is.na(already_step), 0, already_step)
  ) %>%
  # Replace NAs with FALSE
  mutate(across(is_step:already_step, ~ ifelse(is.na(.x), FALSE, .x)))

write.csv(mg_out_term, "output/mod_data/stepping_offices_by_term.csv")