# Replication files for 'The Emergence of Party-Based Political Careers in the UK, 1801-1918'
# Cox & Nowacki (Journal of Politics, forthcoming)
# prepare_office.R: Tidy data for office-level analysis

# Dependencies ----
pkgs <- c("tidyverse", "rio", "lubridate", "devtools", "fastDummies")
lapply(pkgs, library, character.only = TRUE, quietly = TRUE)

# Source code ----
source_url("https://raw.githubusercontent.com/tobiasnowacki/RTemplates/master/plottheme.R")
source("code/0_functions.R")

# Load data ----
cand_cab <- read.csv("output/mod_data/candidates_cabinet.csv") # candidate data
ofid <- import("data/eggers_spirling/offices.csv") # original office data
# officeholder data
of <- import("data/eggers_spirling/officeholdings.csv") %>%
    mutate(
        start_date = ymd(start_date),
        end_date = ymd(end_date),
        in_cabinet = ifelse(is.na(in_cabinet), 0, in_cabinet)
    ) %>%
    filter(as.Date(start_date) < as.Date("1929-01-01"))

# Functions ----

# Function to expand officeholding into parliamentary terms
expand_terms <- function(start_date, end_date,
                         in_cabinet,
                         member_id,
                         office_id,
                         parl_term_df) {
    i_off <- interval(start_date, end_date)
    over <- map_lgl(
        parl_term_df[["interval_elec"]],
        ~ int_overlaps(i_off, .x)
    )
    out <- parl_term_df[over, ]
    tibble(
        year_elec = out$year_elec,
        in_cabinet = in_cabinet,
        member_id = member_id,
        office_id = office_id
    ) %>% return()
}

# for every office holder, expand into years
expand_years <- function(start_date,
                         end_date,
                         in_cabinet,
                         member_id,
                         office_id) {
    yrs <- 0
    if (!is.na(year(end_date) - year(start_date))) {
        yrs <- year(start_date):year(end_date)
        # check if first / last year are longer than one half
        if (month(start_date) < 7) {
            yrs <- yrs[-1]
            if (month(end_date) >= 7) {
            }
            yrs <- yrs[-length(yrs)]
        }
    }
    if (length(yrs) < 1) {
        return(NA)
    }
    tibble(
        year = yrs,
        in_cabinet = in_cabinet,
        member_id = member_id,
        office_id = office_id
    )
}


# Prepare data ----

# Simple descriptive: How often is each office listed?
of_n <- of %>%
    group_by(office_id) %>%
    summarise(n = n()) %>%
    left_join(ofid) %>%
    arrange(-n) %>%
    filter(n >= 5)

# Keep when office has more than 5 office holders
of <- of %>%
    filter(office_id %in% of_n$office_id)

# prepare DF with election intervals
ge_dates <- unique(cand_cab$date) %>% sort()
ge_dates <- ge_dates[ge_dates > date("1832-01-01")]

ge_date_df <- tibble(
    from = ge_dates[-25],
    to = ge_dates[-1],
    year_elec = year(ge_dates[-25])
) %>%
    mutate(interval_elec = interval(from, to))


# Prepare Office-By-Term DF ----

# For loop for all the offices to be expanded
for_term_list <- list()
for (i in 1:nrow(of)) {
    print(i)
    for_term_list[[i]] <- expand_terms(
        of$start_date[i],
        of$end_date[i],
        of$in_cabinet[i],
        of$member_id[i],
        of$office_id[i],
        ge_date_df
    )
}
offices_by_term <- do.call(rbind, for_term_list)

# Into wide format
office_by_term_member <- offices_by_term %>%
    dummy_cols(select_columns = "office_id") %>%
    # Group by member id and term to avoid duplicates
    group_by(member_id, year_elec) %>%
    summarise(across(
        starts_with("office_id_"),
        ~ sum(.x, na.rm = TRUE)
    ),
    in_cabinet = max(in_cabinet, na.rm = TRUE)
    ) %>%
    mutate(in_cabinet = ifelse(is.na(in_cabinet), 0, in_cabinet))

# Add ever / future cabinet
office_term_ext <- office_by_term_member

write_csv(office_term_ext, "output/mod_data/office_by_term_member.csv")

# Prepare Office-By-Year DF ----

# Expand offices to yearly frame
for_list <- list()
for (i in 1:nrow(of)) {
    for_list[[i]] <- expand_years(of$start_date[i], of$end_date[i], of$in_cabinet[i], of$member_id[i], of$office_id[i])
}
offices_by_year <- do.call(rbind, for_list) %>%
    filter(!(year == 0 | is.na(year)))

# turn into wide format --> office id into columns
office_by_year_member <- offices_by_year %>%
    dummy_cols(select_columns = "office_id") %>%
    group_by(member_id, year) %>%
    summarise(across(starts_with("office_id_"), ~ sum(.x, na.rm = TRUE)),
        in_cabinet = max(in_cabinet, na.rm = TRUE)
    ) %>%
    mutate(in_cabinet = ifelse(is.na(in_cabinet), 0, in_cabinet))

office_ext <- office_by_year_member

# Export -----

# save as df
write_csv(office_ext, "output/mod_data/office_by_year_member.csv")