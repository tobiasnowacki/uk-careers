# Replication files for 'The Emergence of Party-Based Political Careers in the UK, 1801-1918'
# Cox & Nowacki (Journal of Politics, forthcoming)
# prepare_data_elections.R: Tidy data for election-level analysis

# -------------
# DEPENDENCIES
# -------------
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

# ---------
# PREPARE DATA
# ---------

# Prepare Eggers-Spirling Data: Election-Level
elecs <- d_orig %>%
    mutate(
        date = as.Date(date),
        year = year(date)
    ) %>%
    filter(date > as.Date("1832-01-01") & date < as.Date("1930-01-01")) %>%
    mutate(year = ifelse(date == as.Date("1910-12-03"), 1911, year)) %>%
    filter(by_election == 0)

# Get IDs
ge_dates <- unique(elecs$date)
ge_dates2 <- unique(d_orig$date[d_orig$by_election == 0])
ge_ids <- unique(elecs$election_id)

# Clean party labels
con_labels <- c("C", "C (Ind C)", "C (Nat P)", "C (Nat P) (C)", "C*", "Ind L/Crf (LU)", "LU", "LU (C)", "LU (Nat P) (C)", "LU*", "U", "Co C")
lib_labels <- c("L", "Ind L (L)", "LU (L)", "Co L")

# how many party non-standard party labels?
trunc <- f %>%
    filter(party %in% con_labels[c(-1, -7)] |
        party %in% lib_labels[-1]) %>%
    left_join(elecs) %>%
    filter(year < 1912)
nrow(trunc)

# Prepare contest-level dataset
winners <- f %>%
    group_by(election_id) %>%
    mutate(party = case_when(
        party %in% con_labels ~ "C",
        party %in% lib_labels ~ "L",
        TRUE ~ "O"
    )) %>%
    mutate(
        v_share = votes / sum(votes),
        con_share = sum(v_share[party == "C"]),
        lib_share = sum(v_share[party == "L"])
    ) %>%
    ungroup() %>%
    filter(winner == 1 & election_id %in% ge_ids) %>%
    mutate(
        Con = party == "C" | party == "LU" | party == "LU (C)",
        Lib = party == "L",
        Other = party != "C" & party != "L"
    ) %>%
    group_by(election_id, unopposed) %>%
    summarise(
        Con = sum(Con),
        con_votes = mean(con_share),
        Lib = sum(Lib),
        lib_votes = mean(lib_share),
        Other = sum(Other)
    ) %>%
    left_join(elecs) # add contest-level info

# Contest-level dataset: add lagged seat shares / contest indicators
shares <- winners %>%
    mutate(
        tpty = Con + Lib,
        Con = Con / tpty,
        Lib = Lib / tpty,
        tpty_v = con_votes + lib_votes,
        con_votes = con_votes / tpty_v,
        lib_votes = lib_votes / tpty_v,
        con_sub = ifelse(
            con_votes == 0 | is.na(con_votes) == TRUE,
            Con,
            con_votes
        )
    ) %>%
    group_by(constituency.id) %>%
    arrange(date) %>%
    mutate(
        Con_lg = lag(Con),
        Con_lg2 = lag(Con, 2),
        unopposed_lg = lag(unopposed),
        unopposed_lg2 = lag(unopposed, 2),
        con_votes_lg = lag(con_votes),
        con_sub_lg = lag(con_sub),
        con_sub_lg2 = lag(con_sub, 2),
        year = substr(date, 1, 4)
    ) %>%
    mutate(year = ifelse(date == as.Date("1910-12-03"), 1911, year)) %>%
    filter(year != "1831") %>%
    ungroup() %>%
    dplyr::select(election_id, constituency.id, constituency.name, date, unopposed, year, Con, Lib, total_seats, seats_up)

# Get constituency ID for pre-1832 constituencies
d_pre <- d_orig %>%
    mutate(
        date = as.Date(date),
        year = year(date)
    ) %>%
    filter(date < as.Date("1832-01-01"), by_election == 0) %>%
    dplyr::select(election_id, constituency.id, constituency.name, year)

# Clean HSS data (pre-1832) to get contest-level
hss_shares <- hss %>%
    rowwise() %>%
    mutate(date = make_date(year, month, day)) %>%
    filter(bye == 0 & winner == 1) %>%
    group_by(election) %>%
    mutate(
        Con = party == "T",
        Lib = party == "W",
        unopposed = as.numeric(ncands == nwinners)
    ) %>%
    group_by(election, constituency, unopposed, year, date, nwinners) %>%
    summarise(
        Con = sum(Con),
        Lib = sum(Lib)
    ) %>%
    mutate(
        tpty = Con + Lib,
        Con = Con / tpty,
        Lib = Lib / tpty
    ) %>%
    ungroup() %>%
    group_by(constituency) %>%
    arrange(year) %>%
    mutate(
        Con_lg = lag(Con),
        Con_lg2 = lag(Con, 2),
        unopposed_lg = lag(unopposed),
        unopposed_lg2 = lag(unopposed, 2)
    ) %>%
    left_join(d_pre, by = c("constituency" = "constituency.name", "year")) %>%
    mutate(constituency.name = constituency, total_seats = nwinners, seats_up = nwinners) %>%
    ungroup() %>%
    dplyr::select(election_id, constituency.id, constituency.name, date, unopposed, year, Con, Lib, total_seats, seats_up)


# Bind pre-1832 and post-1832 together
mg_df <- rbind(hss_shares, shares) %>%
    # mutate_at(vars(con, lib), ~ replace_na(., 0)) %>%
    group_by(constituency.name) %>%
    arrange(year) %>%
    mutate(
        Con_lg = lag(Con),
        Con_lg2 = lag(Con, 2),
        unopposed_lg = lag(unopposed),
        unopposed_lg2 = lag(unopposed, 2),
        year_fac = factor(year)
    ) %>%
    distinct()

# prepare and merge in constituency dummies
cdum <- cdum %>%
    select(
        constituency_id, start_year, end_year, const_type,
        country, patron_sack, patron_gash, patron_hanham
    )

# add const dummies and recode patronal districts
mg_df <- mg_df %>%
    left_join(cdum, by = c("constituency.id" = "constituency_id")) %>%
    mutate(patronal = case_when(
        year < 1832 & patron_sack == 1 ~ 1,
        year %in% 1832:1867 & const_type == "borough" & patron_sack == 1 & patron_hanham == 1 ~ 1,
        year %in% 1832:1867 & const_type == "county" & patron_hanham == 1 ~ 1,
        year > 1867 & patron_hanham == 1 ~ 1,
        TRUE ~ 0
    ))

# keep important variables
export <- mg_df %>%
    dplyr::select(election_id, constituency.id, constituency.name, date, year_fac, unopposed, year, Con, Lib, Con_lg, Con_lg2, unopposed_lg, unopposed_lg2, const_type, country, patronal, total_seats)

# Export for next steps
write.csv(export, "output/mod_data/seat_shares.csv")