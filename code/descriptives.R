library(tidyverse)
library(kableExtra)
source("code/0_functions.R")

df = import("output/mod_data/candidates_cabinet.csv")

# Descriptives
desc = df %>% group_by(year, party, unopposed ) %>%
    filter(party %in% c("C", "L")) %>%
    filter(is_ire_val == TRUE, mp_switch_between == FALSE) %>%
    filter(!is.na(party)) %>%
    mutate(party = as.character(party)) %>%
    summarise(count = n(), 
              share_win = mean(winner, na.rm = TRUE) %>% round(., digits = 2)) %>%
    pivot_wider(id_cols = year, names_from = party:unopposed, values_from = count:share_win) %>%
    dplyr::select(-c(count_C_NA, count_L_NA, share_win_C_NA, share_win_L_NA)) %>%
    filter(year <= 1918)

desc = desc[, c("year", "count_C_0", "share_win_C_0", "count_C_1", "count_L_0", "share_win_L_0", "count_L_1")]

names(desc) = c("Year", "# Cand (Opp)", "Share Win", "# Cand (Unopp)", "# Cand (Opp)", "Share Win", "# Cand (Unopp)")

tbl_desc = kable(desc, "latex", 
                 booktabs = TRUE,
                 linesep = "",
                 caption = "\\textbf{Number of candidates by year and party}. Cand (Opp) refers to the number of candidates in contested constituencies. Share Win is the share of successful candidacies in contested constituencies. Cand (Unopp) refers to the number of candidacies that were not opposed by other candidates. \\label{tab:desc}") %>%
    add_header_above(c("", "Conservative" = 3, "Liberal" = 3)) %>%
    kable_styling(latex_options = c("scale_down", "hold_position"))

cat(tbl_desc, file = "output/tables/desc_stats.tex")
