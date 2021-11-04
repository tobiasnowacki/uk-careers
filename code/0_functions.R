# UK Careers Project
# This file contains some functions used across the project.
#


###
### DEPENDENCIES
###

library(tidyverse)
library(rio)
library(lubridate)
library(lfe)
library(devtools)
library(stringr)
source_url("https://raw.githubusercontent.com/tobiasnowacki/RTemplates/master/plottheme.R")
library(rdrobust)

###
### FUNCTIONS
###

# -----------------
#### Insert dashed era lines in plot
# -----------------

ref_years <- function(){
  geom_vline(xintercept = c(1832, 1867, 1885), lty = "dashed")
}

# -----------------
#### Check government status
# -----------------

check_govt_status <- function(interval1, party){
  x <- govlist %>% 
    filter(party == govt_party)
  sum(int_overlaps(interval1, x$span) == TRUE, na.rm = TRUE) > 0
}

# -----------------
#### Name cleaning
# -----------------

clean_name <- function(x){
    x %>%
    tolower() %>%
    gsub("(\\.)", " ", .) %>%
    gsub("\\.\\s+", " ", .) %>%
    gsub("\\s+", " ", .) %>%
    gsub("[^[:alnum:] ]", "", .) %>%
    # gsub(" i+$", "", .) %>%
    gsub("^sir |^mr ", "", .) %>%
    gsub("hon ", "", .) %>%
    gsub(",? bt.?", "", .) %>%
    gsub("viscount ", "", .)
}

shorten_name <- function(x){
  str_replace_all(x, "([\\S])\\p{L}+ | ", "\\1 ")
}

# ----------------------
#### Check office status
# ----------------------

# Compile list of MPs holding office in any given interval
cabinet_df <- function(start_year, end_year){
  pruned <- of %>%
    filter(as.Date(start_date) <= end_year & 
           as.Date(end_date) >= start_year) %>%
    dplyr::select(member_id, in_cabinet) %>%
    group_by(member_id) %>%
    summarise(year = start_year,
              office = 1,
              in_cabinet = (sum(in_cabinet, na.rm = TRUE) > 0) %>% 
                                as.numeric) %>% 
    unique()
  # tibble(year = start_year, member_id = unique(pruned$member_id), cabinet = 1)
}

# --------------------
#### Date-floor functions
# --------------------

# process_division <- function(x){
#     date <- x$date[1] %>% ymd()
#     whiplist_res <- whiplist %>%
#       filter(date %within% inter)
#     x$out <- x$member_id %in% whiplist_res$member_id
#     return(x)
# }

# get_mode <- function(x) {
#   ux <- unique(x)
#   ux[which.max(tabulate(match(x, ux)))]
# }

# F'n to match last election year
date_floor <- function(date){
  y <- ge_dates[as.Date(ge_dates) < as.Date(date)] %>% as.Date
  max(y)
}

date_govt <- function(date){
  x <- govlist %>%
    filter(as.Date(date) %within% span)
  x$govt_party
  if(nrow(x) < 1){
    return(NA)
  } else{
    return(x$govt_party)
  }
}

date_floor_govt <- function(date){
  x <- govlist %>%
    filter(as.Date(date) %within% span)
  x$date_from
  if(nrow(x) < 1){
    return(NA)
  } else{
    return(x$date_from)
  }
}

# Very simple function getting confidence interval for time series
get_ci <- function(x, type = "lower"){
  x <- x[is.na(x) == FALSE]

  # Handle case where series is N = 1
  if (length(unique(x)) == 1) {
    return(mean(x, na.rm = TRUE))
  }
  
  # Otherwise, return lower/upper estimate
  if (type == "lower") {
    return(t.test(x, conf = 0.95)$conf[1])
  } 
  if (type == "upper") {
    return(t.test(x, conf = 0.95)$conf[2])
  }
  
}
