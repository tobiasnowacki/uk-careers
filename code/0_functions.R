# Replication files for 'The Emergence of Party-Based Political Careers in the UK, 1801-1918'
# Cox & Nowacki (Journal of Politics, forthcoming)
# 0_functions.R: utility functions

# Dependencies ------------------------------------------------
library(tidyverse)
library(rio)
library(lubridate)
library(lfe)
library(devtools)
library(stringr)
source_url("https://raw.githubusercontent.com/tobiasnowacki/RTemplates/master/plottheme.R")
library(rdrobust)

# Utility functions -------------------------------------------

#' Insert dashed era lines into plot
#'
ref_years <- function() {
  geom_vline(xintercept = c(1832, 1867, 1885), lty = "dashed")
}

#' Check government status throughout a given interval
#'
check_govt_status <- function(interval1, party) {
  x <- govlist %>%
    filter(party == govt_party)
  sum(int_overlaps(interval1, x$span) == TRUE, na.rm = TRUE) > 0
}

#' Clean names
#'
clean_name <- function(x) {
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

#' Shorten name
#'
shorten_name <- function(x) {
  str_replace_all(x, "([\\S])\\p{L}+ | ", "\\1 ")
}

# Check office status --------------------------------------


#' Fx to match last election year
#'
date_floor <- function(date) {
  y <- ge_dates[as.Date(ge_dates) < as.Date(date)] %>% as.Date()
  max(y)
}

#' Get acting government at given date
#'
date_govt <- function(date) {
  x <- govlist %>%
    filter(as.Date(date) %within% span)
  x$govt_party
  if (nrow(x) < 1) {
    return(NA)
  } else {
    return(x$govt_party)
  }
}

date_floor_govt <- function(date) {
  x <- govlist %>%
    filter(as.Date(date) %within% span)
  x$date_from
  if (nrow(x) < 1) {
    return(NA)
  } else {
    return(x$date_from)
  }
}

# Confidence intervals ------------------------------------------

#' Very simple function getting confidence interval for time series
#'
get_ci <- function(x, type = "lower") {
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