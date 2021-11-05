# Replication files for 'The Emergence of Party-Based Political Careers in the UK, 1801-1918'
# Cox & Nowacki (Journal of Politics, forthcoming)
# partisan_simulation.R: Simulation for district splits


# Dependencies ----------------------------------------------
library(tidyverse)
library(devtools)
source_url(
    "https://raw.githubusercontent.com/tobiasnowacki/RTemplates/master/plottheme.R"
)

# --------------
# SIMULATION
# --------------

# Set up 50 districts w/ voters

#' Function to split district into two
#'
#' @param voter_vec: vector of partisan voters
#' @param param: partisan sorting parameter where 0 is random and 1 is fully partisan
district_split <- function(voter_vec, param) {
    # Set up containers
    vec_1 <- c()
    vec_2 <- c()

    # Shuffle vector randomly
    randvec <- voter_vec

    # Assign voters to subdistricts
    for (i in 1:length(randvec)) {

        # If either vec_1 or vec_2 are full, put rest of voter_vec into other one
        if (length(vec_1) == 50) {
            vec_2 <- c(vec_2, randvec[i:length(randvec)])
            return(list(vec_1, vec_2))
        }
        if (length(vec_2) == 50) {
            vec_1 <- c(vec_1, randvec[i:length(randvec)])
            return(list(vec_1, vec_2))
        }

        # If neither of them are full, take object, and decide where to put it
        decision <- rbinom(1, 1,
            prob = randvec[i] * param + (1 - randvec[i]) * (1 - param)
        )
        # print(decision)
        if (decision == 1) {
            vec_1 <- c(vec_1, randvec[i])
        } else {
            vec_2 <- c(vec_2, randvec[i])
        }
        #
    }

    return(list(vec_1, vec_2))
}
#' Run simulation for a given number of parameters and districts
#'
#' @param param: splitting parameter
#' @param districts: number of districts
#' @param id: simulation id
run_simulation <- function(param, districts, id) {

    # Partisan lean of 50 districts
    theta_districts <- runif(districts)

    # Every district has 100 voters
    # Partisans - 0 or 1
    voters <- map(theta_districts, ~ rbinom(100, 1, prob = .x))

    # original distribution of partisanship
    original_shares <- map_dbl(voters, ~ mean(.x))

    # split districts accordingly
    split_districts <- flatten(map(voters, ~ district_split(.x, param)))

    redistricted_shares <- map_dbl(split_districts, ~ mean(.x))

    return(data.frame(
        share = c(original_shares, redistricted_shares),
        type = c(rep("original", districts), rep("split", districts * 2)),
        param = paste0("param = ", param),
        id = id
    ))
}

# Carry out simulation ------------------------------------------------

sim_shares <- map2_dfr(1:1200, rep(c(0.5, 0.6, 0.7, 0.8, 0.9, 1), 200), ~ run_simulation(.y, 100, .x))

# Plot and export ----------------------------------------------------

ggplot(sim_shares, aes(x = share, group = interaction(type, id))) +
    geom_line(aes(color = type), alpha = 0.09, stat = "density") +
    scale_x_continuous(expand = c(0, 0)) +
    facet_wrap(~param) +
    theme_tn()

ggsave("output/figures/simulation.pdf", width = 7.5, height = 4, device = cairo_pdf)