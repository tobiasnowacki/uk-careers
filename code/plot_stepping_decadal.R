pkgs <- c("tidyverse", 
          "rio",
          "lubridate",
          "devtools",
          "rdrobust")
lapply(pkgs, library, character.only = TRUE, quietly = TRUE)
source_url("https://raw.githubusercontent.com/tobiasnowacki/RTemplates/master/plottheme.R")

source("code/0_functions.R")

mg_steps <- read.csv("output/mod_data/stepping_offices.csv")

# Prepare for decadal plot
mg_plot_decadal <- mg_steps %>%
    filter(is_govt == TRUE, already_cabinet == FALSE, patronal == 0) %>%
    mutate(decade = year_annual - year_annual %% 10) %>%
    group_by(member_id, decade) %>%
    summarise(
        is_step = sum(is_step == TRUE, na.rm = TRUE) >= 1,
        ever_cabinet = sum(ever_cabinet == TRUE, na.rm = TRUE) >= 1
    ) %>%
    group_by(decade, is_step) %>%
    summarise(
        mean_cab = mean(ever_cabinet, na.rm = TRUE),
        mean_cab_low = get_ci(ever_cabinet, "lower"),
        mean_cab_high = get_ci(ever_cabinet, "upper"),
        n = n()
    ) %>%
    filter(decade %in% 1840:1900)

# Plot
plot_dec <- ggplot(mg_plot_decadal, aes(decade, mean_cab)) +
    geom_ribbon(aes(
        ymin = mean_cab_low,
        ymax = mean_cab_high,
        group = is_step
    ),
    alpha = 0.3
    ) +
    geom_point(aes(shape = is_step)) +
    geom_line(aes(lty = is_step)) +
    theme_tn() +
    coord_cartesian(ylim = c(0, 1)) +
    labs(
        x = "Decade", y = "Pr(Ever Cabinet | Stepping Stone)",
        lty = "Stepping Stone Office?", shape = "Stepping Stone Office?"
    )
ggsave(plot_dec, 
    filename = "output/figures/stepping_prob.pdf",
    width = 4, height = 4, device = cairo_pdf
)
