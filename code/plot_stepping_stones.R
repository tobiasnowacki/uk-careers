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

stepping_stones_diff <- read.csv(
    "output/mod_data/stepping_stones_diff.csv"
)

plot_step <- ggplot(stepping_stones_diff, aes(reorder(name, -diff), diff)) +
    geom_col(aes(fill = diff)) +
    geom_hline(aes(yintercept = 0.015), lty = "dotted") +
    theme_tn() +
    theme(axis.text.x = element_text(angle = 90)) +
    scale_fill_viridis(begin = 0.7, end = 0.1) +
    guides(fill = FALSE) +
    labs(x = "Office", y = "Pr(Office | Ever Cabinet) - Pr(Office | Never Cabinet)")

ggsave(plot_step,
    filename = "output/figures/stepping_stones.pdf",
    width = 11, height = 7, device = cairo_pdf
)