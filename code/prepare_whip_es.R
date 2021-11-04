library(tidyverse)
library(rio)

# Load data
file_list = list.files("data/eggers_spirling/cohesion_scores/")

import_file = function(x){
    yr = sub(".*_.*_([0-9]*)\\.csv", "\\1", x)
    dat = import(paste0("data/eggers_spirling/cohesion_scores/", x)) %>%
        mutate(year = as.numeric(yr))
    return(dat)
}

all = map_dfr(file_list, ~ import_file(.x))

all_export = all %>%
  rename(member_id = m.id) %>% 
  select(-V1)

write.csv(all_export, "output/mod_data/whip_es.csv")