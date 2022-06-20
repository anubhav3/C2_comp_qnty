# 13.06.2022
# Compile individual predator guts in species aggregated way

library(dplyr)
library(ggplot2)
library(readr)


bs_data <- read_delim("../../GitHub/Celtic Sea/data/Predator_and_prey_body_sizes_in_marine_food_webs_vsn4.txt", 
                      "\t", escape_double = FALSE, trim_ws = TRUE)
bs_data <- bs_data %>%
  filter(!Prey %in% c("fish larvae", "fish unidentified", "flatfish") ) %>%
  filter(!is.na(Prey) & !is.na(Predator)) %>%
  filter(`Geographic location` == "Europe, Celtic Sea ecosystem")


