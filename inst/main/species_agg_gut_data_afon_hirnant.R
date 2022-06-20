# 15.06.2022
# Compile individual predator guts in species aggregated way


library(dplyr)
library(ggplot2)

bs_data <- read.csv("~/Google Drive/PhD (UZH)/tadnoll/AfonHirnantData_Gilljam_etal_AER_v45ch3.csv", comment.char="#")

pred_species <- unique(bs_data$predID)
prey_species <- unique(bs_data$preyID)

species_name <- unique(union(pred_species, prey_species))

