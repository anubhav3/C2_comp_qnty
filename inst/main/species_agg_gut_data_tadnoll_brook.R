# 14.06.2022
# Compile individual predator guts in species aggregated way

library(dplyr)
library(ggplot2)

bs_data  <- read.csv("~/Google Drive/PhD (UZH)/tadnoll/TadnollData_Gilljam_etal_AER_v45c3.csv", comment.char="#")

pred_species <- unique(bs_data$predID)
prey_species <- unique(bs_data$preyID)

species_name <- unique(union(pred_species, prey_species))

pred_size_df <- bs_data %>%
  select(c(predID, predMass)) %>%
  mutate(species = predID, mass = predMass) %>%
  select(-c(predID, predMass))

prey_size_df <- bs_data %>%
  select(c(preyID, preyMass)) %>%
  mutate(species = preyID, mass = preyMass) %>%
  select(-c(preyID, preyMass))

species_size_df <- rbind(pred_size_df, prey_size_df)

bodysize <- species_size_df %>%
  group_by(species) %>%
  summarise(size = exp(mean(log(mass))))

bodysize_sorted <- bodysize[order(bodysize$size),]


#### Predator guts ####

ngut <- length(unique(bs_data$predUniqueID))
nspecies <- length(species_name)

gut_data <- matrix(data = 0, nrow = nspecies + 2, ncol = ngut)

rownames(gut_data)[3:(nspecies+2)] <- bodysize_sorted$species
rownames(gut_data)[1] <- "log_size"
rownames(gut_data)[2] <- "species"

uniq_pred_identifier <- unique(bs_data$predUniqueID)
colnames(gut_data) <- uniq_pred_identifier

#### Constructing the predator gut matrix ####

total_row <- dim(bs_data)[1]
for(n_row in 1:total_row){
  row_sel <- bs_data[n_row,]
  gut_data[row_sel$preyID, row_sel$predUniqueID] <- 1
  gut_data[2, row_sel$predUniqueID] <- row_sel$predID
  gut_data[1, row_sel$predUniqueID] <- log10(row_sel$predMass)
}

colnames(gut_data) <- as.character(gut_data[2,])

## Storing species names as indexes (sorted according to bodysizes) for conveniences

species_ind <- data.frame(species = bodysize_sorted$species, index = 1:length(bodysize_sorted$species))

species_info <- data.frame(species = as.character(gut_data[2,]), gut_ind = 1:length(gut_data[2,]))

merged_species_info <- merge(x = species_info, y = species_ind, by.x = "species", by.y = "species", sort = TRUE)

gut_data[2,] <- merged_species_info[order(merged_species_info$gut_ind),]$index

class(gut_data) <- "numeric"

#### Predation matrix ####


species_name <- bodysize_sorted$species

pred_mat <- matrix(data = 0, nrow = nspecies, ncol = nspecies)
colnames(pred_mat) <- species_name
rownames(pred_mat) <- species_name

total_row <- dim(bs_data)[1]
for(n_row in 1:total_row){
  row_sel <- bs_data[n_row,]
  pred_mat[row_sel$preyID, row_sel$predID] <- 1
}

# saveRDS(object = gut_data, file = "data/gut_data/Tadnoll Brook species_agg_ind_predator.gut.Rdata")


fw_data <- list(web.name = "Tadnoll Brook species_agg",
                species.names = species_name,
                species.sizes = bodysize_sorted$size,
                predation.matrix = pred_mat)

# saveRDS(object = fw_data, file = "data/Tadnoll Brook species_agg.web.Rdata")
