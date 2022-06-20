# 20.05.2022
# Compile individual gut data of predators in species aggregated way

#### Broadstone Stream ####

library(dplyr)
library(ggplot2)
library(tidyr)

## Abundances data
abundances_clean <- read.csv("~/Google Drive/GitHub/Broadstone_stream/data/abundances_clean.csv")
abundances_clean <- filter(abundances_clean, Species != "Bezzia" & Species != "Other Diptera" & Species != "Platambus")


data_files <- c("1Woodward Feb.csv", "2Woodward Apr.csv", "3Woodward June.csv",
                "4Woodward Aug.csv", "5Woodward Dec.csv", "6Woodward Oct.csv")


f <- data_files[1]

month_data <- read.csv(paste("../../GitHub/Broadstone_stream/data/", f, sep = ""))


#### Aggregating all the months data into a single data frame ####

all_data <- month_data
for(f in data_files[-1]){
  month_data <- read.csv(paste("../../GitHub/Broadstone_stream/data/", f, sep = ""))
  all_data <- rbind(all_data, month_data)
}

all_data_fil <- all_data[, c(1,5,6,7,13:42,81,82)]

all_data_fil_longer <- all_data_fil %>%
  pivot_longer(
    cols = 5:34,
    names_to = "Prey.species"
  ) %>% 
  filter(value == 1) %>%
  select(-c("value")) %>%
  filter(!Predator.species %in% c("Platambus", "Bezzia", "Ceratopogonidae")) %>%
  filter(!Prey.species %in% c("Platambus", "Bezzia", "Ceratopogonidae"))
  



#### Aggregating the prey and predator body sizes ####

pred_size_df <- all_data_fil_longer %>%
  select(c("Month", "Predator.species", "Predator.Individual.Number", "Log.predator.mass"))


pred_size_df_fil <- pred_size_df %>%
  group_by(Month, Predator.species, Predator.Individual.Number) %>%
  summarise(Log.predator.mass = unique(Log.predator.mass)) 

pred_size_df_fil <- pred_size_df_fil[, c("Predator.species", "Log.predator.mass")]
names(pred_size_df_fil) <- c("Species", "log_mass")


prey_size_df_fil <- all_data_fil_longer %>%
  select("Prey.species", "Log.prey.mass") 

names(prey_size_df_fil) <- c("Species", "log_mass")


all_size_df <- rbind(pred_size_df_fil, prey_size_df_fil)

body_size <- all_size_df %>%
  group_by(Species) %>%
  summarise(size = mean(log_mass))

body_size_sorted <- body_size[order(body_size$size),]


#### Predator guts ####

ngut <- dim(all_data_fil_longer %>%
              group_by(Month, Predator.species, Predator.Individual.Number) %>%
              summarise(N = n()))[1]

nspecies <- length(unique(abundances_clean$Species))

gut_data <- matrix(data = 0, nrow = nspecies + 2, ncol = ngut)

rownames(gut_data)[3:(nspecies+2)] <- body_size_sorted$Species
rownames(gut_data)[1] <- "log_size"
rownames(gut_data)[2] <- "species"

uniq_pred_identifier <- unique(all_data_fil_longer$Individual.code)
colnames(gut_data) <- uniq_pred_identifier


#### Constructing the predator gut matrix ####

total_row <- dim(all_data_fil_longer)[1]
for(n_row in 1:total_row){
  row_sel <- all_data_fil_longer[n_row,]
  gut_data[row_sel$Prey.species, row_sel$Individual.code] <- 1
  gut_data[2, row_sel$Individual.code] <- row_sel$Predator.species
  gut_data[1, row_sel$Individual.code] <- row_sel$Log.predator.mass
}

colnames(gut_data) <- as.character(gut_data[2,])

## Storing species names as indexes (sorted according to bodysizes) for conveniences

species_ind <- data.frame(species = body_size_sorted$Species, index = 1:length(body_size_sorted$Species))

species_info <- data.frame(species = as.character(gut_data[2,]), gut_ind = 1:length(gut_data[2,]))

merged_species_info <- merge(x = species_info, y = species_ind, by.x = "species", by.y = "species", sort = TRUE)

gut_data[2,] <- merged_species_info[order(merged_species_info$gut_ind),]$index

class(gut_data) <- "numeric"


#### Predation matrix ####


species_name <- body_size_sorted$Species

pred_mat <- matrix(data = 0, nrow = nspecies, ncol = nspecies)
colnames(pred_mat) <- species_name
rownames(pred_mat) <- species_name

total_row <- dim(all_data_fil_longer)[1]
for(n_row in 1:total_row){
  row_sel <- all_data_fil_longer[n_row,]
  pred_mat[row_sel$Prey.species, row_sel$Predator.species] <- 1
}


# saveRDS(object = gut_data, file = "data/gut_data/Broadstone Stream species_agg_v2_ind_predator.gut.Rdata")



fw_data <- list(web.name = "Broadstone Stream species_agg_v2",
                species.names = species_name,
                species.sizes = 10^body_size_sorted$size,
                predation.matrix = pred_mat)

# saveRDS(object = fw_data, file = "data/Broadstone Stream species_agg_v2.web.Rdata")




