#We sample k number of distinct predator individuals from n number of species


fw_name <- "sim_Small Reef"
pred_mat <- readRDS("data/gut_data/sim_Small Reef_r8.gut.Rdata")

n_species <- dim(pred_mat)[1]
n_sample <- 1000

for(i in 1:n_species){
  species_ind_mat <- matrix(data = 0, nrow = n_sample, ncol = i)
  for(k in 1:n_sample){
    species_ind_mat[k,] <- sample(x = 1:n_species, size = i)
  }
  f_name <- paste("results/rejection/sim_Small Reef/rule_s2/SIR/",fw_name,"_", i, ".SIR_index.Rdata", sep = "")
  saveRDS(object = species_ind_mat, file = f_name)
}

#species indices are generated with increasing number of species (1,2,3,...) with decreasing body size
fw_name <- "sim_Benguela Pelagic"
pred_mat <- readRDS("data/sim_Benguela Pelagic.web.Rdata")$predation.matrix

n_species <- dim(pred_mat)[1]
n_sample <- 1

for(i in 1:n_species){
  species_ind_mat <- matrix(data = 0, nrow = n_sample, ncol = i)
  species_ind_mat[1,] <- seq(n_species, n_species-i+1)
  f_name <- paste("results/rejection/sim_Benguela Pelagic/rule_s5/SIR/",fw_name,"_", i, ".SIR_index.Rdata", sep = "")
  # saveRDS(object = species_ind_mat, file = f_name)
}


#species indices are generated with increasing number of species (1,2,3,...) with increasing body size
fw_name <- "sim_Small Reef"
pred_mat <- readRDS("data/gut_data/sim_Small Reef_r8.gut.Rdata")

n_species <- dim(pred_mat)[1]
n_sample <- 1

for(i in 1:n_species){
  species_ind_mat <- matrix(data = 0, nrow = n_sample, ncol = i)
  species_ind_mat[1,] <- seq(1, i)
  f_name <- paste("results/rejection/sim_Small Reef/rule_s4/SIR/",fw_name,"_", i, ".SIR_index.Rdata", sep = "")
  saveRDS(object = species_ind_mat, file = f_name)
}

