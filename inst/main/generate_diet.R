#We sample k number of distinct predator individuals from n number of species


fw_name <- "Broadstone Stream size_agg"
rule <- "g1"
fname <- paste("data/gut_data/",fw_name,"_",rule,".gut.Rdata", sep = "")
pred_mat <- readRDS(fname)

n_species <- dim(pred_mat)[1]
n_sample <- 1000

for(i in 1:n_species){
  species_ind_mat <- matrix(data = 0, nrow = n_sample, ncol = i)
  for(k in 1:n_sample){
    species_ind_mat[k,] <- sample(x = 1:n_species, size = i)
  }
  f_name <- paste("results/rejection/",fw_name,"/rule_",rule,"/diet/",fw_name,"_", i, ".gut_index.Rdata", sep = "")
  # saveRDS(object = species_ind_mat, file = f_name)
}



#We sample k number of distinct predator guts from n number of predators gut

fw_name <- "Trancura size_agg"
rule <- "ind_predator"
fname <- paste("data/gut_data/",fw_name,"_",rule,".gut.Rdata", sep = "")
gut_mat <- readRDS(fname)

n_ind_gut <- dim(gut_mat)[2]
n_sample <- 1000

set.seed(1)
for(i in 1:n_ind_gut){
  species_ind_mat <- matrix(data = 0, nrow = n_sample, ncol = i)
  for(k in 1:n_sample){
    species_ind_mat[k,] <- sample(x = 1:n_ind_gut, size = i)
  }
  f_name <- paste("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name,"/rule_",rule,"/diet/",fw_name,"_", i, ".gut_index.Rdata", sep = "")
  saveRDS(object = species_ind_mat, file = f_name)
}



#We sample k number of distinct predator guts from n number of predators gut
#the predator guts fall in two sets: first set where body size is less than average and other set where the body size greater
#than average

fw_name <- "Celtic Sea size_agg"
rule <- "ind_predator"
fname <- paste("data/gut_data/",fw_name,"_",rule,".gut.Rdata", sep = "")
gut_mat <- readRDS(fname)

avg_M <- mean(gut_mat[1,])
lower_ind <- which(gut_mat[1,] <= avg_M)
upper_ind <- which(gut_mat[1,] > avg_M)

n_sample <- 1000

#We sample the predator indices with body size less than average
set.seed(1)

for(i in 1:length(lower_ind)){
  species_ind_mat <- matrix(data = 0, nrow = n_sample, ncol = i)
  for(k in 1:n_sample){
    species_ind_mat[k,] <- sample(x = lower_ind, size = i)
  }
  f_name <- paste("results/rejection/",fw_name,"/rule_lbs_predator/diet/",fw_name,"_", i, ".gut_index.Rdata", sep = "")
  # saveRDS(object = species_ind_mat, file = f_name)
}

#We sample the predator indices with body size greater than average
set.seed(1)

for(i in 1:length(upper_ind)){
  species_ind_mat <- matrix(data = 0, nrow = n_sample, ncol = i)
  for(k in 1:n_sample){
    species_ind_mat[k,] <- sample(x = upper_ind, size = i)
  }
  f_name <- paste("results/rejection/",fw_name,"/rule_ubs_predator/diet/",fw_name,"_", i, ".gut_index.Rdata", sep = "")
  saveRDS(object = species_ind_mat, file = f_name)
}



##Rule rand: Here, we generate random diet of predators with random connectance
#We sample k number of distinct predator guts from n number of predators gut

set.seed(1)
fw_name <- "Broadstone Stream size_agg_v2"
rule <- "rand"
fname <- paste("data/gut_data/",fw_name,"_",rule,".gut.Rdata", sep = "")


n_ind_gut <- 1008
n_species <- 29
diet_mat <- matrix(data = NA, nrow = n_species+2, ncol = n_ind_gut)
diet_mat[1,] <- sample(1:n_species, size = n_ind_gut, replace = TRUE)
diet_mat[2,] <- diet_mat[1,]

for(pred in 1:n_ind_gut){
  connectance <- runif(1)
  links <- ifelse(runif(n_species) < connectance, 1, 0)
  diet_mat[3:(n_species+2), pred] <- links
}

# saveRDS(object = diet_mat, file = fname)


n_sample <- 1000

for(i in 1:n_ind_gut){
  f_name <- paste("results/rejection/",fw_name,"/rule_","ind","/diet/",fw_name,"_", i, ".gut_index.Rdata", sep = "")
  gut_data <- readRDS(file = f_name)
  f_name_rand <- paste("results/rejection/",fw_name,"/rule_", rule,"/diet/",fw_name,"_", i, ".gut_index.Rdata", sep = "")
  # saveRDS(object = gut_data, file = f_name_rand)
}
