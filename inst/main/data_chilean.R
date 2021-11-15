# 07.10.2021
# Extract the dataset from food webs Coilaco, Guampoe and Trancura

library(dplyr)
library(ggplot2)

org_data <- read.csv("~/Google Drive/GitHub/C2_comp_qnty/data/raw_data/ChileanRiverData_Gilljam_etal_AER_v45ch3.csv", comment.char="#")


###################### Coilaco food web ###########################

bs_data <- org_data %>%
  filter(locality == "Coilaco")

# We first construct the predation matrix

n_node <- length(unique(c(bs_data$predID, bs_data$preyID)))

pred_ind <- c()
max_size <- numeric(1)
min_size <- numeric(1)

min_size <- log10(min(bs_data$preyMass, bs_data$preyMass))
max_size <- log10(max(bs_data$predMass, bs_data$predMass))

h <- (max_size-min_size)/n_node
nbin <- n_node
M <- 10^(min_size + ((0:(nbin-1)+0.5))*h)

mass_to_ind <- function(mass){
  index <- ceiling((mass-min_size)/h)
  if(index == 0) {index = 1}
  return(index)
}

species_name <- as.character(1:nbin)
ndim <- length(species_name)
pred_mat <- matrix(rep(0,ndim*ndim), nrow = ndim, ncol = ndim,
                   dimnames = list(as.character(1:nbin), as.character(1:nbin)))


for(row_no in 1:nrow(bs_data)){
  
  mt <- bs_data[row_no,]
  prey_mass <- log10(bs_data$preyMass[row_no])
  pred_mass <- log10(bs_data$predMass[row_no])
  
  predator_node <- mass_to_ind(pred_mass)
  prey_node <- mass_to_ind(prey_mass)
  
  pred_mat[prey_node, predator_node] <- 1
  
}

all.web.info <- list(predation.matrix = pred_mat,
                     species.sizes = M,
                     web.name = "Coilaco size_agg")


# saveRDS(object = all.web.info, file = "../C2_comp_qnty/data/Coilaco size_agg.web.Rdata")


# Now, we construct the diet matrix of predators

uniq_ind <- unique(bs_data$predUniqueID)

n_pred_ind <- length(uniq_ind)

#The first row of the diet_mat corresponds to the predator identity denoted by a unique number and second row consists of bin number
diet_mat <- matrix(data = 0, nrow = n_node+2, ncol = n_pred_ind,)
diet_mat[1,] <- c(1:n_pred_ind)
i <- 1 


for(row_no in 1:nrow(bs_data)){
  
  mt <- bs_data[row_no,]
  
  pseudo_pred_ind <- which(uniq_ind == bs_data$predUniqueID[row_no])
  
  prey_mass <- log10(mt$preyMass)
  prey_name <- mass_to_ind(prey_mass)
  
  pred_mass <- log10(mt$predMass)
  pred_name <- mass_to_ind(pred_mass)
  
  pred_ind <- mt$predUniqueID
  pseudo_pred_ind <- which(uniq_ind == pred_ind) 
  
  diet_mat[prey_name+2, pseudo_pred_ind] <- 1
  diet_mat[1, pseudo_pred_ind] <- log10(mt$predMass)
  diet_mat[2, pseudo_pred_ind] <- pred_name
  
}

fname <- "../C2_comp_qnty/data/gut_data/Coilaco size_agg_ind_predator.gut.Rdata"
# saveRDS(diet_mat, fname)

##################################################################

###################### Guampoe food web ###########################

bs_data <- org_data %>%
  filter(locality == "Guampoe")

# We first construct the predation matrix

n_node <- length(unique(c(bs_data$predID, bs_data$preyID)))

pred_ind <- c()
max_size <- numeric(1)
min_size <- numeric(1)

min_size <- log10(min(bs_data$preyMass, bs_data$preyMass))
max_size <- log10(max(bs_data$predMass, bs_data$predMass))

h <- (max_size-min_size)/n_node
nbin <- n_node
M <- 10^(min_size + ((0:(nbin-1)+0.5))*h)

mass_to_ind <- function(mass){
  index <- ceiling((mass-min_size)/h)
  if(index == 0) {index = 1}
  return(index)
}

species_name <- as.character(1:nbin)
ndim <- length(species_name)
pred_mat <- matrix(rep(0,ndim*ndim), nrow = ndim, ncol = ndim,
                   dimnames = list(as.character(1:nbin), as.character(1:nbin)))


for(row_no in 1:nrow(bs_data)){
  
  mt <- bs_data[row_no,]
  prey_mass <- log10(bs_data$preyMass[row_no])
  pred_mass <- log10(bs_data$predMass[row_no])
  
  predator_node <- mass_to_ind(pred_mass)
  prey_node <- mass_to_ind(prey_mass)
  
  pred_mat[prey_node, predator_node] <- 1
  
}

all.web.info <- list(predation.matrix = pred_mat,
                     species.sizes = M,
                     web.name = "Guampoe size_agg")


# saveRDS(object = all.web.info, file = "../C2_comp_qnty/data/Guampoe size_agg.web.Rdata")


# Now, we construct the diet matrix of predators

uniq_ind <- unique(bs_data$predUniqueID)

n_pred_ind <- length(uniq_ind)

#The first row of the diet_mat corresponds to the predator identity denoted by a unique number and second row consists of bin number
diet_mat <- matrix(data = 0, nrow = n_node+2, ncol = n_pred_ind)
diet_mat[1,] <- c(1:n_pred_ind)
i <- 1 


for(row_no in 1:nrow(bs_data)){
  
  mt <- bs_data[row_no,]
  
  pseudo_pred_ind <- which(uniq_ind == bs_data$predUniqueID[row_no])
  
  prey_mass <- log10(mt$preyMass)
  prey_name <- mass_to_ind(prey_mass)
  
  pred_mass <- log10(mt$predMass)
  pred_name <- mass_to_ind(pred_mass)
  
  pred_ind <- mt$predUniqueID
  pseudo_pred_ind <- which(uniq_ind == pred_ind) 
  
  diet_mat[prey_name+2, pseudo_pred_ind] <- 1
  diet_mat[1, pseudo_pred_ind] <- log10(mt$predMass)
  diet_mat[2, pseudo_pred_ind] <- pred_name
  
}

fname <- "../C2_comp_qnty/data/gut_data/Guampoe size_agg_ind_predator.gut.Rdata"
# saveRDS(diet_mat, fname)

##################################################################


###################### Trancura food web ###########################

bs_data <- org_data %>%
  filter(locality == "Trancura")

# We first construct the predation matrix

n_node <- length(unique(c(bs_data$predID, bs_data$preyID)))

pred_ind <- c()
max_size <- numeric(1)
min_size <- numeric(1)

min_size <- log10(min(bs_data$preyMass, bs_data$preyMass))
max_size <- log10(max(bs_data$predMass, bs_data$predMass))

h <- (max_size-min_size)/n_node
nbin <- n_node
M <- 10^(min_size + ((0:(nbin-1)+0.5))*h)

mass_to_ind <- function(mass){
  index <- ceiling((mass-min_size)/h)
  if(index == 0) {index = 1}
  return(index)
}

species_name <- as.character(1:nbin)
ndim <- length(species_name)
pred_mat <- matrix(rep(0,ndim*ndim), nrow = ndim, ncol = ndim,
                   dimnames = list(as.character(1:nbin), as.character(1:nbin)))


for(row_no in 1:nrow(bs_data)){
  
  mt <- bs_data[row_no,]
  prey_mass <- log10(bs_data$preyMass[row_no])
  pred_mass <- log10(bs_data$predMass[row_no])
  
  predator_node <- mass_to_ind(pred_mass)
  prey_node <- mass_to_ind(prey_mass)
  
  pred_mat[prey_node, predator_node] <- 1
  
}

all.web.info <- list(predation.matrix = pred_mat,
                     species.sizes = M,
                     web.name = "Trancura size_agg")


# saveRDS(object = all.web.info, file = "../C2_comp_qnty/data/Trancura size_agg.web.Rdata")


# Now, we construct the diet matrix of predators

uniq_ind <- unique(bs_data$predUniqueID)

n_pred_ind <- length(uniq_ind)

#The first row of the diet_mat corresponds to the predator identity denoted by a unique number and second row consists of bin number
diet_mat <- matrix(data = 0, nrow = n_node+2, ncol = n_pred_ind,)
diet_mat[1,] <- c(1:n_pred_ind)
i <- 1 


for(row_no in 1:nrow(bs_data)){
  
  mt <- bs_data[row_no,]
  
  pseudo_pred_ind <- which(uniq_ind == bs_data$predUniqueID[row_no])
  
  prey_mass <- log10(mt$preyMass)
  prey_name <- mass_to_ind(prey_mass)
  
  pred_mass <- log10(mt$predMass)
  pred_name <- mass_to_ind(pred_mass)
  
  pred_ind <- mt$predUniqueID
  pseudo_pred_ind <- which(uniq_ind == pred_ind) 
  
  diet_mat[prey_name+2, pseudo_pred_ind] <- 1
  diet_mat[1, pseudo_pred_ind] <- log10(mt$predMass)
  diet_mat[2, pseudo_pred_ind] <- pred_name
  
}

fname <- "../C2_comp_qnty/data/gut_data/Trancura size_agg_ind_predator.gut.Rdata"
# saveRDS(diet_mat, fname)

##################################################################