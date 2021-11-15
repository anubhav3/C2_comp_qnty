# 22.07.2021
# We look at the Mi/Mj ratio of body size 

library(readr)
library(dplyr)
library(R.utils)
library(cheddar)

sourceDirectory("../C1_method/C1_method/R", modifiedOnly = FALSE)

bs_data <- read.csv("~/Google Drive/PhD (UZH)/tadnoll/AfonHirnantData_Gilljam_etal_AER_v45ch3.csv", comment.char="#")

bs_data <- bs_data %>%
  filter(!is.na(preyID) & !is.na(predID))

prey_range <- range(log10(bs_data$preyMass))
pred_range <- range(log10(bs_data$predMass))
l_limit <- min(prey_range, pred_range)
u_limit <- max(prey_range, pred_range)

n_species <- 33
nbin <- n_species
minimum <- l_limit
h <- (u_limit-l_limit)/nbin
M <- 10^(l_limit + ((0:(nbin-1)+0.5))*h)


mass_to_ind <- function(mass){
  index <- ceiling((mass-minimum)/h)
  if(index == 0) { index = 1}
  
  return(as.integer(index))
}


species_name <- as.character(1:nbin)
ndim <- length(species_name)
pred_mat <- matrix(rep(0,ndim*ndim), nrow = ndim, ncol = ndim,
                   dimnames = list(as.character(1:nbin), as.character(1:nbin)))
count <- 0

dd <- data.frame(Mi_by_Mj_ratio = double(), row_no = integer())

for(row_no in 1:nrow(bs_data)){
  
  count <- count + 1
  
  mt <- bs_data[row_no,]
  prey_mass <- log10(bs_data$preyMass[row_no])
  pred_mass <- log10(bs_data$predMass[row_no])
  
  predator_node <- mass_to_ind(pred_mass)
  prey_node <- mass_to_ind(prey_mass)
  
  Mi_by_Mj_ratio <- (bs_data$preyMass[row_no])/(bs_data$predMass[row_no])
  
  dd <- rbind(dd,
              data.frame(Mi_by_Mj_ratio = Mi_by_Mj_ratio, row_no = row_no))
  
  pred_mat[prey_node, predator_node] <- 1
  
  
}


dd_sort <- dd[order(dd, Mi_by_Mj_ratio),]


hist(log10(Mi_by_Mj_ratio), breaks = 50)


# Let's look at the extremes
N <- dim(dd_sort)[1]
check <- list()
count <- 1
for(k in 1:N){
  row_no_k <- dd_sort$row_no[k]
  bs_data_k <- bs_data[row_no_k,]
  
  prey_of_k <- bs_data %>%
    filter(predUniqueID == bs_data_k$predUniqueID)
  
  pred_of_k <- bs_data %>%
    filter(preyID == bs_data_k$preyID)
  
  check_local <- intersect(prey_of_k$preyID, pred_of_k$predID)
  if(length(check_local)!=0){
    check[[count]] <- check_local
    print(check_local)
    print(k)
    count <- count + 1
    }
}




