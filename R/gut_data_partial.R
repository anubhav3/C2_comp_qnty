#this samples 'propn' amount of links from the predation matrix 'gut_mat'
gut_data_partial <- function(gut_mat, propn){
  
  L <- sum(gut_mat)
  S <- dim(gut_mat)[1]
  C <- L/S^2
  
  n <- as.integer(propn*L)
  ind <- which(gut_mat == 1)
  sel_ind <- sample(ind, n)
  
  gut_mat_partial <- matrix(0, nrow = S, ncol = S)
  gut_mat_partial[sel_ind] <- 1
  
  return(gut_mat_partial)
}


#this returns the diet breadth of n number of predators through sampling
gut_data_pred_index <- function(pred_index, n){
  samp_ind <- sample(pred_index, n)
  
  return(samp_ind)
}


#this samples 'propn_diet' amount of diets from the predation matrix 'gut mat' and 'propn_links' number of links
#from the diet breadth

gut_data_partial_1 <- function(gut_mat, propn_diet, propn_links){
  
  L <- sum(gut_mat)
  S <- dim(gut_mat)[1]
  C <- L/S^2
  
  n <- as.integer(propn_diet*L)
  ind <- which(gut_mat == 1)
  sel_ind <- sample(ind, n)
  
  gut_mat_partial <- matrix(0, nrow = S, ncol = S)
  gut_mat_partial[sel_ind] <- 1
  
  
  
  return(gut_mat_partial)
}