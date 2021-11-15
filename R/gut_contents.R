## Returns diet of a species as per a rule.
## Rule: probability of a presence of a prey in the gut of a predator is directly proportional to the body size of the prey

set.seed(1)

pbly_r1 <- function(diet_breadth, M){
  pbly_temp <- (M*diet_breadth)/sum(M*diet_breadth)
  return(pbly_temp)
}

pbly_r2 <- function(diet_breadth, M){
  pbly_temp <- diet_breadth/sum(diet_breadth)
  return(pbly_temp)
}


diet <- function(diet_breadth, M, pbly_f){
  pbly <- pbly_f(diet_breadth, M)
  nspecies <- length(diet_breadth)
  species_index <- 1:nspecies
  diet_temp <- numeric(nspecies)
  n_diet <- sum(diet_breadth)
  if(sum(diet_breadth)!=0){
    diet_index <- sample(species_index, size = n_diet, replace = TRUE, prob = pbly)
    diet_temp[diet_index] <- rep(1, length(diet_index))
  }
  
  return(diet_temp)
}



gut_matrix <- function(pred_mat, M, pbly_f){
  nspecies <- length(M)
  diet_mat <- matrix(data = rep(0, nspecies^2), nrow = nspecies, ncol = nspecies)
  for(i in 1:nspecies){
    diet_mat[,i] <- diet(pred_mat[,i], M, pbly_f)
  }
  
  return(diet_mat)
}

diet_r3 <- function(diet_breadth){
  if(sum(diet_breadth) !=0 ){
    smallest_prey_index <- min(which(diet_breadth == 1))
    largest_prey_index <- max(which(diet_breadth == 1))
    diet_temp <- numeric(length(diet_breadth))
    diet_temp[c(smallest_prey_index, largest_prey_index)] =  c(1,1)
  }
  else{
    diet_temp <- diet_breadth
  }
  return(diet_temp)
}

gut_matrix_r3 <- function(pred_mat){
  nspecies <- dim(pred_mat)[1]
  diet_mat <- matrix(data = rep(0, nspecies^2), nrow = nspecies, ncol = nspecies)
  for(i in 1:nspecies){
    diet_mat[,i] <- diet_r3(pred_mat[,i])
  }
  
  return(diet_mat)
}

pbly_e1 <- function(diet_breadth, M, Mj, n, ni, a, ai, aj){
  abund <- abundance(n = n, M = M, ni = ni)
  scr <- space_clearance_rate(M = M, Mj = Mj, a = a, ai = ai, aj = aj)
  ecr <- abund*scr
  ecr_eff <- ecr*diet_breadth
  pbly <- ecr_eff/(sum(ecr_eff))
  # print(pbly)
}


diet_e1 <- function(diet_breadth, M, Mj, n, ni, a, ai, aj){
  pbly <- pbly_e1(diet_breadth = diet_breadth, M = M, Mj = Mj, n = n, ni = ni, a = a, ai = ai, aj = aj)
  nspecies <- length(diet_breadth)
  species_index <- 1:nspecies
  diet_temp <- numeric(nspecies)
  n_diet <- sum(diet_breadth)
  
  if(sum(diet_breadth)!=0){
    diet_index <- sample(species_index, size = n_diet, replace = TRUE, prob = pbly)
    diet_temp[diet_index] <- rep(1, length(diet_index))
  }
  
  return(diet_temp)
}

gut_matrix_e1 <- function(pred_mat, M, n, ni, a, ai, aj){
  nspecies <- length(M)
  diet_mat <- matrix(data = rep(0, nspecies^2), nrow = nspecies, ncol = nspecies)
  for(i in 1:nspecies){
    diet_mat[,i] <- diet_e1(diet_breadth = pred_mat[,i], M = M, Mj = M[i], n = n, ni = ni, a = a, ai = ai, aj = aj)
  }
  
  return(diet_mat)
}


pbly_e1r1 <- function(diet_breadth, M, Mj, n, ni, a, ai, aj){
  abund <- abundance(n = n, M = M, ni = ni)
  scr <- space_clearance_rate(M = M, Mj = Mj, a = a, ai = ai, aj = aj)
  ecr <- abund*scr
  ecr_eff <- ecr*diet_breadth
  pbly_ecr <- ecr_eff/(sum(ecr_eff))
  pbly_M <- M/sum(M)
  pbly <- pbly_ecr*pbly_M
  print(pbly)
}


diet_e1r1 <- function(diet_breadth, M, Mj, n, ni, a, ai, aj){
  pbly <- pbly_e1r1(diet_breadth = diet_breadth, M = M, Mj = Mj, n = n, ni = ni, a = a, ai = ai, aj = aj)
  nspecies <- length(diet_breadth)
  species_index <- 1:nspecies
  diet_temp <- numeric(nspecies)
  n_diet <- sum(diet_breadth)
  if(sum(diet_breadth)!=0){
    diet_index <- sample(species_index, size = n_diet, replace = TRUE, prob = pbly)
    diet_temp[diet_index] <- rep(1, length(diet_index))
  }
  
  return(diet_temp)
}

gut_matrix_e1r1 <- function(pred_mat, M, n, ni, a, ai, aj){
  nspecies <- length(M)
  diet_mat <- matrix(data = rep(0, nspecies^2), nrow = nspecies, ncol = nspecies)
  for(i in 1:nspecies){
    diet_mat[,i] <- diet_e1r3(diet_breadth = pred_mat[,i], M = M, Mj = M[i], n = n, ni = ni, a = a, ai = ai, aj = aj)
  }
  
  return(diet_mat)
}


# asd <- gut_matrix_e1(pred_mat = pred_mat, M = M, n = n, ni = ni, a = a, ai = ai, aj = aj)
