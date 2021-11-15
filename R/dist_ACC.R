dist_ACC <- function(ss_sim, ss_real){
  TP <- sum(ss_sim==ss_real & ss_real==1)
  TN <- sum(ss_sim==ss_real & ss_real==0)
  P <- sum(ss_real==1)
  N <- sum(ss_real==0)
  
  acc <- (TP+TN)/(P+N)
  dist_ACC_r <- 1-acc
  
  return(dist_ACC_r)
  
}

dist_alphaACC <- function(ss_sim, ss_real){
  TP <- sum(ss_sim==ss_real & ss_real==1)
  TN <- sum(ss_sim==ss_real & ss_real==0)
  P <- sum(ss_real==1)
  N <- sum(ss_real==0)
  alpha <- 1
  acc <- alpha*(TP/P) + (1-alpha)*(TN/N)
  dist_alphaACC_r <- 1-acc
  
  return(dist_alphaACC_r)
}


dist_connectance_ACC <- function(ss_sim, ss_real){
  if(dist_connectance(ss_sim, ss_real) <= 0.01){
    dist_connectance_ACC_r <- dist_ACC(ss_sim, ss_real)
  }
  else dist_connectance_ACC_r <- 100
  
  return(dist_connectance_ACC_r)
}

dist_TSS_old <- function(ss_sim, ss_real){
  a <- sum(ss_sim==ss_real & ss_real==1)
  c <- sum(ss_real==1)-a
  d <- sum(ss_sim==ss_real & ss_real==0)
  b <- sum(ss_real==0)-d
  TSS_func <- (a*d-b*c)/((a+c)*(b+d))
  dist_TSS_r <- 1-TSS_func
  if(is.nan(dist_TSS_r)==TRUE){dist_TSS_r <- 1000}
  
  return(dist_TSS_r)
}

dist_TSS <- function(ss_sim, ss_real){
  a <- sum(ss_sim==1 & ss_real==1)
  b <- sum(ss_sim==1 & ss_real==0)
  c <- sum(ss_sim==0 & ss_real==1)
  d <- sum(ss_sim==0 & ss_real==0)
  
  TSS_func <- (a*d-b*c)/((a+c)*(b+d))
  dist_TSS_r <- 1-TSS_func
  if(is.nan(dist_TSS_r)==TRUE){dist_TSS_r <- 1000}
  
  return(dist_TSS_r)
}
dist_connectance <- function(ss_sim=ss_sim, ss_real=ss_real){
  S <- dim(ss_real)[1]
  ss_sim <- sum(ss_sim)/(S^2)
  ss_real <- sum(ss_real)/(S^2)
  dist_connectance_r <- abs(ss_sim-ss_real)
  
  return(dist_connectance_r)
}


connectance_f <- function(ss_sim = ss_sim){
  S <- dim(ss_sim)[1]
  connectance_f_r <- sum(ss_sim)/(S^2)
  
  return(connectance_f_r)
}


dist_connectance_prop <- function(ss_sim, ss_real){
  if(dist_connectance(ss_sim, ss_real) <= 0.01){
    TP <- sum(ss_sim==1 & ss_real==1)
    P <- sum(ss_real==1)
    distt <- 1-TP/P
  }
  else distt <- 999
  
  return(distt)
}

dist_exact_main <- function(index){
  
  dist_exact <- function(ss_sim, ss_real){
    
    comp <- sum(ss_sim[,1:index] != ss_real[,1:index])
    if(comp == 0){dist <- 0}
    else {dist <- 999}
    
    return(dist)
  }
  
  return(dist_exact)
}

dist_SIR <- function(ss_sim, ss_real, sp_ind){
  
  TL_x <- ss_sim
  TL_y <- ss_real
  dist <- sum(abs(TL_x[sp_ind]-TL_y[sp_ind]))
  
  return(dist)
}

dist_gut <- function(ss_sim, ss_real){
  a <- sum(ss_real == 1)
  b <- sum(ss_real == 1 & ss_sim == 1)
  diff <- a - b 
  if(diff == 0) {dist <- 0}
  else {dist <- 999}
  
  return(dist)
}

dist_exact_index <- function(index){
  
  dist_exact <- function(ss_sim, ss_real){
    
    comp <- sum(ss_sim[,index] != ss_real[,index])
    if(comp == 0){dist <- 0}
    else {dist <- 999}
    
    return(dist)
  }
  
  return(dist_exact)
}


FPR_fun <- function(ss_sim, ss_real){
  
  fp <- sum(ss_sim==1 & ss_real==0)
  tn <- sum(ss_sim==0 & ss_real==0)
  
  fpr <- fp/(fp+tn)
  
  return(fpr)
}

