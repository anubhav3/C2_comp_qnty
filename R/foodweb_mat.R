foodweb_mat <- function(parameters, foodweb){
  
  n_length <- length(parameters$a)
  n_species <- length(M)
  avg_att_rate_list<- numeric(n_length)
  other <- ADBM_core_par(foodweb = foodweb)
  M <- other$M
  for(i in 1:n_length){
    a=parameters$a[i]
    ai=parameters$ai[i]
    aj=parameters$aj[i]
    r.b=parameters$r.b[i]
    
    best.EHL <- Ratio.allometric.EHL(M=M,
                                     e=other$e,
                                     a=a, ai = ai, aj=aj,
                                     n=other$n, ni=other$ni,
                                     r.a=other$r.a, r.b=r.b)
    best.web <- Get.web(best.EHL, energy.intake = F)
    
    att_rate_mat <- matrix(nrow = n_species, ncol = n_species)
    
    for(j in 1:n_species){
      for(k in 1:n_species){
        att_rate_mat[j,k] <- a*M[j]^(ai)*M[k]^(aj)
      }
    }
    feas_att_rate <- att_rate_mat
    avg_att_rate <- mean(feas_att_rate)
    avg_att_rate_list[i] <- avg_att_rate
  }
  
  return(avg_att_rate_list)
}

