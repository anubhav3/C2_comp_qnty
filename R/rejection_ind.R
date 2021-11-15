# 28.07.2021
# implements rejection algorithm from Handbook of Approximate Bayesian Computation

rejection_ind <- function(fw_data, model, model_core_par,
                          model_prior_par, input_parameters, dist_main,
                          prior_dist, gut_data_main, n_dietbreadth,
                          weight_type, species_ind, pred_mat)
{
  M <- fw_data$species.sizes    #body size
  tol <- input_parameters$tol
  N <- input_parameters$N
  n_cores <- input_parameters$n_cores
  n_par <- model_core_par$n
  
  pred_nodes <- which(colSums(fw_data$predation.matrix) != 0)
  
  registerDoParallel(cores=n_cores)
  
  
  
  post_dists <- data.frame(a=double(), ai=double(), aj=double(), r.b=double())
  n <- N/n_cores
  
  res_pcores <- foreach(i = 1:n_cores, .combine = rbind) %dopar% 
    {
      set.seed(n_sample*1000 + i*200)
      
      TSS_fw <- numeric(n)
      FPR_fw <- numeric(n)
      dist_gut <- numeric(n)
      total_sim <- 0
      
      #Extracting partial gut data here
      n_ind_gut <- dim(gut_data_main)[2]
      nspecies <- dim(gut_data_main)[1]-2
      indexes <- species_ind
      dist_ss <- dist_main
      gut_data <- matrix(0, nrow = nspecies, ncol = nspecies)
      
      partial_diet <- gut_data_main[,indexes]
      
      if(length(indexes) != 1){
        
        pred_ind <- partial_diet[2,]
        uniq_ind <- sort(unique(partial_diet[2,]))
        for(ind in uniq_ind){
          local_ind <- which(pred_ind == ind)
          if(length(local_ind) > 1)
          {
            diet_temp <- rowSums(partial_diet[3:(nspecies+2),local_ind])
            diet_temp[which(diet_temp > 1)] = 1
            gut_data[,ind] <-  diet_temp
          }
          else
          {
            gut_data[,ind] <- partial_diet[3:(nspecies+2),local_ind]
          }
        }
      }
      
      else
      {
        pred_ind <- partial_diet[2]
        uniq_ind <- pred_ind
        gut_data[,pred_ind] <- partial_diet[3:(nspecies+2)]
      }
      
      
      ss_real <- gut_data
      count <- 1
      while(count <= n)
      {
        
        
        local_count <- 0
        if(local_count == 0){
          ## Generating parameters from sampling
          local_par <- prior_dist(par=model_prior_par, no=1)
          
          ## Generating summary stat from likelihood
          ss_sim <- model(opt=local_par, x=model_core_par)
          num_gen <- runif(1, 0, 1)
          
          
          dist_temp <- dist_ss(ss_sim = ss_sim[,uniq_ind], ss_real = ss_real[,uniq_ind])
          
          TSS_fw[count] <- 1 - dist_ss(ss_sim = ss_sim[, pred_nodes], ss_real = pred_mat[, pred_nodes])
          FPR_fw[count] <- FPR_fun(ss_sim = ss_sim[, pred_nodes], ss_real = pred_mat[, pred_nodes])
          dist_gut[count] <- dist_temp
          post_dists <- rbind(post_dists, local_par)
          count <- count + 1
          local_count <- local_count + 1
          # }
        }
        total_sim <- total_sim + 1
        
      }
      
      list(post_dists_pcore = post_dists, TSS_fw_pcore = TSS_fw, dist_gut_pcore = dist_gut, 
           FPR_fw_pcore = FPR_fw, total_sim_pcore = total_sim)
    }
  
  post_dists <- res_pcores[[1]]
  TSS_fw <- res_pcores[[n_cores+1]]
  dist_gut <- res_pcores[[2*n_cores+1]]
  FPR_fw <- res_pcores[[3*n_cores+1]]
  total_sim <- res_pcores[[4*n_cores+1]]
  
  if(n_cores > 1){
    for (i in 1:(n_cores-1)){
      post_dists <- rbind(post_dists, res_pcores[[i+1]])
      TSS_fw <- c(TSS_fw, res_pcores[[n_cores+i+1]])
      dist_gut <- c(dist_gut, res_pcores[[2*n_cores+i+1]])
      FPR_fw <- c(FPR_fw, res_pcores[[3*n_cores+i+1]])
      total_sim <- total_sim + res_pcores[[4*n_cores+i+1]]
    }
  }
  
  list(TSS_fw = TSS_fw, dist_gut = dist_gut, post_dists = post_dists, FPR_fw = FPR_fw, total_sim = total_sim)
  
}


