# 05.05.2021
rejection_s_r_2 <- function(fw_data, model, model_core_par, model_prior_par, 
                            input_parameters, dist_main, prior_dist, 
                            gut_data_main, main_propn, species_ind,
                            n_gut, pred_nodes, sir_data_main)
{
  M <- fw_data$species.sizes
  tol <- input_parameters$tol
  N <- input_parameters$N
  n_cores <- input_parameters$n_cores
  e <- model_core_par$e
  n_par <- model_core_par$n
  ni <- model_core_par$ni
  r.a <- model_core_par$r.a
  
  registerDoParallel(cores=n_cores)
  
  # ss_real <- gut_data
  
  
  post_dists <- data.frame(a=double(), ai=double(), aj=double(), r.b=double())
  n <- N/n_cores
  
  res_pcores <- foreach(i = 1:n_cores, .combine = rbind) %dopar% 
    {
      set.seed(n_gut * 200 + i + 1)
      
      TSS <- numeric(n)
      dist_tl <- numeric(n)
      dist_diet <- numeric(n)
      TSS_fw <- numeric(n)
      total_sim <- 0

      
      count <- 1
      while(count <= n)
      {
        
        #print(ss_real)
        
        local_count <- 0
        if(local_count == 0){
          ## Generating parameters from sampling
          local_par <- prior_dist(par = model_prior_par, no = 1)
          

          ss_sim <- model(opt = local_par, x = model_core_par)
          ss_sim[, -pred_nodes] <- 0
          
          ss_sim_tl <- trophic_position(web = ss_sim, title = "sim_foodweb")
          
          if(sum(is.na(ss_sim_tl)) == 0){
            dist_diet[count] <- dist_TSS(ss_sim = ss_sim[, species_ind], ss_real = gut_data_main[, species_ind])
            dist_tl[count] <- dist_SIR(ss_sim = ss_sim_tl[species_ind], ss_real = sir_data_main[species_ind])
            TSS_fw[count] <- 1 - dist_TSS(ss_sim = ss_sim[, pred_nodes], ss_real = gut_data_main[, pred_nodes])
            
            post_dists <- rbind(post_dists, local_par)
            count <- count + 1
            local_count <- local_count + 1
          }
          
        }
        total_sim <- total_sim + 1
        
      }
      
      list(post_dists_pcore = post_dists, TSS_pcore = TSS_fw, dist_diet_pcore = dist_diet, 
           dist_tl_pcore = dist_tl, total_sim_pcore = total_sim)
    }
  
  post_dists <- res_pcores[[1]]
  TSS_fw <- res_pcores[[n_cores+1]]
  dist_diet <- res_pcores[[2*n_cores+1]]
  dist_tl <- res_pcores[[3*n_cores+1]]
  total_sim <- res_pcores[[4*n_cores+1]]
  
  if(n_cores > 1){
    for (i in 1:(n_cores-1)){
      post_dists <- rbind(post_dists, res_pcores[[i+1]])
      TSS_fw <- c(TSS_fw, res_pcores[[n_cores+i+1]])
      dist_diet <- c(dist_diet, res_pcores[[2*n_cores+i+1]])
      dist_tl <- c(dist_tl, res_pcores[[3*n_cores+i+1]])
      total_sim <- total_sim + res_pcores[[4*n_cores+i+1]]
    }
  }
  
  list(post_dists = post_dists, TSS_fw = TSS_fw, dist_diet = dist_diet, 
       dist_tl = dist_tl, total_sim = total_sim)
  
}


