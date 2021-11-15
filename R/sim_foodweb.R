# Simulates a user defined food web

# Remember a and r.b are not in log scale here
sim_foodweb_par <- function(){
  return(data.frame(a = 10^-7.379692,
                    ai = -1.472906,
                    aj = 1.271297,
                    r.b = 10^0.4634886))
}


sim_foodweb <- function(sim_par, fw_name){
  
  
  fname <- paste("data/", fw_name, ".web.Rdata", sep = "")
  fw_data <- readRDS(file = fname)
  
  n_species <- length(fw_data$species.sizes)
  
  # set.seed(1)
  M <- fw_data$species.sizes
  M <- sort(M)
  
  sim_a <- sim_par$a
  sim_ai <- sim_par$ai
  sim_aj <- sim_par$aj
  sim_r.b <- sim_par$r.b
  
  local_par <- data.frame(a = sim_a, ai = sim_ai, aj = sim_aj, r.b = sim_r.b)
  sim_model_core_par <- list(e = 1, n = 1, ni = -3/4, r.a = 1, M  = M)
  pred_mat <- ratio.power(opt=local_par, x = sim_model_core_par)
  rownames(pred_mat) <- as.character(seq(1, n_species))
  colnames(pred_mat) <- as.character(seq(1, n_species))
  
  
  all.web.info <- list(web.name = fw_name, species.sizes = M, 
                       species.names = as.character(seq(1, n_species)),
                       predation.matrix = pred_mat, a = sim_a, ai = sim_ai, 
                       aj = sim_aj, r.b = sim_r.b)

  # saveRDS(all.web.info, file = paste(c("data/sim_", fw_name, ".web.Rdata"), collapse = ""))
  # saveRDS(all.web.info, file = paste(c("data/sim_best_", fw_name, ".web.Rdata"), collapse = ""))
  return(all.web.info)
}



sim_foodweb_predator <- function(sim_par, fw_name){
  
  fname <- paste("data/", fw_name, ".web.Rdata", sep = "")
  fw_data <- readRDS(file = fname)
  pred_nodes <- which(colSums(fw_data$predation.matrix)>0)
  
  n_species <- length(fw_data$species.sizes)
  
  set.seed(1)
  M <- fw_data$species.sizes
  M <- sort(M)
  
  sim_a <- sim_par$a
  sim_ai <- sim_par$ai
  sim_aj <- sim_par$aj
  sim_r.b <- sim_par$r.b
  
  local_par <- data.frame(a = sim_a, ai = sim_ai, aj = sim_aj, r.b = sim_r.b)
  sim_model_core_par <- list(e = 1, n = 1, ni = -3/4, r.a = 1, M  = M)
  pred_mat <- ratio.power(opt=local_par, x = sim_model_core_par)
  rownames(pred_mat) <- as.character(seq(1, n_species))
  colnames(pred_mat) <- as.character(seq(1, n_species))
  
  pred_mat[, -pred_nodes] <- 0
  
  all.web.info <- list(web.name = fw_name, species.sizes = M, 
                       species.names = as.character(seq(1, n_species)),
                       predation.matrix = pred_mat, a = sim_a, ai = sim_ai, 
                       aj = sim_aj, r.b = sim_r.b)
  
  # saveRDS(all.web.info, file = paste(c("data/sim_", fw_name, ".web.Rdata"), collapse = ""))
  # saveRDS(all.web.info, file = paste(c("data/sim_best_", fw_name, ".web.Rdata"), collapse = ""))
  return(all.web.info)
}
