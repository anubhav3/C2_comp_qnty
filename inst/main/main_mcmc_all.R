library(R.utils)
library(tictoc)

## Loading required libraries
library(ggpubr); library(plotrix); library(ggplot2)
library(cowplot); library(latex2exp); library(HDInterval)
library(doParallel); library(foreach); library(raster)
library(cheddar); library(dplyr); library(reshape2)

library(psych); library(DirectedClustering); library(colorspace)
library(stringr); library(readxl)


## Relative path from the project directory
sourceDirectory("R", modifiedOnly=FALSE)

dist_par_data <- read_excel("data/dist_TSS_conn.xlsx")
fw_ind <- 1
for(foodweb in dist_par_data$foodweb){
  
  ## Start of computation time
  tic("Time elapsed:")
  
  web.to.analyse <- foodweb
  foodweb_data <- load(paste("data/", web.to.analyse, ".web.Rdata", sep=""))
  all.web.info <- get(foodweb_data)
  
  model = ratio.power_exp
  model_core_par = ADBM_core_par(all.web.info)
  model_prior_par = ADBM_prior_par(all.web.info)
  input_par = data.frame(tol = dist_par_data$dist[fw_ind],
                         N = 1e5,
                         n_cores = 3)
  prior_dist_pbly = prior_pbly_unif
  prior_dist_x = prior_unif_x
  proposal_dist_pbly = proposal_dist_pbly_unif_mcmc
  proposal_dist_x = proposal_dist_x_unif_mcmc
  ADBM_par_sd_ <- ADBM_par_sd()
  dist_ss = dist_TSS
  desc = "conn_TSS_1"
  
  ## Creating a directory where all the results will be stored
  dir_N <- input_par$N 
  dir_tol <- input_par$tol
  
  dirnam <- paste(c("results/mcmc/",web.to.analyse,"/",'rN=', dir_N, '_tol=', dir_tol,"_", desc), collapse = '')
  dir.create(dirnam)
  
  ## Running the rejection algorithm
  abclike.RH.web <- mcmc(all.web.info = all.web.info, model = model, model_core_par = model_core_par,
                         model_prior_par=model_prior_par, input_parameters=input_par, dist_ss = dist_ss,
                         prior_dist_pbly= prior_dist_pbly, prior_dist_x= prior_dist_x, 
                         proposal_dist_pbly, proposal_dist_x, ADBM_par_sd_)
  
  saveRDS(abclike.RH.web, file = paste(c(dirnam,"/",web.to.analyse,".Rdata"), collapse = ''))
  
  
  time_elap <- toc()
  print(paste("Proportion of accepted simulations =", round(dir_N/abclike.RH.web$total_sim,3)))
  
  print(paste("No.of jumps =", abclike.RH.web[[1]]$n_jump))
  ## Displaying some important information in a pdf file
  display_info_mcmc(prop_acc_sim = abclike.RH.web[[1]]$n_jump, time = time_elap, dirnam = dirnam)
  
  # tic()
  ## Plotting the real and predicted food web matrix alongwith some food web properties
  # plot_foodweb(real_foodweb = all.web.info, predicted_foodweb = abclike.RH.web,
  #              dirnam = dirnam, model_core_par = model_core_par,
  #              model_prior_par = model_prior_par,
  #              model = model, desc=desc, prior_dist_x = prior_dist, web.to.analyse = web.to.analyse)
  
  # time_elap_fw <- toc()
  ## Displaying some important information in a pdf file
  # display_info_fw(prop_acc_sim = round(dir_N/abclike.RH.web$total_sim,6), time = time_elap_fw, dirnam = dirnam)
  
  fw_ind <- fw_ind + 1
  
  print(paste(foodweb, " done!", sep = ""))
}

