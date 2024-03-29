library(R.utils); library(tictoc); library(stringr)
library(ggpubr); library(plotrix); library(ggplot2)
library(cowplot); library(latex2exp); library(HDInterval)
library(doParallel); library(foreach); library(raster)
library(cheddar); library(dplyr); library(reshape2)
library(psych); library(DirectedClustering); library(colorspace)


## Relative path from the project directory
sourceDirectory("R", modifiedOnly=FALSE)

#Remember to check these:
fw_name <- "sim_Small Reef" 
prior_dist_x <- prior_unif_x
dist_ss <- dist_gut
desc_main <- "TSS_gut_r6"
##########################

fname_data <- paste("data/", fw_name, ".web.Rdata", sep="")
fw_data <- readRDS(fname_data)
nspecies <- dim(fw_data$predation.matrix)[1]
rule <- "r6"
fname_gut <- paste("data/gut_data/", fw_name, "_", rule, ".gut.Rdata", sep="")
gut_data_main <- readRDS(file = fname_gut)
main_propn <- 0.7

model <- ratio.power_exp
model_core_par <- ADBM_core_par(fw_data)
model_prior_par <- ADBM_prior_par(fw_data)
input_par <- input_parameters()

for(i in 1:nspecies){
  
  # indexes <- gut_data_pred_index(pred_index = c(1:nspecies), n = i)
  # dist_ss <- dist_exact_index(index = indexes)
  # gut_data <- matrix(0, nrow = nspecies, ncol = nspecies)
  # 
  # for(k in indexes){
  #   link_ind <- which(as.numeric(gut_data_main[,k]) == 1)
  #   n_link_ind <- length(link_ind)
  #   nsubsamples <- as.integer(main_propn*n_link_ind)
  #   sub_link_ind <- sample(link_ind, nsubsamples)
  #   gut_data[,k][c(sub_link_ind)] <- gut_data_main[,k][c(sub_link_ind)]
  # }
  # 
  
  desc <- paste(desc_main, '_l', i, "_prop_", main_propn, sep = '')
  ## Start of computation time
  tic("Time elapsed:")
  
  ## Creating a directory where all the results will be stored
  dir_N <- input_par$N 
  dir_tol <- input_par$tol
  dirnam <- paste(c("results/rejection/",fw_name,"/",'rN=', dir_N, '_tol=', dir_tol,"_", desc), collapse = '')
  dir.create(dirnam)
  
  print(i)
  ## Running the rejection algorithm
  output <- rejection_r6(all.web.info = fw_data, model = model, model_core_par = model_core_par,
                      model_prior_par = model_prior_par, input_parameters = input_par, dist_main = dist_ss,
                      prior_dist = prior_dist_x, gut_data = gut_data_main, n_dietbreadth = i,
                      main_propn = main_propn)
  print(i)
  saveRDS(output, file = paste(c(dirnam,"/",fw_name,".Rdata"), collapse = ''))
  
  time_elap <- toc()
  print(paste("Proportion of accepted simulations =", round(dir_N/output$total_sim,3)))
  
  ## Displaying some important information in a pdf file
  display_info_rej(prop_acc_sim = round(dir_N/output$total_sim,6), time = time_elap, dirnam = dirnam, ncores = input_par$n_cores)
  
  tic()
  
  ## Computing food web properties
  fname_output <- paste(dirnam, "/", fw_name, ".Rdata", sep = "")
  output <- readRDS(file = fname_output)
  properties <- fw_prop(foodweb = output, other = model_core_par, all.web.info = fw_data)
  fname_prop <- paste(dirnam, "/", fw_name,"_prop.Rdata", sep = "")
  saveRDS(properties, file = fname_prop)
  
  time_elap_fw <- toc()
  
  ## Displaying some important information in a pdf file
  display_info_prop(time = time_elap_fw, dirnam = dirnam)
  
  fname_pred_fw <- paste(dirnam, "/", fw_name, ".Rdata", sep = "")
  predicted_foodweb <- readRDS(file = fname_pred_fw)
  
  fname_prop <- paste(dirnam, "/", fw_name,"_prop.Rdata", sep = "")
  prop_web <- readRDS(file = fname_prop)
  
  ## Plotting the real and predicted food web matrix alongwith some food web properties
  plot_foodweb_prop(real_foodweb = fw_data, predicted_foodweb = predicted_foodweb,
                    dirnam = dirnam, model_core_par = model_core_par, 
                    model_prior_par = model_prior_par,
                    model = model, desc = desc, prior_dist_x = prior_dist_x, 
                    web.to.analyse = fw_name, prop_web = prop_web,
                    true_val_plot = T, gut_data = gut_data_main)
}

