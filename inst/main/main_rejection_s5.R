# 08.04.2021
library(R.utils); library(tictoc); library(stringr)
library(plotrix); library(ggplot2)
library(cowplot); library(latex2exp); library(HDInterval)
library(doParallel); library(foreach); library(raster)
library(cheddar); library(dplyr); library(reshape2)
library(psych); library(DirectedClustering); library(colorspace)

## Relative path from the project directory
sourceDirectory("R", modifiedOnly=FALSE)

#Remember to check these:
fw_name <- "sim_Benguela Pelagic" 
prior_dist_x <- prior_unif_x
dist_ss <- dist_SIR
dist_TSS <- dist_TSS
desc_main <- "TSS_sir_s5"
##########################

fname_data <- paste("data/", fw_name, ".web.Rdata", sep="")
fw_data <- readRDS(fname_data)
real_pred_mat <- fw_data$predation.matrix
nspecies <- dim(fw_data$predation.matrix)[1]
rule <- "s5"
fname_sir <- paste("data/sir_data/", fw_name, "_", rule, ".sir.Rdata", sep="")
sir_data_main <- readRDS(file = fname_sir)
n_sir_sample <- 10

model <- ratio.power_exp
model_core_par <- ADBM_core_par(fw_data)
model_prior_par <- ADBM_prior_par(fw_data)
input_par <- input_parameters()

for(i in 1:nspecies){
  dir_N <- input_par$N
  dir_tol <- input_par$tol
  
  desc <- paste(desc_main, '_l', i, sep = '')
  
  fname_sp_ind <- paste("results/rejection/",fw_name,"/rule_", rule, "/sir/", fw_name ,"_",i,".sir_index.Rdata", sep = "")
  species_ind <- readRDS(fname_sp_ind)
  
  dir_main <- paste("results/rejection/",fw_name,"/", "rule_", rule,'/N=', dir_N, '_tol=', dir_tol, '_n_pred = ', i,
                    sep = "")
  dir.create(dir_main)
  for(n_sir in 1:n_sir_sample){
    ## Start of computation time
    tic("Time elapsed:")
    
    ## Creating a directory where all the results will be stored
    dirnam <- paste(c(dir_main, '/rn_sir=', n_sir,'_N=', dir_N, '_tol=', dir_tol,"_", desc), collapse = '')
    dir.create(dirnam)
    
    print(i)
    ## Running the rejection algorithm
    output <- rejection_s5(fw_data = fw_data, model = model, model_core_par = model_core_par,
                           model_prior_par = model_prior_par, input_parameters = input_par, dist_ss = dist_ss,
                           prior_dist = prior_dist_x, sir_data_main = sir_data_main[n_sir,],
                           weight_type = "epanechnikov", species_ind = species_ind[1,],
                           real_pred_mat = real_pred_mat, dist_TSS = dist_TSS, n_sir = n_sir)
    print(paste("i = ", i))
    print(paste("n_sir = ", n_sir))
    saveRDS(output, file = paste(c(dirnam,"/",fw_name,".Rdata"), collapse = ''))
    
    time_elap <- toc()
    print(paste("Proportion of accepted simulations =", round(dir_N/output$total_sim,3)))
    
    ## Displaying some important information in a pdf file
    display_info_rej(prop_acc_sim = round(dir_N/output$total_sim,6), time = time_elap, dirnam = dirnam, ncores = input_par$n_cores)
    
  }
  
}

