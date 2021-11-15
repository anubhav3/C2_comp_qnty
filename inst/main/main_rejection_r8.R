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
dist_ss <- dist_TSS
desc_main <- "TSS_gut_r8"
##########################

fname_data <- paste("data/", fw_name, ".web.Rdata", sep="")
fw_data <- readRDS(fname_data)
nspecies <- dim(fw_data$predation.matrix)[1]
rule <- "r8"
fname_gut <- paste("data/gut_data/", fw_name, "_", rule, ".gut.Rdata", sep="")
gut_data_main <- readRDS(file = fname_gut)
main_propn <- 1
n_gut_sample <- 100

model <- ratio.power_exp
model_core_par <- ADBM_core_par(fw_data)
model_prior_par <- ADBM_prior_par(fw_data)
input_par <- input_parameters()

for(i in 1:nspecies){
  dir_N <- input_par$N
  dir_tol <- input_par$tol
   
  desc <- paste(desc_main, '_l', i, "_prop_", main_propn, sep = '')
  
  fname_sp_ind <- paste("results/rejection/",fw_name,"/rule_r8/diet/", fw_name ,"_",i,".gut_index.Rdata", sep = "")
  species_ind <- readRDS(fname_sp_ind)
  
  dir_main <- paste("results/rejection/",fw_name,"/", "rule_", rule,'/N=', dir_N, '_tol=', dir_tol, '_n_pred = ', i,
                    sep = "")
  dir.create(dir_main)
  for(n_gut in 1:n_gut_sample){
    ## Start of computation time
    tic("Time elapsed:")
    
    
    ## Creating a directory where all the results will be stored
    dirnam <- paste(c(dir_main, '/rn_gut=', n_gut,'_N=', dir_N, '_tol=', dir_tol,"_", desc), collapse = '')
    dir.create(dirnam)
    
    print(i)
    ## Running the rejection algorithm
    output <- rejection_r8(fw_data = fw_data, model = model, model_core_par = model_core_par,
                           model_prior_par = model_prior_par, input_parameters = input_par, dist_main = dist_ss,
                           prior_dist = prior_dist_x, gut_data_main = gut_data_main, n_dietbreadth = i,
                           main_propn = main_propn, weight_type = "epanechnikov", species_ind = species_ind[n_gut,])
    print(paste("i = ", i))
    print(paste("n_gut = ", n_gut))
    saveRDS(output, file = paste(c(dirnam,"/",fw_name,".Rdata"), collapse = ''))
    
    time_elap <- toc()
    print(paste("Proportion of accepted simulations =", round(dir_N/output$total_sim,3)))
    
    ## Displaying some important information in a pdf file
    display_info_rej(prop_acc_sim = round(dir_N/output$total_sim,6), time = time_elap, dirnam = dirnam, ncores = input_par$n_cores)
    
  }

}

