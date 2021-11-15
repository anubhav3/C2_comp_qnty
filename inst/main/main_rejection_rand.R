#We look at how food web prediction measured by TSS varies with number of predator gut data and variation in the body size of the predators sampled.
library(R.utils); library(tictoc); library(stringr)
library(ggpubr); library(plotrix); library(ggplot2)
library(cowplot); library(latex2exp); library(HDInterval)
library(doParallel); library(foreach); library(raster)
library(cheddar); library(dplyr); library(reshape2)
library(psych); library(DirectedClustering); library(colorspace)


## Relative path from the project directory
sourceDirectory("R", modifiedOnly=FALSE)

#Remember to check these:
fw_name <- "Broadstone Stream size_agg_v2" 
prior_dist_x <- prior_unif_x
dist_ss <- dist_TSS
desc_main <- "TSS_gut_ind"
##########################

fname_data <- paste("data/", fw_name, ".web.Rdata", sep="")
fw_data <- readRDS(fname_data)
nspecies <- dim(fw_data$predation.matrix)[1]
rule <- "rand"
fname_gut <- paste("data/gut_data/", fw_name, "_", rule, ".gut.Rdata", sep="")
gut_data_main <- readRDS(file = fname_gut)

model <- ratio.power_exp
model_core_par <- ADBM_core_par(fw_data)
model_prior_par <- ADBM_prior_par(fw_data)
input_par <- input_parameters()
n_ind_gut <- dim(gut_data_main)[2]
n_gut_sample <- 100
seq_ind_gut <- seq(8, n_ind_gut, by = 20)

for(i in seq_ind_gut){
  dir_N <- input_par$N
  dir_tol <- input_par$tol
  
  desc <- paste(desc_main, '_l', i, sep = '')
  
  fname_sp_ind <- paste("results/rejection/",fw_name,"/rule_ind/diet/", fw_name ,"_",i,".gut_index.Rdata", sep = "")
  species_ind <- readRDS(fname_sp_ind)
  
  dir_main <- paste("results/rejection/",fw_name,"/", "rule_", rule,'/N=', dir_N, '_tol=', dir_tol, '_n_diet = ', i,
                    sep = "")
  dir.create(dir_main)
  for(n_sample in 1:n_gut_sample){
    ## Start of computation time
    tic("Time elapsed:")
    
    
    ## Creating a directory where all the results will be stored
    dirnam <- paste(c(dir_main, '/rn_sample=', n_sample,'_N=', dir_N, '_tol=', dir_tol,"_", desc), collapse = '')
    dir.create(dirnam)
    
    print(i)
    ## Running the rejection algorithm
    output <- rejection_rand(fw_data = fw_data, model = model, model_core_par = model_core_par,
                            model_prior_par = model_prior_par, input_parameters = input_par, dist_main = dist_ss,
                            prior_dist = prior_dist_x, gut_data_main = gut_data_main, n_dietbreadth = i,
                            weight_type = "epanechnikov", species_ind = species_ind[n_sample,], 
                            pred_mat = fw_data$predation.matrix)
    print(paste("i = ", i))
    print(paste("n_sample = ", n_sample))
    saveRDS(output, file = paste(c(dirnam,"/",fw_name,".Rdata"), collapse = ''))
    
    time_elap <- toc()
    print(paste("Proportion of accepted simulations =", round(dir_N/output$total_sim,3)))
    
    ## Displaying some important information in a pdf file
    display_info_rej(prop_acc_sim = round(dir_N/output$total_sim,6), time = time_elap, dirnam = dirnam, ncores = input_par$n_cores)
    
  }
  
}

