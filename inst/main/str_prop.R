# 03.05.2021
# We compute the structural properties here

library(R.utils); library(tictoc); library(stringr)
library(ggpubr); library(plotrix); library(ggplot2)
library(cowplot); library(latex2exp); library(HDInterval)
library(doParallel); library(foreach); library(raster)
library(cheddar); library(dplyr); library(reshape2)
library(psych); library(DirectedClustering); library(colorspace)
library(bipartite)

## Relative path from the project directory
sourceDirectory("R", modifiedOnly=FALSE)


fw_name <- "Broadstone Stream size_agg_v2"
dir_N <- 1e5
dir_tol <- 2 #different for ind and lbs and ubs
n_gut_sample <- 100 
propn <- c(seq(481, 1008, by = 20), 1008)
n_sel <- 1 #number of diets of predator selected with maximum TSS

desc_main <- "TSS_gut_ind"
rule <- "ind_predator"
l_ind <- 1

fname_data <- paste("data/", fw_name, ".web.Rdata", sep="")
fw_data <- readRDS(fname_data)
model_core_par <- ADBM_core_par(fw_data)

for(i in propn){
  desc <- paste(desc_main, '_l', i, sep = '')
  dir_main <- paste("../../../PhD_local/C2_files/", fw_name, "/rule_", rule ,'/N=', dir_N, '_tol=', dir_tol, '_n_diet = ', i,
                    sep = "")
  sim_TSS <- c()
  
  # This is just for initialising the variable prop_acc_accepted
  prop_acc_accepted <- list(TSS_fw = double(), dist_gut = double(),
                            post_dists = data.frame(a = double(), ai = double(), aj = double(), r.b = double()),
                            total_sim = n_gut_sample)
  
  for(n_sample  in 1:n_gut_sample){
    dirnam <- paste(c(dir_main, '/rn_sample=', n_sample,'_N=', dir_N, '_tol=', dir_tol,"_", desc), collapse = '')
    fname <- paste(dirnam, "/", fw_name, ".Rdata", sep = "")
    prop_acc <- readRDS(fname)
    gut_TSS <- 1 - prop_acc$dist
    gut_TSS_indexes <- order(gut_TSS, decreasing = TRUE)[1:n_sel]
    prop_acc_accepted$TSS_fw[n_sample] <- prop_acc$TSS_fw[gut_TSS_indexes]
    prop_acc_accepted$dist_gut[n_sample] <- prop_acc$dist_gut[gut_TSS_indexes]
    prop_acc_accepted$post_dists[n_sample,] <- prop_acc$post_dists[gut_TSS_indexes,]
  }
  
  calc_prop <- fw_prop_predator(foodweb = prop_acc_accepted, other = model_core_par, all.web.info = fw_data)
    
  fname_prop <- paste0(dir_main,"/", fw_name, "_prop.Rdata")
  saveRDS(object = calc_prop, file = fname_prop)
  print(i)
}



