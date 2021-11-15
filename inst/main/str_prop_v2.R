# 07.09.2021
# We compute the structural properties here, we use the parameters from the file accepted_par.Rdata
# 17.09.2021
# We save the properties in a single folder

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
propn <- c(seq(1, 1008, by = 20), 1008)
dir_N <- 1e5
dir_tol <- 2 #different for ind and lbs and ubs
n_gut_sample <- 100 

desc_main <- "TSS_gut_ind"
rule <- "ind_predator"
l_ind <- 1

fname_data <- paste("data/", fw_name, ".web.Rdata", sep="")
fw_data <- readRDS(fname_data)
model_core_par <- ADBM_core_par(fw_data)

par_accepted <- readRDS(paste("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_", rule, 
                      "/accepted_ones/accepted_par.Rdata",
                      sep = ""))

for(i in propn){
  desc <- paste(desc_main, '_l', i, sep = '')
  dir_main <- paste("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_", rule ,'/N=', dir_N, '_tol=', dir_tol, '_n_diet = ', i,
                    sep = "")
  sim_TSS <- c()

  
  fil_acc_par <- filter(par_accepted, ndiet == i)
  
  prop_acc_accepted <- prop_acc_accepted <- list(post_dists = 
                                                   data.frame(a = fil_acc_par$a, 
                                                              ai = fil_acc_par$ai, 
                                                              aj = fil_acc_par$aj, 
                                                              r.b = fil_acc_par$r.b)
                                                 )
  
  calc_prop <- fw_prop_predator(foodweb = prop_acc_accepted, other = model_core_par, all.web.info = fw_data)
  
  fname_prop <- paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_", rule, 
                       "/accepted_ones/properties/", "/", fw_name,"_n_diet=", i, "_properties.Rdata")
  saveRDS(object = calc_prop, file = fname_prop)
  print(i)
}



