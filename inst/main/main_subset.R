library(R.utils)
library(tictoc)

## Loading required libraries
library(ggpubr); library(plotrix); library(ggplot2)
library(cowplot); library(latex2exp); library(HDInterval)
library(doParallel); library(foreach); library(raster)
library(cheddar); library(dplyr); library(reshape2)

library(psych); library(DirectedClustering); library(colorspace)
library(stringr)

sourceDirectory("R", modifiedOnly=FALSE)

foodweb <- "Benguela Pelagic"
dirnam <- "rN=10000_tol=1_connectance_prop"

fw_data <- readRDS(paste("results/rejection/", foodweb, "/", dirnam, "/", foodweb, ".Rdata", sep = ""))

order_index_dist <- order(fw_data$dist)
sort_fw_dist <- sort(fw_data$dist)
till <- 5000
tol <- sort_fw_dist[till]

sub_index <- which(fw_data$dist <= tol)
sub_index <- sub_index[1:till]
  
sub_fw_data <- list(acc_ss = fw_data$acc_ss[sub_index], dist = fw_data$dist[sub_index], 
                       post_dists = fw_data$post_dists[sub_index,], total_sim = till)

dirnam_sub <- "rN=5000_tol=1_connectance_prop"
dirnam_sub <- paste("results/rejection/", foodweb, "/", dirnam_sub, sep = "")
dir.create(dirnam_sub)
fname <- paste(dirnam_sub, "/", foodweb, ".Rdata", sep = "")
saveRDS(sub_fw_data, file = fname)

foodweb_data <- load(paste("data/", foodweb, ".web.Rdata", sep=""))
all.web.info <- get(foodweb_data)
model_core_par <- ADBM_core_par(all.web.info)
model_prior_par <- ADBM_prior_par(all.web.info)
model <- ratio.power_exp
desc <- "conn_prop_rall"
prior_dist = prior_unif_x

## Plotting the real and predicted food web matrix alongwith some food web properties
plot_foodweb(real_foodweb = all.web.info, predicted_foodweb = sub_fw_data,
             dirnam = dirnam_sub, model_core_par = model_core_par, 
             model_prior_par = model_prior_par,
             model = model, desc=desc, prior_dist_x = prior_dist, web.to.analyse = foodweb)
