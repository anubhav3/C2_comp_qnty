# 04.05.2021
# Computes mean standardised error in structural properties wrt ngut

# 28.01.2022
# Computes mean standardised error in structural properties wrt ngut for all food webs

library(plotrix); library(ggplot2)
library(cowplot); library(latex2exp); library(HDInterval)
library(doParallel); library(foreach); library(raster)
library(cheddar); library(dplyr); library(reshape2)
library(psych); library(DirectedClustering); library(colorspace)
library(R.utils)
library(cheddar)
library(bipartite)

sourceDirectory("R", modifiedOnly=FALSE)
prop_name <- c("connectance", "prop_basal", "prop_inter", "prop_top", "prop_herb",
               "mean_omn", "clus_coeff", "sd_gen", "sd_vulner", "mean_max_trophic_sim", "mean_path_lt",
               "nest_prop")


#### For Broadstone Stream size_agg_v2 ####

propn <- c(seq(1, 1008, by = 20), 1008)
desc_main <- "TSS_gut_ind"
rule <- "ind_predator"
n_gut_sample <- 100
dir_N <- 1e5
dir_tol <- 2

n_prop <- length(prop_name)
plot_list <- list()

# Computing the max SE for a property
max_error <- numeric(n_prop)
min_error <- numeric(n_prop)

fw_name <- "Broadstone Stream size_agg_v2"

foodweb_data <- readRDS(paste("data/", fw_name, ".web.Rdata", sep=""))
real_prop_org <- real_prop_predator(foodweb_data)

SE_vs_ngut <- data.frame(l_SE = double(), u_SE = double(), mean_SE = double(),
                        ngut = integer(),
                        foodweb = character(), foodweb = character())

for(ngut in propn){
  
  desc <- paste(desc_main, '_l', ngut, sep = '')
  dir_main <- paste("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_", rule ,'/accepted_par/properties/', fw_name, '_n_diet=', ngut,
                    "_properties.Rdata", sep = "")
  fname <- dir_main
  
  prop_data_org <- readRDS(fname)
  prop_all <- prop_data_org$prop
  real_prop <- real_prop_org[prop_name]
  
  err_mat <- prop_all[prop_name]
  
  for(i in 1:n_gut_sample){
    err_mat[i,] <- abs(err_mat[i,] - real_prop)
  }
  
  for(i in 1:n_prop){
    max_error_temp <- max(as.numeric(na.omit(err_mat[,i])))
    print(max_error_temp)
    max_error[i] <- max(max_error_temp, max_error[i])
    
    min_error_temp <- min(as.numeric(na.omit(err_mat[,i])))
    min_error[i] <- max(min_error_temp, min_error[i])
  }
  
  print(ngut)
}


# Computing the standardised error
for(ngut in propn){

  desc <- paste(desc_main, '_l', ngut, sep = '')
  dir_main <- paste("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_", rule ,'/accepted_par/properties/', fw_name, '_n_diet=', ngut,
                    "_properties.Rdata", sep = "")
  fname <- dir_main
  
  prop_data_org <- readRDS(fname)
  prop_all <- prop_data_org$prop
  real_prop <- real_prop_org[prop_name]
  
  err_mat <- prop_all[prop_name]
  
  for(i in 1:n_gut_sample){
    err_mat[i,] <- abs(err_mat[i,] - real_prop)
  }
  
  for(i in 1:n_prop){
    err_mat[,i] <- err_mat[,i]/max_error[i]
  }
  
  
  mean_err <- rowMeans(err_mat, na.rm = TRUE)
  
  
  CI_err <- hdi(mean_err, credMass = 0.95)
  
  SE_vs_ngut_temp <- data.frame(l_SE = as.numeric(CI_err[1]), u_SE = as.numeric(CI_err[2]), mean_SE = mean(mean_err),
                               ngut = ngut,
                               foodweb = fw_name)
  
  SE_vs_ngut <- rbind(SE_vs_ngut, SE_vs_ngut_temp)
}


plot_SE_vs_ngut_bs <- ggplot(SE_vs_ngut) +
  geom_line(aes(x = ngut, y = mean_SE)) +
  geom_ribbon(aes(x = ngut, ymin = l_SE, ymax = u_SE), alpha = 0.5) +
  geom_abline(slope = 0, intercept = 0, color = "red") +
  theme_classic() +
  xlab("Number of distinct predator gut content data") +
  ylab("Mean standardised error")


#### For Celtic Sea ####

propn <- seq(1, 491, by = 10)
desc_main <- "TSS_gut_ind"
rule <- "ind_predator"
n_gut_sample <- 100
dir_N <- 1e5
dir_tol <- 2

n_prop <- length(prop_name)
plot_list <- list()

# Computing the max SE for a property
max_error <- numeric(n_prop)
min_error <- numeric(n_prop)

fw_name <- "Celtic Sea size_agg"

foodweb_data <- readRDS(paste("data/", fw_name, ".web.Rdata", sep=""))
real_prop_org <- real_prop_predator(foodweb_data)

for(ngut in propn){
  
  desc <- paste(desc_main, '_l', ngut, sep = '')
  dir_main <- paste("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_", rule ,'/accepted_par/properties/', fw_name, '_n_diet=', ngut,
                    "_properties.Rdata", sep = "")
  fname <- dir_main
  
  prop_data_org <- readRDS(fname)
  prop_all <- prop_data_org$prop
  real_prop <- real_prop_org[prop_name]
  
  err_mat <- prop_all[prop_name]
  
  for(i in 1:n_gut_sample){
    err_mat[i,] <- abs(err_mat[i,] - real_prop)
  }
  
  for(i in 1:n_prop){
    max_error_temp <- max(as.numeric(na.omit(err_mat[,i])))
    print(max_error_temp)
    max_error[i] <- max(max_error_temp, max_error[i])
    
    min_error_temp <- min(as.numeric(na.omit(err_mat[,i])))
    min_error[i] <- max(min_error_temp, min_error[i])
  }
  
  print(ngut)
}


# Computing the standardised error
for(ngut in propn){
  
  desc <- paste(desc_main, '_l', ngut, sep = '')
  dir_main <- paste("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_", rule ,'/accepted_par/properties/', fw_name, '_n_diet=', ngut,
                    "_properties.Rdata", sep = "")
  fname <- dir_main
  
  prop_data_org <- readRDS(fname)
  prop_all <- prop_data_org$prop
  real_prop <- real_prop_org[prop_name]
  
  err_mat <- prop_all[prop_name]
  
  for(i in 1:n_gut_sample){
    err_mat[i,] <- abs(err_mat[i,] - real_prop)
  }
  
  for(i in 1:n_prop){
    err_mat[,i] <- err_mat[,i]/max_error[i]
  }
  
  
  mean_err <- rowMeans(err_mat, na.rm = TRUE)
  
  
  CI_err <- hdi(mean_err, credMass = 0.95)
  
  SE_vs_ngut_temp <- data.frame(l_SE = as.numeric(CI_err[1]), u_SE = as.numeric(CI_err[2]), mean_SE = mean(mean_err),
                                ngut = ngut,
                                foodweb = fw_name)
  
  SE_vs_ngut <- rbind(SE_vs_ngut, SE_vs_ngut_temp)
}


plot_SE_vs_ngut_cs <- ggplot(SE_vs_ngut) +
  geom_line(aes(x = ngut, y = mean_SE)) +
  geom_ribbon(aes(x = ngut, ymin = l_SE, ymax = u_SE), alpha = 0.5) +
  geom_abline(slope = 0, intercept = 0, color = "red") +
  theme_classic() +
  xlab("Number of distinct predator gut content data") +
  ylab("Mean standardised error")

#### For Tadnoll Brook ####

propn <- c(seq(1, 688, by = 14), 688)
desc_main <- "TSS_gut_ind"
rule <- "ind_predator"
n_gut_sample <- 100
dir_N <- 1e5
dir_tol <- 2

n_prop <- length(prop_name)
plot_list <- list()

# Computing the max SE for a property
max_error <- numeric(n_prop)
min_error <- numeric(n_prop)

fw_name <- "Tadnoll Brook size_agg"

foodweb_data <- readRDS(paste("data/", fw_name, ".web.Rdata", sep=""))
real_prop_org <- real_prop_predator(foodweb_data)


for(ngut in propn){
  
  desc <- paste(desc_main, '_l', ngut, sep = '')
  dir_main <- paste("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_", rule ,'/accepted_par/properties/', fw_name, '_n_diet=', ngut,
                    "_properties.Rdata", sep = "")
  fname <- dir_main
  
  prop_data_org <- readRDS(fname)
  prop_all <- prop_data_org$prop
  real_prop <- real_prop_org[prop_name]
  
  err_mat <- prop_all[prop_name]
  
  for(i in 1:n_gut_sample){
    err_mat[i,] <- abs(err_mat[i,] - real_prop)
  }
  
  for(i in 1:n_prop){
    max_error_temp <- max(as.numeric(na.omit(err_mat[,i])))
    print(max_error_temp)
    max_error[i] <- max(max_error_temp, max_error[i])
    
    min_error_temp <- min(as.numeric(na.omit(err_mat[,i])))
    min_error[i] <- max(min_error_temp, min_error[i])
  }
  
  print(ngut)
}


# Computing the standardised error
for(ngut in propn){
  
  desc <- paste(desc_main, '_l', ngut, sep = '')
  dir_main <- paste("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_", rule ,'/accepted_par/properties/', fw_name, '_n_diet=', ngut,
                    "_properties.Rdata", sep = "")
  fname <- dir_main
  
  prop_data_org <- readRDS(fname)
  prop_all <- prop_data_org$prop
  real_prop <- real_prop_org[prop_name]
  
  err_mat <- prop_all[prop_name]
  
  for(i in 1:n_gut_sample){
    err_mat[i,] <- abs(err_mat[i,] - real_prop)
  }
  
  for(i in 1:n_prop){
    err_mat[,i] <- err_mat[,i]/max_error[i]
  }
  
  
  mean_err <- rowMeans(err_mat, na.rm = TRUE)
  
  
  CI_err <- hdi(mean_err, credMass = 0.95)
  
  SE_vs_ngut_temp <- data.frame(l_SE = as.numeric(CI_err[1]), u_SE = as.numeric(CI_err[2]), mean_SE = mean(mean_err),
                                ngut = ngut,
                                foodweb = fw_name)
  
  SE_vs_ngut <- rbind(SE_vs_ngut, SE_vs_ngut_temp)
}


plot_SE_vs_ngut_tb <- ggplot(SE_vs_ngut) +
  geom_line(aes(x = ngut, y = mean_SE)) +
  geom_ribbon(aes(x = ngut, ymin = l_SE, ymax = u_SE), alpha = 0.5) +
  geom_abline(slope = 0, intercept = 0, color = "red") +
  theme_classic() +
  xlab("Number of distinct predator gut content data") +
  ylab("Mean standardised error")


#### For Afon Hirnant ####

propn <- setdiff(c(seq(1, 175, by = 4), 175), c(173))
desc_main <- "TSS_gut_ind"
rule <- "ind_predator"
n_gut_sample <- 100
dir_N <- 1e5
dir_tol <- 2

n_prop <- length(prop_name)
plot_list <- list()

# Computing the max SE for a property
max_error <- numeric(n_prop)
min_error <- numeric(n_prop)

fw_name <- "Afon Hirnant size_agg"

foodweb_data <- readRDS(paste("data/", fw_name, ".web.Rdata", sep=""))
real_prop_org <- real_prop_predator(foodweb_data)


for(ngut in propn){
  
  desc <- paste(desc_main, '_l', ngut, sep = '')
  dir_main <- paste("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_", rule ,'/accepted_par/properties/', fw_name, '_n_diet=', ngut,
                    "_properties.Rdata", sep = "")
  fname <- dir_main
  
  prop_data_org <- readRDS(fname)
  prop_all <- prop_data_org$prop
  real_prop <- real_prop_org[prop_name]
  
  err_mat <- prop_all[prop_name]
  
  for(i in 1:n_gut_sample){
    err_mat[i,] <- abs(err_mat[i,] - real_prop)
  }
  
  for(i in 1:n_prop){
    max_error_temp <- max(as.numeric(na.omit(err_mat[,i])))
    print(max_error_temp)
    max_error[i] <- max(max_error_temp, max_error[i])
    
    min_error_temp <- min(as.numeric(na.omit(err_mat[,i])))
    min_error[i] <- max(min_error_temp, min_error[i])
  }
  
  print(ngut)
}


# Computing the standardised error
for(ngut in propn){
  
  desc <- paste(desc_main, '_l', ngut, sep = '')
  dir_main <- paste("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_", rule ,'/accepted_par/properties/', fw_name, '_n_diet=', ngut,
                    "_properties.Rdata", sep = "")
  fname <- dir_main
  
  prop_data_org <- readRDS(fname)
  prop_all <- prop_data_org$prop
  real_prop <- real_prop_org[prop_name]
  
  err_mat <- prop_all[prop_name]
  
  for(i in 1:n_gut_sample){
    err_mat[i,] <- abs(err_mat[i,] - real_prop)
  }
  
  for(i in 1:n_prop){
    err_mat[,i] <- err_mat[,i]/max_error[i]
  }
  
  
  mean_err <- rowMeans(err_mat, na.rm = TRUE)
  
  
  CI_err <- hdi(mean_err, credMass = 0.95)
  
  SE_vs_ngut_temp <- data.frame(l_SE = as.numeric(CI_err[1]), u_SE = as.numeric(CI_err[2]), mean_SE = mean(mean_err),
                                ngut = ngut,
                                foodweb = fw_name)
  
  SE_vs_ngut <- rbind(SE_vs_ngut, SE_vs_ngut_temp)
}


plot_SE_vs_ngut_ah <- ggplot(SE_vs_ngut) +
  geom_line(aes(x = ngut, y = mean_SE)) +
  geom_ribbon(aes(x = ngut, ymin = l_SE, ymax = u_SE), alpha = 0.5) +
  geom_abline(slope = 0, intercept = 0, color = "red") +
  theme_classic() +
  xlab("Number of distinct predator gut content data") +
  ylab("Mean standardised error")


#### For Coilaco ####

propn <- c(seq(1, 74, by = 3), 74)
desc_main <- "TSS_gut_ind"
rule <- "ind_predator"
n_gut_sample <- 100
dir_N <- 1e5
dir_tol <- 2

n_prop <- length(prop_name)
plot_list <- list()

# Computing the max SE for a property
max_error <- numeric(n_prop)
min_error <- numeric(n_prop)

fw_name <- "Coilaco size_agg"

foodweb_data <- readRDS(paste("data/", fw_name, ".web.Rdata", sep=""))
real_prop_org <- real_prop_predator(foodweb_data)

for(ngut in propn){
  
  desc <- paste(desc_main, '_l', ngut, sep = '')
  dir_main <- paste("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_", rule ,'/accepted_par/properties/', fw_name, '_n_diet=', ngut,
                    "_properties.Rdata", sep = "")
  fname <- dir_main
  
  prop_data_org <- readRDS(fname)
  prop_all <- prop_data_org$prop
  real_prop <- real_prop_org[prop_name]
  
  err_mat <- prop_all[prop_name]
  
  for(i in 1:n_gut_sample){
    err_mat[i,] <- abs(err_mat[i,] - real_prop)
  }
  
  for(i in 1:n_prop){
    max_error_temp <- max(as.numeric(na.omit(err_mat[,i])))
    print(max_error_temp)
    max_error[i] <- max(max_error_temp, max_error[i])
    
    min_error_temp <- min(as.numeric(na.omit(err_mat[,i])))
    min_error[i] <- max(min_error_temp, min_error[i])
  }
  
  print(ngut)
}


# Computing the standardised error
for(ngut in propn){
  
  desc <- paste(desc_main, '_l', ngut, sep = '')
  dir_main <- paste("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_", rule ,'/accepted_par/properties/', fw_name, '_n_diet=', ngut,
                    "_properties.Rdata", sep = "")
  fname <- dir_main
  
  prop_data_org <- readRDS(fname)
  prop_all <- prop_data_org$prop
  real_prop <- real_prop_org[prop_name]
  
  err_mat <- prop_all[prop_name]
  
  for(i in 1:n_gut_sample){
    err_mat[i,] <- abs(err_mat[i,] - real_prop)
  }
  
  for(i in 1:n_prop){
    err_mat[,i] <- err_mat[,i]/max_error[i]
  }
  
  
  mean_err <- rowMeans(err_mat, na.rm = TRUE)
  
  
  CI_err <- hdi(mean_err, credMass = 0.95)
  
  SE_vs_ngut_temp <- data.frame(l_SE = as.numeric(CI_err[1]), u_SE = as.numeric(CI_err[2]), mean_SE = mean(mean_err),
                                ngut = ngut,
                                foodweb = fw_name)
  
  SE_vs_ngut <- rbind(SE_vs_ngut, SE_vs_ngut_temp)
}


plot_SE_vs_ngut_co <- ggplot(SE_vs_ngut) +
  geom_line(aes(x = ngut, y = mean_SE)) +
  geom_ribbon(aes(x = ngut, ymin = l_SE, ymax = u_SE), alpha = 0.5) +
  geom_abline(slope = 0, intercept = 0, color = "red") +
  theme_classic() +
  xlab("Number of distinct predator gut content data") +
  ylab("Mean standardised error")

#### For Trancura ####

propn <- seq(1, 47, by = 2)
desc_main <- "TSS_gut_ind"
rule <- "ind_predator"
n_gut_sample <- 100
dir_N <- 1e5
dir_tol <- 2

n_prop <- length(prop_name)
plot_list <- list()

# Computing the max SE for a property
max_error <- numeric(n_prop)
min_error <- numeric(n_prop)

fw_name <- "Trancura size_agg"

foodweb_data <- readRDS(paste("data/", fw_name, ".web.Rdata", sep=""))
real_prop_org <- real_prop_predator(foodweb_data)


for(ngut in propn){
  
  desc <- paste(desc_main, '_l', ngut, sep = '')
  dir_main <- paste("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_", rule ,'/accepted_par/properties/', fw_name, '_n_diet=', ngut,
                    "_properties.Rdata", sep = "")
  fname <- dir_main
  
  prop_data_org <- readRDS(fname)
  prop_all <- prop_data_org$prop
  real_prop <- real_prop_org[prop_name]
  
  err_mat <- prop_all[prop_name]
  
  for(i in 1:n_gut_sample){
    err_mat[i,] <- abs(err_mat[i,] - real_prop)
  }
  
  for(i in 1:n_prop){
    max_error_temp <- max(as.numeric(na.omit(err_mat[,i])))
    print(max_error_temp)
    max_error[i] <- max(max_error_temp, max_error[i])
    
    min_error_temp <- min(as.numeric(na.omit(err_mat[,i])))
    min_error[i] <- max(min_error_temp, min_error[i])
  }
  
  print(ngut)
}


# Computing the standardised error
for(ngut in propn){
  
  desc <- paste(desc_main, '_l', ngut, sep = '')
  dir_main <- paste("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_", rule ,'/accepted_par/properties/', fw_name, '_n_diet=', ngut,
                    "_properties.Rdata", sep = "")
  fname <- dir_main
  
  prop_data_org <- readRDS(fname)
  prop_all <- prop_data_org$prop
  real_prop <- real_prop_org[prop_name]
  
  err_mat <- prop_all[prop_name]
  
  for(i in 1:n_gut_sample){
    err_mat[i,] <- abs(err_mat[i,] - real_prop)
  }
  
  for(i in 1:n_prop){
    err_mat[,i] <- err_mat[,i]/max_error[i]
  }
  
  
  mean_err <- rowMeans(err_mat, na.rm = TRUE)
  
  
  CI_err <- hdi(mean_err, credMass = 0.95)
  
  SE_vs_ngut_temp <- data.frame(l_SE = as.numeric(CI_err[1]), u_SE = as.numeric(CI_err[2]), mean_SE = mean(mean_err),
                                ngut = ngut,
                                foodweb = fw_name)
  
  SE_vs_ngut <- rbind(SE_vs_ngut, SE_vs_ngut_temp)
}


plot_SE_vs_ngut_tr <- ggplot(SE_vs_ngut) +
  geom_line(aes(x = ngut, y = mean_SE)) +
  geom_ribbon(aes(x = ngut, ymin = l_SE, ymax = u_SE), alpha = 0.5) +
  geom_abline(slope = 0, intercept = 0, color = "red") +
  theme_classic() +
  xlab("Number of distinct predator gut content data") +
  ylab("Mean standardised error")


#### For Guampoe ####

propn <- setdiff(c(seq(1, 87, by = 3), 87), c(85,87))
desc_main <- "TSS_gut_ind"
rule <- "ind_predator"
n_gut_sample <- 100
dir_N <- 1e5
dir_tol <- 2

n_prop <- length(prop_name)
plot_list <- list()

# Computing the max SE for a property
max_error <- numeric(n_prop)
min_error <- numeric(n_prop)

fw_name <- "Guampoe size_agg"

foodweb_data <- readRDS(paste("data/", fw_name, ".web.Rdata", sep=""))
real_prop_org <- real_prop_predator(foodweb_data)


for(ngut in propn){
  
  desc <- paste(desc_main, '_l', ngut, sep = '')
  dir_main <- paste("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_", rule ,'/accepted_par/properties/', fw_name, '_n_diet=', ngut,
                    "_properties.Rdata", sep = "")
  fname <- dir_main
  
  prop_data_org <- readRDS(fname)
  prop_all <- prop_data_org$prop
  real_prop <- real_prop_org[prop_name]
  
  err_mat <- prop_all[prop_name]
  
  for(i in 1:n_gut_sample){
    err_mat[i,] <- abs(err_mat[i,] - real_prop)
  }
  
  for(i in 1:n_prop){
    max_error_temp <- max(as.numeric(na.omit(err_mat[,i])))
    print(max_error_temp)
    max_error[i] <- max(max_error_temp, max_error[i])
    
    min_error_temp <- min(as.numeric(na.omit(err_mat[,i])))
    min_error[i] <- max(min_error_temp, min_error[i])
  }
  
  print(ngut)
}


# Computing the standardised error
for(ngut in propn){
  
  desc <- paste(desc_main, '_l', ngut, sep = '')
  dir_main <- paste("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_", rule ,'/accepted_par/properties/', fw_name, '_n_diet=', ngut,
                    "_properties.Rdata", sep = "")
  fname <- dir_main
  
  prop_data_org <- readRDS(fname)
  prop_all <- prop_data_org$prop
  real_prop <- real_prop_org[prop_name]
  
  err_mat <- prop_all[prop_name]
  
  for(i in 1:n_gut_sample){
    err_mat[i,] <- abs(err_mat[i,] - real_prop)
  }
  
  for(i in 1:n_prop){
    err_mat[,i] <- err_mat[,i]/max_error[i]
  }
  
  
  mean_err <- rowMeans(err_mat, na.rm = TRUE)
  
  
  CI_err <- hdi(mean_err, credMass = 0.95)
  
  SE_vs_ngut_temp <- data.frame(l_SE = as.numeric(CI_err[1]), u_SE = as.numeric(CI_err[2]), mean_SE = mean(mean_err),
                                ngut = ngut,
                                foodweb = fw_name)
  
  SE_vs_ngut <- rbind(SE_vs_ngut, SE_vs_ngut_temp)
}


plot_SE_vs_ngut_gu <- ggplot(SE_vs_ngut) +
  geom_line(aes(x = ngut, y = mean_SE)) +
  geom_ribbon(aes(x = ngut, ymin = l_SE, ymax = u_SE), alpha = 0.5) +
  geom_abline(slope = 0, intercept = 0, color = "red") +
  theme_classic() +
  xlab("Number of distinct predator gut content data") +
  ylab("Mean standardised error")




###### Plotting all the food webs ######
leg_lab <- c("Broadstone Stream size_agg_v2" = "Broadstone Stream",
             "Celtic Sea size_agg" = "Celtic Sea",
             "Tadnoll Brook size_agg" = "Tadnoll Brook",
             "Afon Hirnant size_agg" = "Afon Hirnant",
             "Coilaco size_agg" = "Coilaco",
             "Guampoe size_agg" = "Guampoe",
             "Trancura size_agg" = "Trancura")


plot_mse_ngut_all <- ggplot(SE_vs_ngut) +
  geom_line(aes(x = ngut, y = mean_SE)) +
  geom_ribbon(aes(x = ngut, ymin = l_SE, ymax = u_SE), alpha = 0.5) +
  facet_wrap(~foodweb, scales = "free_x", labeller = as_labeller(leg_lab)) +
  theme_classic() +
  xlab("Number of distinct predator gut content data") +
  ylab("Mean standardised error")

# ggsave(filename = "results/misc/plot_mse_ngut_all.png", plot = plot_mse_ngut_all)
