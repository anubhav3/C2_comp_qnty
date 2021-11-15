# 31.05.2021
# Plot model mse vs empirical mse


########### Broadstone Stream size_agg_v2

propn <- c(8, 28, 48, 68, 88, 108, 208, 308, 408, 508, 808, 1008)
desc_main <- "TSS_gut_ind"
rule <- "ind_predator"
n_gut_sample <- 100
dir_N <- 1e5
dir_tol <- Inf

prop_name <- c("connectance", "prop_basal", "prop_inter", "prop_top", "prop_herb",
               "mean_omn", "clus_coeff", "sd_gen", "sd_vulner", "mean_max_trophic_sim", "mean_path_lt",
               "nest_prop")

n_prop <- length(prop_name)
plot_list <- list()

# Computing the max SE for a property (ADBM)
max_error <- numeric(n_prop)
min_error <- numeric(n_prop)

fw_name <- "Broadstone Stream size_agg_v2"

foodweb_data <- readRDS(paste("data/", fw_name, ".web.Rdata", sep=""))
real_prop_org <- real_prop_predator(foodweb_data)

SE_vs_ngut <- data.frame(l_SE = double(), u_SE = double(), mean_SE = double(),
                         ngut = integer(),
                         foodweb = character())

for(ngut in propn){
  
  desc <- paste(desc_main, '_l', ngut, sep = '')
  dir_main <- paste("../../../PhD_local/C2_files/", fw_name, "/rule_", rule ,'/N=', dir_N, '_tol=', dir_tol, '_n_diet = ', ngut,
                    sep = "")
  fname <- paste(dir_main, "/", fw_name, "_prop.Rdata", sep = "")
  
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
    min_error[i] <- min(min_error_temp, min_error[i])
  }
  
  print(ngut)
}

# Computing the max SE for a property (data)
max_error_data <- numeric(n_prop)
min_error_data <- numeric(n_prop)

fw_name <- "Broadstone Stream size_agg_v2"

foodweb_data <- readRDS(paste("data/", fw_name, ".web.Rdata", sep=""))
real_prop_org <- real_prop_predator(foodweb_data)

SE_vs_ngut <- data.frame(l_SE = double(), u_SE = double(), mean_SE = double(),
                         ngut = integer(),
                         foodweb = character())

for(ngut in propn){
  
  desc <- paste(desc_main, '_l', ngut, sep = '')
  dir_main <- paste("../../../PhD_local/C2_files/", fw_name, "/rule_data/", 'n_diet=', ngut,
                    sep = "")
  fname <- paste(dir_main, "/", fw_name, "_prop.Rdata", sep = "")
  
  prop_data_org <- readRDS(fname)
  prop_all <- prop_data_org
  real_prop <- real_prop_org[prop_name]
  
  err_mat <- prop_all[prop_name]
  
  for(i in 1:n_gut_sample){
    err_mat[i,] <- abs(err_mat[i,] - real_prop)
  }
  
  for(i in 1:n_prop){
    max_error_temp <- max(as.numeric(na.omit(err_mat[,i])))
    print(max_error_temp)
    max_error_data[i] <- max(max_error_temp, max_error_data[i])
    
    min_error_temp <- min(as.numeric(na.omit(err_mat[,i])))
    min_error_data[i] <- min(min_error_temp, min_error_data[i])
  }
  
  print(ngut)
}

for(i in 1:n_prop){
  max_error[i] <- max(max_error[i], max_error_data[i])
}
  
# Computing the standardised error (ADBM)

SE_vs_ngut_model <- data.frame(l_SE = double(), u_SE = double(), mean_SE = double(),
                         ngut = integer(),
                         foodweb = character())

for(ngut in propn){
  
  desc <- paste(desc_main, '_l', ngut, sep = '')
  dir_main <- paste("../../../PhD_local/C2_files/", fw_name, "/rule_", rule ,'/N=', dir_N, '_tol=', dir_tol, '_n_diet = ', ngut,
                    sep = "")
  fname <- paste(dir_main, "/", fw_name, "_prop.Rdata", sep = "")
  
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
  
  SE_vs_ngut_model <- rbind(SE_vs_ngut_model, SE_vs_ngut_temp)
}


# Computing the standardised error (data)

SE_vs_ngut_data <- data.frame(l_SE = double(), u_SE = double(), mean_SE = double(),
                               ngut = integer(),
                               foodweb = character())

for(ngut in propn){
  
  desc <- paste(desc_main, '_l', ngut, sep = '')
  dir_main <- paste("../../../PhD_local/C2_files/", fw_name, "/rule_data/", 'n_diet=', ngut,
                    sep = "")
  fname <- paste(dir_main, "/", fw_name, "_prop.Rdata", sep = "")
  
  prop_data_org <- readRDS(fname)
  prop_all <- prop_data_org
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
  
  SE_vs_ngut_data <- rbind(SE_vs_ngut_data, SE_vs_ngut_temp)
}



mse_model_empirical_broadstone_stream <- data.frame(model_mse = SE_vs_ngut_model$mean_SE, data_mse = SE_vs_ngut_data$mean_SE, ngut = SE_vs_ngut_data$ngut,
                                  l_model_mse = SE_vs_ngut_model$l_SE, u_model_mse = SE_vs_ngut_model$u_SE,
                                  l_data_mse = SE_vs_ngut_data$l_SE, u_data_mse = SE_vs_ngut_data$u_SE)


plot_mse_model_empirical_broadstone_stream_size_agg_v2 <- 
  mse_model_empirical_broadstone_stream %>%
  ggplot() +
  geom_point(aes(x = data_mse, y = model_mse,  color = as.factor(ngut)), size = 4) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  xlim(c(0,1)) +
  ylim(c(0,1)) +
  xlab("Mean standardised error (Gut content data)") +
  ylab("Mean standardised error (ADBM and Gut content data)") +
  theme_classic() +
  labs(title = "Broadstone Stream", tag = "(a)") +
  theme(plot.title = element_text(hjust=0.5), plot.tag = element_text(face = "bold")) +
  scale_color_brewer(name = "Amount of guts", palette = "Paired", labels = c("1%", "3%", "5%", "7%", "9%",
                                                                                 "11%", "21%", "31%", "40%", "50%",
                                                                                 "80%", "100%"))+
  geom_linerange(aes(x = data_mse, y = model_mse, ymin = l_model_mse, ymax = u_model_mse), alpha = 0.5) +
  geom_linerange(aes(x = data_mse, y = model_mse, xmin = l_data_mse, xmax = u_data_mse), alpha = 0.5)
  


# 31.05.2021
# Plot model mse vs empirical mse


########### Celtic Sea size_agg

propn <- c(11, 31, 51, 71, 91, 111, 151, 171, 231, 331, 431, 491)
desc_main <- "TSS_gut_ind"
rule <- "ind_predator"
n_gut_sample <- 100
dir_N <- 1e5
dir_tol <- Inf

prop_name <- c("connectance", "prop_basal", "prop_inter", "prop_top", "prop_herb",
               "mean_omn", "clus_coeff", "sd_gen", "sd_vulner", "mean_max_trophic_sim", "mean_path_lt",
               "nest_prop")

n_prop <- length(prop_name)
plot_list <- list()

# Computing the max SE for a property (ADBM)
max_error <- numeric(n_prop)
min_error <- numeric(n_prop)

fw_name <- "Celtic Sea size_agg"

foodweb_data <- readRDS(paste("data/", fw_name, ".web.Rdata", sep=""))
real_prop_org <- real_prop_predator(foodweb_data)

SE_vs_ngut <- data.frame(l_SE = double(), u_SE = double(), mean_SE = double(),
                         ngut = integer(),
                         foodweb = character())

for(ngut in propn){
  
  desc <- paste(desc_main, '_l', ngut, sep = '')
  dir_main <- paste("../../../PhD_local/C2_files/", fw_name, "/rule_", rule ,'/N=', dir_N, '_tol=', dir_tol, '_n_diet = ', ngut,
                    sep = "")
  fname <- paste(dir_main, "/", fw_name, "_prop.Rdata", sep = "")
  
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
    min_error[i] <- min(min_error_temp, min_error[i])
  }
  
  print(ngut)
}

# Computing the max SE for a property (data)
max_error_data <- numeric(n_prop)
min_error_data <- numeric(n_prop)

fw_name <- "Celtic Sea size_agg"

foodweb_data <- readRDS(paste("data/", fw_name, ".web.Rdata", sep=""))
real_prop_org <- real_prop_predator(foodweb_data)

SE_vs_ngut <- data.frame(l_SE = double(), u_SE = double(), mean_SE = double(),
                         ngut = integer(),
                         foodweb = character())

for(ngut in propn){
  
  desc <- paste(desc_main, '_l', ngut, sep = '')
  dir_main <- paste("../../../PhD_local/C2_files/", fw_name, "/rule_data/", 'n_diet=', ngut,
                    sep = "")
  fname <- paste(dir_main, "/", fw_name, "_prop.Rdata", sep = "")
  
  prop_data_org <- readRDS(fname)
  prop_all <- prop_data_org
  real_prop <- real_prop_org[prop_name]
  
  err_mat <- prop_all[prop_name]
  
  for(i in 1:n_gut_sample){
    err_mat[i,] <- abs(err_mat[i,] - real_prop)
  }
  
  for(i in 1:n_prop){
    max_error_temp <- max(as.numeric(na.omit(err_mat[,i])))
    print(max_error_temp)
    max_error_data[i] <- max(max_error_temp, max_error_data[i])
    
    min_error_temp <- min(as.numeric(na.omit(err_mat[,i])))
    min_error_data[i] <- min(min_error_temp, min_error_data[i])
  }
  
  print(ngut)
}

for(i in 1:n_prop){
  max_error[i] <- max(max_error[i], max_error_data[i])
}

# Computing the standardised error (ADBM)

SE_vs_ngut_model <- data.frame(l_SE = double(), u_SE = double(), mean_SE = double(),
                               ngut = integer(),
                               foodweb = character())

for(ngut in propn){
  
  desc <- paste(desc_main, '_l', ngut, sep = '')
  dir_main <- paste("../../../PhD_local/C2_files/", fw_name, "/rule_", rule ,'/N=', dir_N, '_tol=', dir_tol, '_n_diet = ', ngut,
                    sep = "")
  fname <- paste(dir_main, "/", fw_name, "_prop.Rdata", sep = "")
  
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
  
  SE_vs_ngut_model <- rbind(SE_vs_ngut_model, SE_vs_ngut_temp)
}


# Computing the standardised error (data)

SE_vs_ngut_data <- data.frame(l_SE = double(), u_SE = double(), mean_SE = double(),
                              ngut = integer(),
                              foodweb = character())

for(ngut in propn){
  
  desc <- paste(desc_main, '_l', ngut, sep = '')
  dir_main <- paste("../../../PhD_local/C2_files/", fw_name, "/rule_data/", 'n_diet=', ngut,
                    sep = "")
  fname <- paste(dir_main, "/", fw_name, "_prop.Rdata", sep = "")
  
  prop_data_org <- readRDS(fname)
  prop_all <- prop_data_org
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
  
  SE_vs_ngut_data <- rbind(SE_vs_ngut_data, SE_vs_ngut_temp)
}



mse_model_empirical_celtic_sea <- data.frame(model_mse = SE_vs_ngut_model$mean_SE, data_mse = SE_vs_ngut_data$mean_SE, ngut = SE_vs_ngut_data$ngut,
                                                    l_model_mse = SE_vs_ngut_model$l_SE, u_model_mse = SE_vs_ngut_model$u_SE,
                                                    l_data_mse = SE_vs_ngut_data$l_SE, u_data_mse = SE_vs_ngut_data$u_SE)


plot_mse_model_empirical_celtic_sea <- 
  mse_model_empirical_celtic_sea %>%
  ggplot() +
  geom_point(aes(x = data_mse, y = model_mse,  color = as.factor(ngut)), size = 4) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  xlim(c(0,1)) +
  ylim(c(0,1)) +
  xlab("Mean standardised error (Gut content data)") +
  ylab("Mean standardised error (ADBM and Gut content data)") +
  theme_classic() +
  labs(title = "Celtic Sea", tag = "(b)") +
  theme(plot.title = element_text(hjust=0.5), plot.tag = element_text(face = "bold")) +
  scale_color_brewer(name = "Amount of guts", palette = "Paired", labels = c("2%", "6%", "10%", "14%", "18%",
                                                                                 "22%", "30%", "35%", "47%", "67%",
                                                                                 "88%", "100%"))+
  geom_linerange(aes(x = data_mse, y = model_mse, ymin = l_model_mse, ymax = u_model_mse), alpha = 0.5) +
  geom_linerange(aes(x = data_mse, y = model_mse, xmin = l_data_mse, xmax = u_data_mse), alpha = 0.5)


plot_mse_model_empirical <- ggarrange(plot_mse_model_empirical_broadstone_stream_size_agg_v2, plot_mse_model_empirical_celtic_sea,
                                      nrow = 1, ncol = 2)
# ggsave(filename = "results/misc/mse_model_vs_data.png", plot = plot_mse_model_empirical,
#       width = 11, height = 4)
