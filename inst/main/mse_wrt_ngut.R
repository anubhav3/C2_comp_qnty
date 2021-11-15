# 04.05.2021
# Computes mean standardised error in structural properties wrt ngut

propn <- c(seq(481, 1008, by = 20), 1008)
desc_main <- "TSS_gut_ind"
rule <- "ind_predator"
n_gut_sample <- 100
dir_N <- 1e5
dir_tol <- 2

prop_name <- c("connectance", "prop_basal", "prop_inter", "prop_top", "prop_herb",
               "mean_omn", "clus_coeff", "sd_gen", "sd_vulner", "mean_max_trophic_sim", "mean_path_lt",
               "nest_prop")

# prop_name <- c("sd_vulner")

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
    min_error[i] <- max(min_error_temp, min_error[i])
  }
  
  print(ngut)
}


# Computing the standardised error
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
  
  SE_vs_ngut <- rbind(SE_vs_ngut, SE_vs_ngut_temp)
}


plot_SE_vs_ngut <- ggplot(SE_vs_ngut) +
  geom_line(aes(x = ngut, y = mean_SE)) +
  geom_ribbon(aes(x = ngut, ymin = l_SE, ymax = u_SE), alpha = 0.5) +
  geom_abline(slope = 0, intercept = 0, color = "red") +
  theme_classic() +
  xlab("Number of distinct predator gut content data") +
  ylab("Mean standardised error")


plot_SE_vs_ngut

# ggsave(filename = "results/misc/plot_SE_vs_ngut.png", plot = plot_SE_vs_ngut)




