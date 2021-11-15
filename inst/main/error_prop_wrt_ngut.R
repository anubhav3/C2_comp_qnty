# 04.05.2021
# Compute error for properties and plot the error graph wrt ngut


propn <- seq(8, 1008, by = 20) #576 for lbs and 432 for ubs
desc_main <- "TSS_gut_ind"
rule <- "ind_predator"
n_gut_sample <- 100 


# prop_name <- c("connectance", "prop_basal", "prop_inter", "prop_top", "prop_herb", "mean_trop_lvl",
#                "mean_omn", "clus_coeff", "sd_gen", "sd_vulner", "mean_max_trophic_sim", "mean_path_lt",
#                "nest_prop")

prop_name <- c("connectance", "TSS")

n_prop <- length(prop_name)
plot_list <- list()


fw_name <- "sim_Broadstone Stream size_agg_v2"

foodweb_data <- readRDS(paste("data/", fw_name, ".web.Rdata", sep=""))
# real_prop_org <- real_prop_predator(foodweb_data)
prop_ind <- 1

for(pname in prop_name){
  df_prop <- data.frame(mean_error = double(), l_error = double(), u_error = double(),
                        ngut = integer())
  
  for(ngut in propn){
    
    desc <- paste(desc_main, '_l', ngut, sep = '')
    dir_main <- paste("../../../PhD_local/C2_files/", fw_name, "/rule_", rule ,'/N=', dir_N, '_tol=', dir_tol, '_n_diet = ', ngut,
                      sep = "")
    fname <- paste(dir_main, "/", fw_name, "_prop.Rdata", sep = "")
    
    prop_data_org <- readRDS(fname)
    prop_data <- prop_data_org$prop[,pname]
    real_prop <- real_prop_org[,pname]
    
    err_prop <- abs(prop_data - real_prop)
    
    l_err_prop <- min(err_prop, na.rm = TRUE)
    u_err_prop <- max(err_prop, na.rm = TRUE)
    mean_err_prop <- mean(err_prop, na.rm = TRUE)
    
    df_prop <- rbind(df_prop, 
                     data.frame(mean_error = mean_err_prop, l_error = l_err_prop, u_error = u_err_prop,
                                           ngut = ngut))
    
  }
  
  plot_list[[prop_ind]] <- ggplot(df_prop) +
    geom_line(aes(x = ngut, y = mean_error)) +
    geom_ribbon(aes(x = ngut, ymin = l_error, ymax = u_error), alpha = 0.5) +
    theme_classic() +
    xlab("Number of distinct predator gut content data") +
    ylab(pname) +
    geom_hline(yintercept = 0, color = "red")
  
  prop_ind <- prop_ind + 1
}

