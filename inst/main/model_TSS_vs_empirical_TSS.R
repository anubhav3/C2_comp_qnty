# 04.05.2021
# Plot model TSS vs empirical TSS


########### Broadstone Stream size_agg_v2

# fw_name <- "Celtic Sea size_agg"
fw_name <- "Broadstone Stream size_agg_v2"
dir_N <- 1e5
dir_tol <- 2 
n_gut_sample <- 100  
# propn <- seq(11, 491, by = 20)
propn <-  c((seq(1, 1008, 20)), 1008)
n_sel <- 1
n <- length(propn)
l_acc <- numeric(n)
r_acc <- numeric(n)
mean_acc <- numeric(n)

desc_main <- "TSS_gut_ind"
rule <- "ind_predator"
l_ind <- 1
for(i in propn){
  
  desc <- paste(desc_main, '_l', i, sep = '')
  # dir_main <- paste("results/rejection/",fw_name,"/", "rule_", rule,'/N=', dir_N, '_tol=', dir_tol, '_n_diet = ', i,
  #                   sep = "")
  dir_main <- paste("../../../PhD_local/C2_files/", fw_name, "/rule_", rule ,'/N=', dir_N, '_tol=', dir_tol, '_n_diet = ', i,
                    sep = "")
  sim_TSS <- c()
  for(n_sample  in 1:n_gut_sample){
    dirnam <- paste(c(dir_main, '/rn_sample=', n_sample,'_N=', dir_N, '_tol=', dir_tol,"_", desc), collapse = '')
    fname <- paste(dirnam, "/", fw_name, ".Rdata", sep = "")
    prop_acc <- readRDS(fname)
    gut_TSS <- 1 - prop_acc$dist
    gut_TSS_indexes <- order(gut_TSS, decreasing = TRUE)[1:n_sel]
    sim_TSS_temp <- prop_acc$TSS_fw[gut_TSS_indexes]
    sim_TSS <- c(sim_TSS, sim_TSS_temp)
  }
  
  range_acc <- range(sim_TSS)
  l_acc[l_ind] <- range_acc[1]
  r_acc[l_ind] <- range_acc[2]
  mean_acc[l_ind] <- mean(sim_TSS)
  l_ind <- l_ind+1
}


# emp_TSS <- readRDS(file = "../../../PhD_local/C2_files/Celtic Sea size_agg/rule_ind_predator/empirical_TSS_ngut.Rdata")
emp_TSS <- readRDS(file = "../../../PhD_local/C2_files/Broadstone Stream size_agg_v2/rule_ind_predator/empirical_TSS_ngut.Rdata")

emp_TSS <- emp_TSS %>%
  filter(ngut %in%  propn)

TSS_model_empirical <- data.frame(model_TSS = mean_acc, data_TSS = emp_TSS$mean_TSS, ngut = emp_TSS$ngut,
                                  l_model_TSS = l_acc, u_model_TSS = r_acc,
                                  l_data_TSS = emp_TSS$l_TSS, u_data_TSS = emp_TSS$u_TSS)


plot_TSS_model_empirical_broadstone_stream_size_agg_v2 <- 
  TSS_model_empirical %>%
  # filter(ngut %in% c(8, 28, 48, 68, 88, 108, 208, 308, 408, 508, 808, 1008)) %>%
  # filter(ngut %in% c(11, 31, 51, 71, 91, 111, 151, 171, 231, 331, 431, 491)) %>%
  ggplot() +
  geom_point(aes(x = data_TSS, y = model_TSS, color = ngut), size = 4) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_ribbon(aes(x = data_TSS, y = model_TSS, ymin = l_model_TSS, ymax = u_model_TSS), alpha = 0.5) +
  geom_linerange(aes(x = data_TSS, y = model_TSS, xmin = l_data_TSS, xmax = u_data_TSS), alpha = 0.5) +
  theme_classic() +
  # xlim(c(0,1)) +
  # ylim(c(0,1)) +
  # scale_color_brewer(name = "Amount of guts", palette = "Paired", labels = c("1%", "3%", "5%", "7%", "9%",
  #                                                                            "11%", "21%", "31%", "40%", "50%",
  #                                                                            "80%", "100%")) +
  # scale_color_brewer(name = "Number of guts", palette = "Paired", labels = c("2%", "6%", "10%", "14%", "18%",
  #                                                                            "22%", "30%", "35%", "47%", "67%",
  #                                                                            "88%", "100%")) +
  scale_color_gradient(low = "blue", high = "red") +
  xlab("TSS (Gut content data)") +
  ylab("TSS (ADBM and Gut content data)") +
  labs(title = "Broadstone Stream", tag = "(a)") +
  theme(plot.title = element_text(hjust=0.5), plot.tag = element_text(face = "bold"))

# ggsave(filename = "results/misc/TSS_model_vs_data_Broadstone_Stream_size_agg_v2.png", plot = plot_TSS_model_empirical)


########### Celtic Sea size_agg

fw_name <- "Celtic Sea size_agg"
dir_N <- 1e5
dir_tol <- Inf #different for ind and rand
n_gut_sample <- 100  
propn <- c(11, 31, 51, 71, 91, 111, 151, 171, 231, 331, 431, 491)
n_sel <- 1
n <- length(propn)
l_acc <- numeric(n)
r_acc <- numeric(n)
mean_acc <- numeric(n)

desc_main <- "TSS_gut_ind"
rule <- "ind_predator"
l_ind <- 1
for(i in propn){
  
  desc <- paste(desc_main, '_l', i, sep = '')
  # dir_main <- paste("results/rejection/",fw_name,"/", "rule_", rule,'/N=', dir_N, '_tol=', dir_tol, '_n_diet = ', i,
  #                   sep = "")
  dir_main <- paste("../../../PhD_local/C2_files/", fw_name, "/rule_", rule ,'/N=', dir_N, '_tol=', dir_tol, '_n_diet = ', i,
                    sep = "")
  sim_TSS <- c()
  for(n_sample  in 1:n_gut_sample){
    dirnam <- paste(c(dir_main, '/rn_sample=', n_sample,'_N=', dir_N, '_tol=', dir_tol,"_", desc), collapse = '')
    fname <- paste(dirnam, "/", fw_name, ".Rdata", sep = "")
    prop_acc <- readRDS(fname)
    gut_TSS <- 1 - prop_acc$dist
    gut_TSS_indexes <- order(gut_TSS, decreasing = TRUE)[1:n_sel]
    sim_TSS_temp <- prop_acc$TSS_fw[gut_TSS_indexes]
    sim_TSS <- c(sim_TSS, sim_TSS_temp)
  }
  
  range_acc <- range(sim_TSS)
  l_acc[l_ind] <- range_acc[1]
  r_acc[l_ind] <- range_acc[2]
  mean_acc[l_ind] <- mean(sim_TSS)
  l_ind <- l_ind+1
}


emp_TSS <- readRDS(file = "../../../PhD_local/C2_files/Celtic Sea size_agg/rule_ind_predator/empirical_TSS_ngut.Rdata")

emp_TSS <- emp_TSS %>%
  filter(ngut %in%  c(11, 31, 51, 71, 91, 111, 151, 171, 231, 331, 431, 491))


TSS_model_empirical <- data.frame(model_TSS = mean_acc, data_TSS = emp_TSS$mean_TSS, ngut = emp_TSS$ngut,
                                  l_model_TSS = l_acc, u_model_TSS = r_acc,
                                  l_data_TSS = emp_TSS$l_TSS, u_data_TSS = emp_TSS$u_TSS)


plot_TSS_model_empirical_celtic_sea <- 
  TSS_model_empirical %>%
  filter(ngut %in% c(11, 31, 51, 71, 91, 111, 151, 171, 231, 331, 431, 491)) %>%
  ggplot() +
  geom_point(aes(x = data_TSS, y = model_TSS, color = as.factor(ngut)), size = 4) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_linerange(aes(x = data_TSS, y = model_TSS, ymin = l_model_TSS, ymax = u_model_TSS), alpha = 0.5) +
  geom_linerange(aes(x = data_TSS, y = model_TSS, xmin = l_data_TSS, xmax = u_data_TSS), alpha = 0.5) +
  theme_classic() +
  xlim(c(0,1)) +
  ylim(c(0,1)) +
  scale_color_brewer(name = "Amount of guts", palette = "Paired", labels = c("2%", "6%", "10%", "14%", "18%",
                                                                             "22%", "30%", "35%", "47%", "67%",
                                                                             "88%", "100%")) +
  xlab("TSS (Gut content data)") +
  ylab("TSS (ADBM and Gut content data)") +
  labs(title = "Celtic Sea", tag = "(b)")  +
  theme(plot.title = element_text(hjust=0.5), plot.tag = element_text(face = "bold"))

# ggsave(filename = "results/misc/TSS_model_vs_data_Broadstone_Stream_size_agg_v2.png", plot = plot_TSS_model_empirical)


plot_TSS_model_empirical <- ggarrange(plot_TSS_model_empirical_broadstone_stream_size_agg_v2, plot_TSS_model_empirical_celtic_sea,
                                      nrow = 1, ncol = 2)
# ggsave(filename = "results/misc/TSS_model_vs_data.png", plot = plot_TSS_model_empirical,
#       width = 11, height = 4)
