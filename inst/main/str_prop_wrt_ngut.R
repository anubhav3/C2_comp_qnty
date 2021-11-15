# 20.09.2021
# We plot the predicted structural properties wrt ngut

library(R.utils); library(tictoc); library(stringr)
library(ggpubr); library(plotrix); library(ggplot2)
library(cowplot); library(latex2exp); library(HDInterval)
library(doParallel); library(foreach); library(raster)
library(cheddar); library(dplyr); library(reshape2)
library(psych); library(DirectedClustering); library(colorspace)
library(bipartite)

## Relative path from the project directory
sourceDirectory("R", modifiedOnly=FALSE)

fw_name <- "Tadnoll Brook size_agg"
propn <- c(seq(1, 688, 14), 688)

dir_N <- 1e5
dir_tol <- 2 #different for ind and lbs and ubs
n_gut_sample <- 100 

rule <- "ind_predator"

fname_data <- paste("data/", fw_name, ".web.Rdata", sep="")
fw_data <- readRDS(fname_data)
model_core_par <- ADBM_core_par(fw_data)

real_prop_calc <- real_prop_predator(fw_data)
prop_name_list <- c("connectance" = "Connectance")

prop_all <- c("connectance", "prop_basal", "prop_inter", "prop_top", "prop_herb",
              "mean_trop_lvl", "max_trop_lvl", "mean_omn", "clus_coeff", "sd_gen",
              "sd_vulner", "mean_max_trophic_sim", "mean_path_lt", "nest_prop")

prop_labeller <- c(connectance = "Connectance", prop_basal = "Proportion of Basal Species",
                   prop_inter = "Proportion of Intermediate Species", prop_top = "Proportion of Top Predators",
                   prop_herb = "Proportion of Herbivores", mean_trop_lvl = "Mean Trophic Level",
                   max_trop_lvl = "Maximum Trophic Level", mean_omn = "Mean Omnivory",
                   clus_coeff = "Clustering Coefficient", sd_gen = "SD of Generality",
                   sd_vulner = "SD of Vulnerability", mean_max_trophic_sim = "Mean Maximum Trophic Similarity",
                   mean_path_lt = "Mean Path Length", nest_prop  = "Nestedness")

prop_ngut <- data.frame(ngut = integer(), l_prop_error = double(), u_prop_error = double(), mean_prop_error = double(),
                        prop_name = character())


for(prop_name in prop_all){
  for(i in propn){
    fname_i <- paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_", rule, 
                      "/accepted_par/properties/", fw_name, "_n_diet=", i, "_properties.Rdata")
    
    real_prop_sel <- as.numeric(real_prop_calc[prop_name])
    
    prop_i <-  readRDS(file = fname_i)$prop
    prop_sel <- prop_i[, prop_name]
    prop_sel_error <- abs(prop_sel - real_prop_sel)
    
    
    prop_ngut <- rbind(prop_ngut,
                       data.frame(ngut = i, l_prop_error = min(prop_sel_error), 
                                  u_prop_error = max(prop_sel_error), mean_prop_error = mean(prop_sel_error),
                                  prop_name = prop_name)
    )
    print(i)
  }
}


#### Broadstone Stream size_agg_v2

plot_bs <- ggplot(prop_ngut) +
  geom_line(aes(x = ngut, y = mean_prop_error)) +
  geom_ribbon(aes(x = ngut, ymin = l_prop_error, ymax = u_prop_error), alpha = 0.5) +
  facet_wrap(~prop_name, scales = "free", ncol = 3, 
             labeller = as_labeller(prop_labeller)) +
  xlab("Number of predator guts") +
  ylab("Absolute Error") +
  scale_x_discrete(limits = c(1, 250, 500, 750, 1008)) +
  theme_classic()

# ggsave(filename = "../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Broadstone Stream size_agg_v2/rule_ind_predator/error_prop_ngut.png",
#        plot = plot_bs, width = 6, height = 8)


#### Celtic Sea size_agg

plot_cs <- ggplot(prop_ngut) +
  geom_line(aes(x = ngut, y = mean_prop_error)) +
  geom_ribbon(aes(x = ngut, ymin = l_prop_error, ymax = u_prop_error), alpha = 0.5) +
  facet_wrap(~prop_name, scales = "free", ncol = 3, 
             labeller = as_labeller(prop_labeller)) +
  xlab("Number of predator guts") +
  ylab("Absolute Error") +
  scale_x_discrete(limits = c(1, 200, 300, 400, 491)) +
  theme_classic()

# ggsave(filename = "../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Celtic Sea size_agg/rule_ind_predator/error_prop_ngut.png",
#        plot = plot_cs, width = 6, height = 8)


#### Afon Hirnant size_agg

plot_ah <- ggplot(prop_ngut) +
  geom_line(aes(x = ngut, y = mean_prop_error)) +
  geom_ribbon(aes(x = ngut, ymin = l_prop_error, ymax = u_prop_error), alpha = 0.5) +
  facet_wrap(~prop_name, scales = "free", ncol = 3, 
             labeller = as_labeller(prop_labeller)) +
  xlab("Number of predator guts") +
  ylab("Absolute Error") +
  scale_x_discrete(limits = c(1, 50, 100, 150, 175)) +
  theme_classic()

# ggsave(filename = "../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Afon Hirnant size_agg/rule_ind_predator/error_prop_ngut.png",
#        plot = plot_ah, width = 6, height = 8)


#### Tadnoll Brook size_agg

plot_tb <- ggplot(prop_ngut) +
  geom_line(aes(x = ngut, y = mean_prop_error)) +
  geom_ribbon(aes(x = ngut, ymin = l_prop_error, ymax = u_prop_error), alpha = 0.5) +
  facet_wrap(~prop_name, scales = "free", ncol = 3, 
             labeller = as_labeller(prop_labeller)) +
  xlab("Number of predator guts") +
  ylab("Absolute Error") +
  scale_x_discrete(limits = c(1, 200, 400, 600, 688)) +
  theme_classic()

# ggsave(filename = "../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Tadnoll Brook size_agg/rule_ind_predator/error_prop_ngut.png",
#        plot = plot_tb, width = 6, height = 8)