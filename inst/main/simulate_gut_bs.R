# 23.04.2021
# Simulate gut of individuals for a simulated Broadstone Stream food web

library(ggplot2)
library(dplyr)

fw_name <- "Broadstone Stream size_agg_v2"
rule <- "ind"
fname <- paste("data/gut_data/",fw_name,"_",rule,".gut.Rdata", sep = "")
diet_mat <- readRDS(fname)

pred_mat <- readRDS("data/Broadstone Stream size_agg_v2.web.Rdata")$predation.matrix
  
diet_mat[3,]

local_diet <- data.frame(pred_ind = diet_mat[2,], local_prey = colSums(diet_mat[-c(1,2),]))

global_diet <- data.frame(pred_ind = 1:29, total_prey = colSums(pred_mat))


local_global_diet <- merge(x = local_diet, y = global_diet, by = "pred_ind")

prop_prey_diet <- local_global_diet <-
  local_global_diet %>%
  mutate(prop_prey = local_prey/total_prey)

plot_local_global_diet <- ggplot(local_global_diet) +
  geom_histogram(aes(x = prop_prey)) +
  # geom_density(aes(x = prop_prey, y = ..density..)) +
  facet_wrap(~pred_ind, nrow = 4, labeller = labeller(pred_ind = c("14" = "Predator node: 14",
                                                                   "15" = "Predator node: 15",
                                                                   "16" = "Predator node: 16",
                                                                   "17" = "Predator node: 17",
                                                                   "18" = "Predator node: 18",
                                                                   "19" = "Predator node: 19",
                                                                   "20" = "Predator node: 20",
                                                                   "21" = "Predator node: 21",
                                                                   "22" = "Predator node: 22",
                                                                   "23" = "Predator node: 23",
                                                                   "24" = "Predator node: 24",
                                                                   "25" = "Predator node: 25",
                                                                   "26" = "Predator node: 26",
                                                                   "27" = "Predator node: 27",
                                                                   "28" = "Predator node: 28",
                                                                   "29" = "Predator node: 29"))) +
  theme_bw() +
  xlim(c(0,1)) +
  xlab("Proportion of prey in predator's gut") +
  ylab("Frequency") +
  theme_classic()


# Consiering predator node: 14
dd <- data.frame(ngut = integer(), l_prop = double(), u_prop = double(), mean_prop = double(), pred_node = character())
for(pred_node in 14:29){
  gut_data <- diet_mat[, which(diet_mat[2,] == pred_node)][3:31,]
  diet_pred <- pred_mat[, pred_node]
  n_gut_pred <- dim(gut_data)[2]
  n_diet_pred <- sum(diet_pred)
  n_sample <- 100
  
  for(ngut in 1:n_gut_pred){
    prop_temp <- c()
    for(sample_ind in 1:n_sample){
      sampled_gut_data <- gut_data[, sample(x = 1:n_gut_pred, size = ngut, replace = FALSE)]
      if(ngut > 1){
        sampled_gut_data <- rowSums(sampled_gut_data)
      }
      sampled_gut_data[which(sampled_gut_data >= 1)] <- 1 
      prop_temp <- c(prop_temp, sum(sampled_gut_data)/n_diet_pred)
    }
    mean_prop <- mean(prop_temp)
    l_prop <- range(prop_temp)[1]
    u_prop <- range(prop_temp)[2]
    dd <- rbind(dd, 
                data.frame(ngut = ngut, l_prop = l_prop, u_prop = u_prop, mean_prop = mean_prop, pred_node = pred_node)
    )
  }
  
}


plot_rarefaction_diet <- ggplot(dd) +
  geom_point(aes(x = ngut, y = mean_prop)) +
  geom_line(aes(x = ngut, y = mean_prop)) +
  geom_ribbon(aes(x = ngut, ymin = l_prop, ymax = u_prop), alpha = 0.5) + 
  facet_wrap(~pred_node, nrow = 4, labeller = labeller(pred_node = c("14" = "Predator node: 14",
                                                                    "15" = "Predator node: 15",
                                                                    "16" = "Predator node: 16",
                                                                    "17" = "Predator node: 17",
                                                                    "18" = "Predator node: 18",
                                                                    "19" = "Predator node: 19",
                                                                    "20" = "Predator node: 20",
                                                                    "21" = "Predator node: 21",
                                                                    "22" = "Predator node: 22",
                                                                    "23" = "Predator node: 23",
                                                                    "24" = "Predator node: 24",
                                                                    "25" = "Predator node: 25",
                                                                    "26" = "Predator node: 26",
                                                                    "27" = "Predator node: 27",
                                                                    "28" = "Predator node: 28",
                                                                    "29" = "Predator node: 29")),
             scales = "free") +
  theme_classic() +
  xlab("Number of guts") +
  ylab("Proportion of prey")

# ggsave(filename = "results/misc/prey_propn_in_gut.png", plot = plot_local_global_diet, width = 20, height = 20, units = "cm")
# ggsave(filename = "results/misc/rarefaction_diet.png", plot = plot_rarefaction_diet, width = 20, height = 20, units = "cm")

# set.seed(1)
# Simulating the diet matrix

sim_diet_mat <- diet_mat
sim_diet_mat[3:31,] <- 0
pred_nodes <- which(colSums(pred_mat) != 0)
sim_pred_mat <- readRDS("data/sim_Broadstone Stream size_agg_v2.web.Rdata")$predation.matrix

ind <- 1

for(pred_ind in diet_mat[2,]){
  prob_prop <- sample(x = prop_prey_diet$prop_prey[which(prop_prey_diet$pred_ind == pred_ind)], size = 1)
  diet_pred <- which(sim_pred_mat[, pred_ind] > 0)
  n_prey <- length(diet_pred)
  
  prey_ind <- sample(diet_pred, size = round(n_prey*prob_prop), replace = FALSE)
  
  sim_diet_mat[-c(1,2), ind][prey_ind] <- 1
  ind <- ind+1
}


# saveRDS(object = sim_diet_mat, file = "data/gut_data/sim_Broadstone Stream size_agg_v2_ind_predator.gut.Rdata")


