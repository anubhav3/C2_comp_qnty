#Rule ind
#We plot TSS with number of predator diets sampled for different values of n_sel

fw_name <- "Broadstone Stream size_agg_v2"
dir_N <- 1e5
dir_tol <- 2 #different for ind and rand
n_gut_sample <- 100 
propn <- seq(8, 1008, by = 20)
n_sel <- 1 #number of diets of predator selected with maximum TSS
n <- length(propn)
l_acc <- numeric(n)
r_acc <- numeric(n)
mean_acc <- numeric(n)

desc_main <- "TSS_gut_ind"
rule <- "ind"
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
    sim_TSS_temp <- 1 - prop_acc$acc_ss[gut_TSS_indexes]
    sim_TSS <- c(sim_TSS, sim_TSS_temp)
  }
  
  range_acc <- range(sim_TSS)
  l_acc[l_ind] <- range_acc[1]
  r_acc[l_ind] <- range_acc[2]
  mean_acc[l_ind] <- mean(sim_TSS)
  l_ind <- l_ind+1
}

# dd_real <- data.frame(l_acc = l_acc, r_acc = r_acc, mean_acc = mean_acc, propn = propn, type = "real")
# dd_rand <- data.frame(l_acc = l_acc, r_acc = r_acc, mean_acc = mean_acc, propn = propn, type = "random")

# dd_1 <- data.frame(l_acc = l_acc, r_acc = r_acc, mean_acc = mean_acc, propn = propn, n_sel = n_sel)
# dd_10 <- data.frame(l_acc = l_acc, r_acc = r_acc, mean_acc = mean_acc, propn = propn, n_sel = n_sel)
dd_50 <- data.frame(l_acc = l_acc, r_acc = r_acc, mean_acc = mean_acc, propn = propn, n_sel = n_sel)

dd <- rbind(dd_1, dd_10, dd_50)

dd$n_sel <- as.factor(dd$n_sel)

dd_plot <- ggplot(dd) +
  # geom_line(aes(x = propn, y = mean_acc)) +
  geom_line(aes(x = propn, y = mean_acc, color = n_sel), 
            position=position_dodge(width = 3)) +
  geom_ribbon(aes(x = propn, ymin = l_acc, ymax = r_acc, fill = n_sel), alpha = 0.5, show.legend = FALSE,
              position=position_dodge(width = 3)) +
  xlab("Number of different predator guts sampled") +
  ylab("TSS (predicted food web, observed food web)") +
  # theme_classic() +
  # scale_color_manual(name = "gut data", values = c("red", "blue")) +
  theme(axis.text = element_text(family = "Times New Roman", size = 20),
        axis.title = element_text(family = "Times New Roman", size = 20),
        legend.title = element_text(family = "Times New Roman", size = 20),
        legend.text = element_text(family = "Times New Roman", size = 20)) 


ggsave(filename = "results/rejection/Broadstone Stream size_agg_v2/rule_ind/TSS_with_n_pred_prop_n_sel=1_10_50.png", plot = dd_plot,
       width = 15, height = 10)
##################################################
sr_data_1 <- readRDS("results/rejection/sim_Small Reef/rule_r8/N=5000_tol=2_n_pred = 50/rn_gut=1_N=5000_tol=2_TSS_gut_r8_l50_prop_0.2/sim_Small Reef.Rdata")
hh_1 <- data.frame(gut_TSS = 1-sr_data_1$dist, sim_TSS = 1-sr_data_1$acc_ss, main_prop = 0.2)

sr_data_2 <- readRDS("results/rejection/sim_Small Reef/rule_r8/N=1000_tol=2_n_pred = 50/rn_gut=1_N=1000_tol=2_TSS_gut_r8_l50_prop_1/sim_Small Reef.Rdata")
hh_2 <- data.frame(gut_TSS = 1-sr_data_2$dist, sim_TSS = 1-sr_data_2$acc_ss, main_prop = 1)

hh <- rbind(hh_1, hh_2)

ggplot(hh) +
  geom_point(aes(x = gut_TSS, y = sim_TSS, color = as.factor(main_prop)), alpha = 0.5, size = 0.3) +
  xlim(c(-1,1)) +
  ylim(c(-1,1))
