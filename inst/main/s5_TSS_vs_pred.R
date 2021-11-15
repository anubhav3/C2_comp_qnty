# 08/04/2021
# Rule s5
# We plot TSS with number of SIR data sampled
# s5: We use a linear transformation with uncertainty in the stable isotope baseline to estimate SIR 
# from the trophic levels. This rule is modification of rule s3.


fw_name <- "sim_Benguela Pelagic"
n <- 29
nspecies <- n
l_acc <- numeric(n)
r_acc <- numeric(n)
mean_acc <- numeric(n)
dir_N <- 1005
dir_tol <- Inf
n_sir_sample <- 100
propn <- 1:n
n_sel <- 1
rule <- "s5"
desc_main <- "TSS_sir_s5"

for(i in 1:n){
  
  desc <- paste(desc_main, '_l', i, sep = '')
  dir_main <- paste("../../../PhD_local/C2_files/",fw_name,"/", "rule_", rule,'/N=', dir_N, '_tol=', dir_tol, '_n_pred = ', i,
                    sep = "")
  TSS_fw <- c()
  for(n_sample  in 1:n_sir_sample){
    dirnam <- paste(c(dir_main, '/rn_sir=', n_sample,'_N=', dir_N, '_tol=', dir_tol,"_", desc), collapse = '')
    fname <- paste(dirnam, "/", fw_name, ".Rdata", sep = "")
    prop_acc <- readRDS(fname)
    
    dist_sir <- prop_acc$dist_tl
    dist_sir_indexes <- order(dist_sir, decreasing = FALSE)[1:n_sel]
    dist_fw_temp <- prop_acc$dist_fw[dist_sir_indexes]
    TSS_fw_temp <- 1 - dist_fw_temp
    TSS_fw <- c(TSS_fw, TSS_fw_temp)
  }
  
  range_acc <- range(TSS_fw)
  l_acc[i] <- range_acc[1]
  r_acc[i] <- range_acc[2]
  mean_acc[i] <- mean(TSS_fw)
  dd <- data.frame(l_acc = l_acc, r_acc = r_acc, mean_acc = mean_acc, propn = propn)
  
}


# dd <- rbind(dd_1, dd_2)

dd_plot <- ggplot(dd) +
  # geom_line(aes(x = propn, y = mean_acc)) +
  geom_point(aes(x = propn, y = mean_acc), 
             position=position_dodge(width=0.5)) +
  geom_errorbar(aes(x = propn, ymin = l_acc, ymax = r_acc), 
                position=position_dodge(width=0.5)) +
  xlab("Number of different predator species sampled (largest to smallest)") +
  ylab("TSS (predicted food web, observed food web)") +
  theme_classic() +
  ylim(c(0, 1)) +
  scale_color_manual(name = "Amount of information in a diet", values = c("red", "blue")) +
  theme(axis.text = element_text(family = "Times New Roman", size = 20),
        axis.title = element_text(family = "Times New Roman", size = 20),
        legend.title = element_text(family = "Times New Roman", size = 20),
        legend.text = element_text(family = "Times New Roman", size = 20)) +
  scale_x_discrete(limits=c(1, 10, 20, 30, 40, 50))

# ggsave(filename = "results/rejection/sim_Benguela Pelagic/rule_s5/TSS_with_n_pred_prop.png", plot = dd_plot, width = 15, height = 10)
##################################################
sr_data_1 <- readRDS("../../../PhD_local/C2_files/sim_Small Reef/rule_s3/N=1e+05_tol=Inf_n_pred = 20/rn_sir=1_N=1e+05_tol=Inf_TSS_sir_s3_l20/sim_Small Reef.Rdata")
hh_1 <- data.frame(sir_dist = sr_data_1$dist, sim_TSS = 1-sr_data_1$acc_ss)


plot1 <- hh_1 %>%
  filter(sir_dist < 1000) %>%
  ggplot() +
  geom_point(aes(x = sir_dist, y = sim_TSS)) +
  ylab("TSS (predicted food web, observed food web)") +
  xlab("distance (predicted SIR, observed SIR)") +
  ggtitle("no. of species' SIR = 20 for simulated Small Reef") +
  xlim(c(0, 10)) +
  theme_bw()

# ggsave(filename = "results/rejection/sim_Small Reef/rule_s3/n=20_TSS_vs_distance.png", plot = plot1)
