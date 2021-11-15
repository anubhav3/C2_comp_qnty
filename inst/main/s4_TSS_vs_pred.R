#Rule s4
#We plot TSS with number of SIR data sampled
#s4: varying the number of SIR data from 1 to many (species based sampling) with increasing body size

fw_name <- "sim_Small Reef"
n <- 50
nspecies <- n
l_acc <- numeric(n)
r_acc <- numeric(n)
mean_acc <- numeric(n)
dir_N <- 1e5
dir_tol <- Inf
n_sir_sample <- 1
propn <- 1:n
n_sel <- dir_N*0.001
long_acc <- data.frame(acc = double(), n = integer())
rule <- "s4"
desc_main <- "TSS_sir_s4"
for(i in 1:n){
  
  desc <- paste(desc_main, '_l', i, sep = '')
  dir_main <- paste("../../../PhD_local/C2_files/",fw_name,"/", "rule_", rule,'/N=', dir_N, '_tol=', dir_tol, '_n_pred = ', i,
                    sep = "")
  sim_TSS <- c()
  for(n_sample  in 1:n_sir_sample){
    dirnam <- paste(c(dir_main, '/rn_sir=', n_sample,'_N=', dir_N, '_tol=', dir_tol,"_", desc), collapse = '')
    fname <- paste(dirnam, "/", fw_name, ".Rdata", sep = "")
    prop_acc <- readRDS(fname)
    sir_TSS <- 1 - prop_acc$dist
    sir_TSS_indexes <- order(sir_TSS, decreasing = TRUE)[1:n_sel]
    sim_TSS_temp <- 1 - prop_acc$acc_ss[sir_TSS_indexes]
    sim_TSS <- c(sim_TSS, sim_TSS_temp)
  }
  
  range_acc <- range(sim_TSS)
  l_acc[i] <- range_acc[1]
  r_acc[i] <- range_acc[2]
  mean_acc[i] <- mean(sim_TSS)
  dd <- data.frame(l_acc = l_acc, r_acc = r_acc, mean_acc = mean_acc, propn = propn)
  
}

# dd_s3 <- data.frame(l_acc = dd$l_acc, r_acc = dd$r_acc, mean_acc = dd$mean_acc, propn = dd$propn,
#                    type = "s3: decreasing body size")

# dd_s4 <- data.frame(l_acc = dd$l_acc, r_acc = dd$r_acc, mean_acc = dd$mean_acc, propn = dd$propn,
#                    type = "s4: increasing body size")

# dd_s3_s4 <- rbind(dd_s3, dd_s4)


dd_plot <- ggplot(dd_s3_s4) +
  # geom_line(aes(x = propn, y = mean_acc)) +
  geom_point(aes(x = propn, y = mean_acc, color = type), 
             position=position_dodge(width=0.5)) +
  geom_errorbar(aes(x = propn, ymin = l_acc, ymax = r_acc, color = type), 
                position=position_dodge(width=0.5)) +
  xlab("Number of distinct predator species gut in sample") +
  ylab("TSS (predicted food web, observed food web)") +
  scale_color_manual(name = "Type of species selection", values = c("blue", "red"), label = c("Decreasing body size",
                                                                                                  "Increasing body size")) +
  theme_classic() +
  theme(axis.text = element_text(family = "Times New Roman", size = 20),
        axis.title = element_text(family = "Times New Roman", size = 20),
        legend.title = element_text(family = "Times New Roman", size = 20),
        legend.text = element_text(family = "Times New Roman", size = 20),
        legend.position = "bottom", legend.box = "vertical", legend.margin=margin()) +
  guides(color=guide_legend(nrow=2, byrow=TRUE)) +
  scale_x_discrete(limits=c(1, 10, 20, 30, 40, 50))

# ggsave(filename = "results/rejection/sim_Small Reef/rule_s4/TSS_with_n_pred_prop_s3_s4.png", plot = dd_plot, width = 10, height = 10)
##################################################


sr_data_1 <- readRDS("../../../PhD_local/C2_files/sim_Small Reef/rule_s4/N=1e+05_tol=Inf_n_pred = 10/rn_sir=1_N=1e+05_tol=Inf_TSS_sir_s4_l10/sim_Small Reef.Rdata")
hh_1 <- data.frame(sir_dist = sr_data_1$dist, sim_TSS = 1-sr_data_1$acc_ss)


plot1 <- hh_1 %>%
  filter(sir_dist < 1000) %>%
ggplot() +
  geom_point(aes(x = sir_dist, y = sim_TSS)) +
  ylab("TSS (predicted food web, observed food web)") +
  xlab("distance (predicted SIR, observed SIR)") +
  ggtitle("no. of species' SIR = 10 for simulated Small Reef")

ggsave(filename = "results/rejection/sim_Small Reef/rule_s4/n=10_TSS_vs_distance.png", plot = plot1)
