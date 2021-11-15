#Rule s2
#We plot TSS with number of SIR data sampled


fw_name <- "sim_Small Reef"
n <- 11
l_acc <- numeric(n)
r_acc <- numeric(n)
mean_acc <- numeric(n)
dir_N <- 1000
dir_tol <- 1000
n_sir_sample <- 100
propn <- 1:n
n_sel <- dir_N*0.01
long_acc <- data.frame(acc = double(), n = integer())
rule <- "s2"
desc_main <- "TSS_sir_s2"
for(i in 1:n){
  
  desc <- paste(desc_main, '_l', i, sep = '')
  dir_main <- paste("results/rejection/",fw_name,"/", "rule_", rule,'/N=', dir_N, '_tol=', dir_tol, '_n_pred = ', i,
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
  
  hdi_acc <- hdi(sim_TSS, credMass = 0.9999)
  l_acc[i] <- hdi_acc[1]
  r_acc[i] <- hdi_acc[2]
  mean_acc[i] <- mean(sim_TSS)
  dd <- data.frame(l_acc = l_acc, r_acc = r_acc, mean_acc = mean_acc, propn = propn)
  
}


dd <- rbind(dd_1, dd_2)

dd_plot <- ggplot(dd) +
  # geom_line(aes(x = propn, y = mean_acc)) +
  geom_point(aes(x = propn, y = mean_acc), 
             position=position_dodge(width=0.5)) +
  geom_errorbar(aes(x = propn, ymin = l_acc, ymax = r_acc), 
                position=position_dodge(width=0.5)) +
  xlab("Number of different predator species sampled") +
  ylab("TSS (predicted food web, observed food web)") +
  theme_classic() +
  scale_color_manual(name = "Amount of information in a diet", values = c("red", "blue")) +
  theme(axis.text = element_text(family = "Times New Roman", size = 20),
        axis.title = element_text(family = "Times New Roman", size = 20),
        legend.title = element_text(family = "Times New Roman", size = 20),
        legend.text = element_text(family = "Times New Roman", size = 20)) +
  scale_x_discrete(limits=c(1, 10, 20, 30, 40, 50))

ggsave(filename = "results/rejection/sim_Small Reef/rule_r8/TSS_with_n_pred_prop.png", plot = dd_plot,
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
