#Rule r8
#We plot TSS with number of predator diets sampled


fw_name <- "sim_Small Reef"
nspecies <- 50
propn <- 1:nspecies
l_acc <- numeric(nspecies)
r_acc <- numeric(nspecies)
mean_acc <- numeric(nspecies)
dir_N <- 10000
dir_tol <- 1000
n_gut_sample <- 100
n_sel <- 1
main_prop <- 1
long_acc <- data.frame(acc = double(), n = integer())
rule <- "r8"


# dir_org <- paste("results/rejection/", fw_name, "/rN=1e+05_tol=2_TSS_gut_r7_l", sep = "")
desc_main <- "TSS_gut_r8"
for(i in 1:50){
  
  desc <- paste(desc_main, '_l', i, "_prop_",main_prop, sep = '')
  dir_main <- paste("../../../PhD_local/C2_files/",fw_name,"/", "rule_", rule,'/N=', dir_N, '_tol=', dir_tol, '_n_pred = ', i,
                    sep = "")
  sim_TSS <- c()
  for(n_sample  in 1:n_gut_sample){
    dirnam <- paste(c(dir_main, '/rn_gut=', n_sample,'_N=', dir_N, '_tol=', dir_tol,"_", desc), collapse = '')
    fname <- paste(dirnam, "/", fw_name, ".Rdata", sep = "")
    prop_acc <- readRDS(fname)
    gut_TSS <- 1 - prop_acc$dist
    gut_TSS_indexes <- order(gut_TSS, decreasing = TRUE)[1:n_sel]
    sim_TSS_temp <- 1 - prop_acc$acc_ss[gut_TSS_indexes]
    sim_TSS <- c(sim_TSS, sim_TSS_temp)
  }
  
  range_acc <- range(sim_TSS)
  l_acc[i] <- range_acc[1]
  r_acc[i] <- range_acc[2]
  mean_acc[i] <- mean(sim_TSS)
  
}


# dd_1 <- data.frame(l_acc = l_acc, r_acc = r_acc, mean_acc = mean_acc, propn = propn, main_prop = as.factor(main_prop))

# dd_2 <- data.frame(l_acc = l_acc, r_acc = r_acc, mean_acc = mean_acc, propn = propn, main_prop = as.factor(main_prop))

# dd <- rbind(dd_1, dd_2)
tsize <- 30
dd_plot <- ggplot(dd_1) +
  geom_line(aes(x = propn, y = mean_acc, color = main_prop)) +
  geom_point(aes(x = propn, y = mean_acc, color = main_prop),
             position=position_dodge(width=0.5)) +
  geom_errorbar(aes(x = propn, ymin = l_acc, ymax = r_acc, color = main_prop),
                position=position_dodge(width=0.5)) +
  # geom_line(aes(x = propn, y = mean_acc, color = main_prop)) +
  # geom_ribbon(aes(x = propn, ymin = l_acc, ymax = r_acc, fill = main_prop), alpha = 0.5, show.legend = FALSE) +
  xlab("Number of distinct predator gut in a sample") +
  ylab("TSS (predicted food web, observed food web)") +
  theme_classic() +
  scale_color_manual(name = "Amount of information in a single gut data", values = c("blue", "red"), label = c("Complete individual gut data",
                                                                                                    "Partial (20%) individual gut data")) +
  theme(axis.text = element_text(family = "Times New Roman", size = tsize),
        axis.title = element_text(family = "Times New Roman", size = tsize),
        legend.title = element_text(family = "Times New Roman", size = tsize),
        legend.text = element_text(family = "Times New Roman", size = tsize),
        legend.position = "bottom", legend.box = "vertical", legend.margin=margin()) +
  guides(color=guide_legend(nrow=2, byrow=TRUE)) +
  scale_x_discrete(limits = c(1,10,20,30,40,50))

# ggsave(filename = "results/rejection/sim_Small Reef/rule_r8/TSS_with_n_pred_prop.png", plot = dd_plot, width = 15, height = 10)
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
