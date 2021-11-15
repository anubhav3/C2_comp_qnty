##Rule e1
fw_name <- "sim_Small Reef"
n <- 50
dir_org <- paste("results/rejection/", fw_name, "/rule_e1/rN=1e+05_tol=2_TSS_gut_e1_l", sep = "")
l_acc <- numeric(n)
r_acc <- numeric(n)
mean_acc <- numeric(n)
propn <- 1:n
long_acc <- data.frame(acc = double(), n = integer())
for(i in 1:n){
  dirnam <- paste(dir_org, i, sep = "")
  fname <- paste(dirnam, "/", fw_name, ".Rdata", sep = "")
  prop_acc <- readRDS(fname)
  gut_TSS <- 1 - prop_acc$dist
  gut_TSS_indexes <- order(gut_TSS, decreasing = TRUE)[1:1e3]
  sim_TSS <- 1 - prop_acc$acc_ss[gut_TSS_indexes]
  hdi_acc <- hdi(sim_TSS, credMass = 0.9999999)
  l_acc[i] <- hdi_acc[1]
  r_acc[i] <- hdi_acc[2]
  mean_acc[i] <- mean(sim_TSS)
  long_acc <- rbind(long_acc,
                    data.frame(acc = sim_TSS, n = i))
}

dd <- data.frame(l_acc = l_acc, r_acc = r_acc, mean_acc = mean_acc, propn = propn, main_prop = as.factor(main_prop))
dd <- rbind(dd_1, dd_2, dd_3)
ggplot(dd) +
  # geom_line(aes(x = propn, y = mean_acc)) +
  geom_point(aes(x = as.factor(propn), y = mean_acc, color = as.factor(main_prop)), 
             position=position_dodge(width=0.5)) +
  geom_errorbar(aes(x = propn, ymin = l_acc, ymax = r_acc, color = as.factor(main_prop)), 
                position=position_dodge(width=0.5)) +
  xlab("Number of different predator individuals sampled") +
  ylab("True Skill Statistics") +
  theme_classic()

ggplot(long_acc) +
  geom_violin(aes(x = as.factor(n), y = acc), color = "blue", scale = 3, fill = "red", alpha = 0.7) +
  theme_classic() +
  ylim(c(-1,1)) +
  xlab("Number of predator individuals sampled") +
  ylab("True Skill Statistics")

ggsave(filename = "results/rejection/sim_Small Reef/TSS_vs_npred_l_to_r.png", height = 5, width = 15)

############################################################################

##Rule r7
## Criterion of selection is proportion

TSS_vs_pred <- function(main_prop){
  fw_name <- "sim_Small Reef"
  n <- 50
  dir_org <- paste("results/rejection/", fw_name, "/rN=1e+05_tol=2_TSS_gut_r7_l", sep = "")
  l_acc <- numeric(n)
  r_acc <- numeric(n)
  mean_acc <- numeric(n)
  propn <- 1:n
  main_prop <- main_prop
  long_acc <- data.frame(acc = double(), n = integer())
  for(i in 1:n){
    dirnam <- paste(dir_org, i,"_prop_", main_prop, sep = "")
    fname <- paste(dirnam, "/", fw_name, ".Rdata", sep = "")
    prop_acc <- readRDS(fname)
    gut_TSS <- 1 - prop_acc$dist
    gut_TSS_indexes <- order(gut_TSS, decreasing = TRUE)[1:1e3]
    sim_TSS <- 1 - prop_acc$acc_ss[gut_TSS_indexes]
    hdi_acc <- hdi(sim_TSS, credMass = 0.9999)
    l_acc[i] <- hdi_acc[1]
    r_acc[i] <- hdi_acc[2]
    mean_acc[i] <- mean(sim_TSS)
    long_acc <- rbind(long_acc,
                      data.frame(acc = sim_TSS, n = i))
  }
  
  dd <- data.frame(l_acc = l_acc, r_acc = r_acc, mean_acc = mean_acc, propn = propn, main_prop = as.factor(main_prop))
  
  return(dd)
}

dd_1 <- TSS_vs_pred(main_prop = 0.2)
dd_2 <- TSS_vs_pred(main_prop = 0.5)
dd_3 <- TSS_vs_pred(main_prop = 1)
dd <- rbind(dd_1, dd_2, dd_3)

ggplot(dd) +
  # geom_line(aes(x = propn, y = mean_acc)) +
  geom_point(aes(x = propn, y = mean_acc, color = as.factor(main_prop)), 
             position=position_dodge(width=0.5)) +
  geom_errorbar(aes(x = propn, ymin = l_acc, ymax = r_acc, color = as.factor(main_prop)), 
                position=position_dodge(width=0.5)) +
  xlab("Number of different predator individuals sampled") +
  ylab("True Skill Statistics") +
  theme_classic()



############################################################
sr_data_1 <- readRDS("results/rejection/sim_Small Reef/rN=1e+05_tol=2_TSS_gut_r7_l6_prop_0.2/sim_Small Reef.Rdata")
sr_data_12 <- readRDS("results/rejection/sim_Small Reef/rN=1e+05_tol=2_TSS_gut_r7_l6_prop_0.5/sim_Small Reef.Rdata")
sr_data_2 <- readRDS("results/rejection/sim_Small Reef/rN=1e+05_tol=2_TSS_gut_r7_l6_prop_1/sim_Small Reef.Rdata")


dd_1 <- data.frame(gut_TSS = 1-sr_data_1$dist, sim_TSS = 1-sr_data_1$acc_ss, main_prop = 0.2)
dd_12 <- data.frame(gut_TSS = 1-sr_data_12$dist, sim_TSS = 1-sr_data_12$acc_ss, main_prop = 0.5)
dd_2 <- data.frame(gut_TSS = 1-sr_data_2$dist, sim_TSS = 1-sr_data_2$acc_ss, main_prop = 1)
dd <- rbind(dd_1, dd_12, dd_2)

# dd_lm <- lm(sim_TSS ~ gut_TSS, data = dd)
# 
# summary(dd_lm)
# autoplot(dd_lm) 
# anova(dd_lm)
# plot(dd_2)

ggplot(dd) +
  geom_point(aes(x = gut_TSS, y = sim_TSS, color = as.factor(main_prop)), alpha = 0.5, size = 0.3) +
  xlim(c(-1,1)) +
  ylim(c(-1,1))



