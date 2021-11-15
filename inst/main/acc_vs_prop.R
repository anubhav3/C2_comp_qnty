#Looking at how the accuracy of the model prediction varies with proportion of gut contents data
#Rule r4
fw_name <- "sim_Small Reef"
n <- 12
dir_org <- paste("results/rejection/", fw_name, "/old/rN=1000_tol=0.1_TSS_gut_r4_l", sep = "")
l_acc <- numeric(n)
r_acc <- numeric(n)
mean_acc <- numeric(n)
propn <- 1:n
long_acc <- data.frame(acc = double(), n = integer())
for(i in 1:n){
  dirnam <- paste(dir_org, i+2, sep = "")
  fname <- paste(dirnam, "/", fw_name, "_prop.Rdata", sep = "")
  prop_acc <- readRDS(fname)
  hdi_acc <- hdi(prop_acc$prop$TSS)
  l_acc[i] <- hdi_acc[1]
  r_acc[i] <- hdi_acc[2]
  mean_acc[i] <- mean(prop_acc$prop$TSS)
  long_acc <- rbind(long_acc,
                    data.frame(acc = prop_acc$prop$TSS, n = i))
}

dd <- data.frame(l_acc = l_acc, r_acc = r_acc, mean_acc = mean_acc, propn = 3:14)
ggplot(dd) +
  geom_line(aes(x = propn, y = mean_acc)) +
  geom_point(aes(x = propn, y = mean_acc)) +
  geom_errorbar(aes(x = propn, ymin = l_acc, ymax = r_acc), color = "red") +
  xlab("Number of predator's diet breadth") +
  ylab("True Skill Statistics") +
  ylim(c(-1,1))


ggplot(long_acc) +
  geom_violin(aes(x = as.factor(n), y = acc), color = "blue", scale = 3, fill = "red", alpha = 0.7) +
  theme_classic() +
  ylim(c(-1,1)) +
  xlab("Number of predator's diet breadth") +
  ylab("True Skill Statistics")

ggsave(filename = "results/rejection/sim_Small Reef/TSS_vs_npred_l_to_r.png", height = 5, width = 15)

##########################################
##Rule r5
fw_name <- "sim_Small Reef"
n <- 13
dir_org <- paste("results/rejection/", fw_name, "/rN=100_tol=2_TSS_gut_r5_la", sep = "")
l_acc <- numeric(n)
r_acc <- numeric(n)
mean_acc <- numeric(n)
propn <- 1:n
long_acc <- data.frame(acc = double(), n = integer())
for(i in 1:n){
  dirnam <- paste(dir_org, i, sep = "")
  fname <- paste(dirnam, "/", fw_name, "_prop.Rdata", sep = "")
  prop_acc <- readRDS(fname)
  acc <- prop_acc$prop$TSS
  hist_acc <- hist(acc, plot= FALSE)
  hdi_acc <- hdi(acc,credMass = 0.95)
  l_acc[i] <- hdi_acc[1]
  r_acc[i] <- hdi_acc[2]
  mean_acc[i] <- hist_acc$mids[which.max(hist_acc$counts)]
  long_acc <- rbind(long_acc,
                    data.frame(acc = acc, n = i))
}

dd <- data.frame(l_acc = l_acc, r_acc = r_acc, mean_acc = mean_acc, propn = as.factor(1:n))
ggplot(dd) +
  geom_line(aes(x = propn, y = mean_acc)) +
  geom_point(aes(x = propn, y = mean_acc)) +
  geom_errorbar(aes(x = propn, ymin = l_acc, ymax = r_acc), color = "red") +
  xlab("Number of predator's diet breadth") +
  ylab("True Skill Statistics") +
  ylim(c(-1,1)) +
  geom_violin(data = long_acc, aes(x = as.factor(n), y = acc), color = "blue") +
  theme_classic()
  
  
  
ggplot(long_acc) +
  geom_violin(aes(x = as.factor(n), y = acc), color = "blue", scale = 3, fill = "red", alpha = 0.7) +
  theme_classic() +
  ylim(c(-1,1)) +
  xlab("Number of predator's diet breadth") +
  ylab("True Skill Statistics")

ggsave(filename = "results/rejection/sim_Small Reef/TSS_vs_npred_unif.png", height = 5, width = 15)

##########################################
### Rule: r6 #####

fw_name <- "sim_Small Reef"
n <- 50
dir_org <- paste("results/rejection/", fw_name, "/rN=100_tol=2_TSS_gut_r6_l", sep = "")
l_acc <- numeric(n)
r_acc <- numeric(n)
mean_acc <- numeric(n)
propn <- 1:n
long_acc <- data.frame(acc = double(), n = integer())
for(i in 1:n){
  dirnam <- paste(dir_org, i, "_prop_0.7", sep = "")
  fname <- paste(dirnam, "/", fw_name, "_prop.Rdata", sep = "")
  prop_acc <- readRDS(fname)
  acc <- prop_acc$prop$TSS
  hist_acc <- hist(acc, plot= FALSE)
  hdi_acc <- hdi(acc,credMass = 0.95)
  l_acc[i] <- hdi_acc[1]
  r_acc[i] <- hdi_acc[2]
  mean_acc[i] <- hist_acc$mids[which.max(hist_acc$counts)]
  long_acc <- rbind(long_acc,
                    data.frame(acc = acc, n = i))
}

dd <- data.frame(l_acc = l_acc, r_acc = r_acc, mean_acc = mean_acc, propn = as.factor(1:n))
ggplot(dd) +
  geom_line(aes(x = propn, y = mean_acc)) +
  geom_point(aes(x = propn, y = mean_acc)) +
  geom_errorbar(aes(x = propn, ymin = l_acc, ymax = r_acc), color = "red") +
  xlab("Number of predator's diet breadth") +
  ylab("True Skill Statistics") +
  ylim(c(-1,1)) +
  geom_violin(data = long_acc, aes(x = as.factor(n), y = acc), color = "blue") +
  theme_classic()


ggplot(long_acc) +
  geom_violin(aes(x = as.factor(n), y = acc), color = "blue", scale = 3, fill = "red", alpha = 0.7) +
  theme_classic() +
  ylim(c(-1,1)) +
  xlab("Number of predator's diet breadth") +
  ylab("True Skill Statistics")

ggsave(filename = "results/rejection/sim_Small Reef/TSS_vs_n_ind_pred_unif.png", height = 5, width = 15)



## Rule e1

fw_name <- "sim_Small Reef"
n <- 5
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
  acc <- 1-prop_acc$dist
  hist_acc <- hist(acc, plot= FALSE)
  hdi_acc <- hdi(acc,credMass = 0.95)
  l_acc[i] <- hdi_acc[1]
  r_acc[i] <- hdi_acc[2]
  mean_acc[i] <- hist_acc$mids[which.max(hist_acc$counts)]
  long_acc <- rbind(long_acc,
                    data.frame(acc = acc, n = i))
}

dd <- data.frame(l_acc = l_acc, r_acc = r_acc, mean_acc = mean_acc, propn = as.factor(1:n))
ggplot(dd) +
  geom_line(aes(x = propn, y = mean_acc)) +
  geom_point(aes(x = propn, y = mean_acc)) +
  geom_errorbar(aes(x = propn, ymin = l_acc, ymax = r_acc), color = "red") +
  xlab("Number of predator's diet breadth") +
  ylab("True Skill Statistics") +
  ylim(c(-1,1))
