# 23.07.2021
# We compute the minimum number of gut content data for food webs

library(dplyr)
library(ggplot2)

df_min_gut_vs_S <- data.frame(n_min_gut = integer(), S = integer(), C = double(), L = integer(), 
                              foodweb = character(), prop_min_gut = double(), max_ngut = integer(),
                              max_TSS = double(), n_pred = integer(), mean_prey = double(),
                              sd_prey = double())
threshold_per <- 0.95
  
## Broadstone Stream

fw_name <- "Broadstone Stream size_agg_v2"
pred_mat <- readRDS(paste0("data/", fw_name, ".web.Rdata"))$predation.matrix
n_pred <- sum(colSums(pred_mat)!=0)

gut_data <- readRDS(paste0("data/gut_data/", fw_name, "_ind_predator.gut.Rdata"))
gut_data <- gut_data[-c(1,2),]
mean_prey <- mean(colSums(gut_data))
sd_prey <- sd(colSums(gut_data))

dd <- readRDS(paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_ind_predator/dd_bs.Rdata"))
max_ngut <- max(dd$propn)

max_TSS <- dd$mean_acc[length(dd$mean_acc)]
threshold_TSS <- threshold_per*max_TSS

k <- 1
repeat{
  if(dd$mean_acc[k] > threshold_TSS){
    N <- k - 1
    break
  }
  k <- k + 1
}

n_min_gut <- dd$propn[k]

df_min_gut_vs_S <- rbind(df_min_gut_vs_S, 
                         data.frame(n_min_gut = n_min_gut, S = 28, C = 0.24, L = 185, foodweb = "Broadstone Stream",
                                    prop_min_gut = n_min_gut/max_ngut, max_ngut = max_ngut, max_TSS = max_TSS,
                                    n_pred = n_pred, mean_prey = mean_prey, sd_prey = sd_prey))




## Celtic Sea

fw_name <- "Celtic Sea size_agg"
pred_mat <- readRDS(paste0("data/", fw_name, ".web.Rdata"))$predation.matrix
n_pred <- sum(colSums(pred_mat)!=0)

gut_data <- readRDS(paste0("data/gut_data/", fw_name, "_ind_predator.gut.Rdata"))
gut_data <- gut_data[-c(1,2),]
mean_prey <- mean(colSums(gut_data))
sd_prey <- sd(colSums(gut_data))

dd <- readRDS(paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_ind_predator/dd_cs.Rdata"))
max_ngut <- max(dd$propn)
max_TSS <- dd$mean_acc[length(dd$mean_acc)]
threshold_TSS <- threshold_per*max_TSS

k <- 1
repeat{
  if(dd$mean_acc[k] > threshold_TSS){
    N <- k - 1
    break
  }
  k <- k + 1
}

n_min_gut <- dd$propn[k]
df_min_gut_vs_S <- rbind(df_min_gut_vs_S, 
                         data.frame(n_min_gut = n_min_gut, S = 48, C = 0.17, L = 386, foodweb = "Celtic Sea",
                                    prop_min_gut = n_min_gut/max_ngut, max_ngut = max_ngut, max_TSS = max_TSS,
                                    n_pred = n_pred, mean_prey = mean_prey, sd_prey = sd_prey))


## Tadnoll Brook

fw_name <- "Tadnoll Brook size_agg"
pred_mat <- readRDS(paste0("data/", fw_name, ".web.Rdata"))$predation.matrix
n_pred <- sum(colSums(pred_mat)!=0)

gut_data <- readRDS(paste0("data/gut_data/", fw_name, "_ind_predator.gut.Rdata"))
gut_data <- gut_data[-c(1,2),]
mean_prey <- mean(colSums(gut_data))
sd_prey <- sd(colSums(gut_data))

dd <- readRDS(paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_ind_predator/dd_tb.Rdata"))
max_ngut <- max(dd$propn)
max_TSS <- dd$mean_acc[length(dd$mean_acc)]
threshold_TSS <- threshold_per*max_TSS

k <- 1
repeat{
  if(dd$mean_acc[k] > threshold_TSS){
    N <- k - 1
    break
  }
  k <- k + 1
}

n_min_gut <- dd$propn[k]
df_min_gut_vs_S <- rbind(df_min_gut_vs_S, 
                         data.frame(n_min_gut = n_min_gut, S = 59, C = 0.14, L = 485, foodweb = "Tadnoll Brook",
                                    prop_min_gut = n_min_gut/max_ngut, max_ngut = max_ngut, max_TSS = max_TSS,
                                    n_pred = n_pred, mean_prey = mean_prey, sd_prey = sd_prey))


## Afon Hirnant
fw_name <- "Afon Hirnant size_agg"
pred_mat <- readRDS(paste0("data/", fw_name, ".web.Rdata"))$predation.matrix
n_pred <- sum(colSums(pred_mat)!=0)

gut_data <- readRDS(paste0("data/gut_data/", fw_name, "_ind_predator.gut.Rdata"))
gut_data <- gut_data[-c(1,2),]
mean_prey <- mean(colSums(gut_data))
sd_prey <- sd(colSums(gut_data))

dd <- readRDS(paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_ind_predator/dd_ah.Rdata"))
max_ngut <- max(dd$propn)
max_TSS <- dd$mean_acc[length(dd$mean_acc)]
threshold_TSS <- threshold_per*max_TSS

k <- 1
repeat{
  if(dd$mean_acc[k] > threshold_TSS){
    N <- k - 1
    break
  }
  k <- k + 1
}

n_min_gut <- dd$propn[k]
df_min_gut_vs_S <- rbind(df_min_gut_vs_S, 
                         data.frame(n_min_gut = n_min_gut, S = 33, C = 0.20, L = 221, foodweb = "Afon Hirnant",
                                    prop_min_gut = n_min_gut/max_ngut, max_ngut = max_ngut, max_TSS = max_TSS,
                                    n_pred = n_pred, mean_prey = mean_prey, sd_prey = sd_prey))





## Coilaco

fw_name <- "Coilaco size_agg"
pred_mat <- readRDS(paste0("data/", fw_name, ".web.Rdata"))$predation.matrix
n_pred <- sum(colSums(pred_mat)!=0)

gut_data <- readRDS(paste0("data/gut_data/", fw_name, "_ind_predator.gut.Rdata"))
gut_data <- gut_data[-c(1,2),]
mean_prey <- mean(colSums(gut_data))
sd_prey <- sd(colSums(gut_data))

dd <- readRDS(paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_ind_predator/dd_co.Rdata"))
max_ngut <- max(dd$propn)
max_TSS <- dd$mean_acc[length(dd$mean_acc)]
threshold_TSS <- threshold_per*max_TSS

k <- 1
repeat{
  if(dd$mean_acc[k] > threshold_TSS){
    N <- k - 1
    break
  }
  k <- k + 1
}

n_min_gut <- dd$propn[k]
df_min_gut_vs_S <- rbind(df_min_gut_vs_S, 
                         data.frame(n_min_gut = n_min_gut, S = 45, C = 0.061, L = 123, foodweb = "Coilaco",
                                    prop_min_gut = n_min_gut/max_ngut, max_ngut = max_ngut, max_TSS = max_TSS,
                                    n_pred = n_pred, mean_prey = mean_prey, sd_prey = sd_prey))



## Guampoe

fw_name <- "Guampoe size_agg"
pred_mat <- readRDS(paste0("data/", fw_name, ".web.Rdata"))$predation.matrix
n_pred <- sum(colSums(pred_mat)!=0)

gut_data <- readRDS(paste0("data/gut_data/", fw_name, "_ind_predator.gut.Rdata"))
gut_data <- gut_data[-c(1,2),]
mean_prey <- mean(colSums(gut_data))
sd_prey <- sd(colSums(gut_data))

dd <- readRDS(paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_ind_predator/dd_gu.Rdata"))
max_ngut <- max(dd$propn)
max_TSS <- dd$mean_acc[length(dd$mean_acc)]
threshold_TSS <- threshold_per*max_TSS

k <- 1
repeat{
  if(dd$mean_acc[k] > threshold_TSS){
    N <- k - 1
    break
  }
  k <- k + 1
}

n_min_gut <- dd$propn[k]
df_min_gut_vs_S <- rbind(df_min_gut_vs_S, 
                         data.frame(n_min_gut = n_min_gut, S = 44, C = 0.072, L = 139, foodweb = "Guampoe",
                                    prop_min_gut = n_min_gut/max_ngut, max_ngut = max_ngut, max_TSS = max_TSS,
                                    n_pred = n_pred, mean_prey = mean_prey, sd_prey = sd_prey))


## Trancura

fw_name <- "Trancura size_agg"
pred_mat <- readRDS(paste0("data/", fw_name, ".web.Rdata"))$predation.matrix
n_pred <- sum(colSums(pred_mat)!=0)

gut_data <- readRDS(paste0("data/gut_data/", fw_name, "_ind_predator.gut.Rdata"))
gut_data <- gut_data[-c(1,2),]
mean_prey <- mean(colSums(gut_data))
sd_prey <- sd(colSums(gut_data))

dd <- readRDS(paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_ind_predator/dd_tr.Rdata"))
max_ngut <- max(dd$propn)
max_TSS <- dd$mean_acc[length(dd$mean_acc)]
threshold_TSS <- threshold_per*max_TSS

k <- 1
repeat{
  if(dd$mean_acc[k] > threshold_TSS){
    N <- k - 1
    break
  }
  k <- k + 1
}

n_min_gut <- dd$propn[k]
df_min_gut_vs_S <- rbind(df_min_gut_vs_S, 
                         data.frame(n_min_gut = n_min_gut, S = 35, C = 0.061, L = 78, foodweb = "Trancura",
                                    prop_min_gut = n_min_gut/max_ngut, max_ngut = max_ngut, max_TSS = max_TSS,
                                    n_pred = n_pred, mean_prey = mean_prey, sd_prey = sd_prey))


# saveRDS(object = df_min_gut_vs_S, file = "results/misc/df_min_gut_vs_S.RDS")
###############################


plot_n_min_gut_vs_S <- ggplot(df_min_gut_vs_S) +
  geom_point(aes(x = S, y = n_min_gut, color = foodweb), size = 5) +
  theme_classic() +
  scale_color_discrete(name = "Food web") +
  xlab("Number of species (S)") +
  ylab("Number of minimum gut")

# ggsave(plot = plot_n_min_gut_vs_S, filename = "inst/report/fig_2021_10_15/plot_n_min_gut_vs_S.png")

plot_n_min_gut_vs_n_max_gut <- ggplot(df_min_gut_vs_S) +
  geom_point(aes(x = max_ngut, y = n_min_gut, color = foodweb), size = 5) +
  theme_classic() +
  scale_color_discrete(name = "Food web") +
  xlab("Number of maximum guts sampled") +
  ylab("Number of minimum gut")


plot_n_min_gut_vs_L <- 
  df_min_gut_vs_S %>%
  # filter(foodweb != "Broadstone Stream") %>%
  ggplot() +
  geom_point(aes(x = L, y = n_min_gut, color = foodweb), size = 5) +
  # geom_smooth(aes(x = L, y = n_min_gut), method = "lm") +
  theme_classic() +
  scale_color_discrete(name = "Food web") +
  xlab("Number of links (L)") +
  ylab("Number of minimum gut")

# ggsave(plot = plot_n_min_gut_vs_L, filename = "inst/report/fig_2021_10_15/plot_n_min_gut_vs_L.png")

lm_ng_L <- 
  df_min_gut_vs_S %>%
  filter(foodweb != "Broadstone Stream") %>%
  lm(formula = n_min_gut ~ L)

summary(lm_ng_L)
anova(lm_ng_L)

lm_ng_L_max_ngut <- lm(n_min_gut ~ ., data = df_min_gut_vs_S[,-5])
  
summary(lm_ng_L_max_ngut)
anova(lm_ng_L_max_ngut)

# ggsave(plot = plot_n_min_gut_vs_L, filename = "inst/report/fig_2021_08_03/plot_n_min_gut_vs_L.png")


plot_n_min_gut_vs_C <- ggplot(df_min_gut_vs_S) +
  geom_point(aes(x = C, y = n_min_gut, color = foodweb), size = 5) +
  # geom_smooth(aes(x = C, y = n_min_gut), method = "lm") +
  theme_classic() +
  scale_color_discrete(name = "Food web") +
  xlab("Connectance (C)") +
  # scale_color_brewer(type = "qual") +
  ylab("Number of minimum gut") 

lm_ng_conn <- lm(log10(n_min_gut) ~ C, data = df_min_gut_vs_S)
summary(lm_ng_conn)
anova(lm_ng_conn)

autoplot(lm_ng_conn)

# ggsave(plot = plot_n_min_gut_vs_C, filename = "inst/report/fig_2021_10_15/plot_n_min_gut_vs_C.png")



plot_n_min_gut_vs_n_pred <- 
  df_min_gut_vs_S %>%
  # filter(foodweb != "Broadstone Stream") %>%
  ggplot() +
  geom_point(aes(x = n_pred, y = n_min_gut, color = foodweb), size = 5) +
  # geom_smooth(aes(x = L, y = n_min_gut), method = "lm") +
  theme_classic() +
  scale_color_discrete(name = "Food web") +
  xlab("Number of predator nodes") +
  ylab("Number of minimum gut")

# ggsave(plot = plot_n_min_gut_vs_n_pred, filename = "inst/report/fig_2021_10_15/plot_n_min_gut_vs_n_pred.png")


plot_prop_min_gut_vs_S <- ggplot(df_min_gut_vs_S) +
  geom_point(aes(x = S, y = prop_min_gut, color = foodweb), size = 5) +
  theme_classic() +
  scale_color_discrete(name = "Food web") +
  xlab("Number of species (S)") +
  ylab("Proportion of minimum gut")


# ggsave(plot = plot_prop_min_gut_vs_S, filename = "inst/report/fig_2021_08_03/plot_prop_min_gut_vs_S.png")

plot_prop_min_gut_vs_L <- ggplot(df_min_gut_vs_S) +
  geom_point(aes(x = L, y = prop_min_gut, color = foodweb), size = 5) +
  theme_classic() +
  scale_color_discrete(name = "Food web") +
  xlab("Number of links") +
  ylab("Proportion of minimum gut")

# ggsave(plot = plot_prop_min_gut_vs_L, filename = "inst/report/fig_2021_08_03/plot_prop_min_gut_vs_L.png")

plot_prop_min_gut_vs_C <- ggplot(df_min_gut_vs_S) +
  geom_point(aes(x = C, y = prop_min_gut, color = foodweb), size = 5) +
  theme_classic() +
  scale_color_discrete(name = "Food web") +
  xlab("Connectance") +
  ylab("Proportion of minimum gut")

# ggsave(plot = plot_prop_min_gut_vs_C, filename = "inst/report/fig_2021_08_03/plot_prop_min_gut_vs_C.png")


lin_mod <- lm(prop_min_gut ~ C, df_min_gut_vs_S)
summary(lin_mod)



plot_n_min_gut_vs_n_pred <- 
  df_min_gut_vs_S %>%
  filter(foodweb != "Broadstone Stream") %>%
  ggplot() +
  geom_point(aes(x = n_pred, y = n_min_gut, color = foodweb), size = 5) +
  # geom_smooth(aes(x = L, y = n_min_gut), method = "lm") +
  theme_bw() +
  xlab("Number of predator") +
  ylab("Number of minimum gut")

lm_ng_n_pred <- 
  df_min_gut_vs_S %>%
  filter(foodweb != "Broadstone Stream") %>%
  lm(formula = n_min_gut ~ n_pred)

summary(lm_ng_n_pred)
anova(lm_ng_n_pred )



plot_n_min_gut_vs_mean_prey <- 
  df_min_gut_vs_S %>%
  # filter(foodweb != "Broadstone Stream") %>%
  ggplot() +
  geom_point(aes(x = mean_prey, y = n_min_gut, color = foodweb), size = 5) +
  # geom_smooth(aes(x = L, y = n_min_gut), method = "lm") +
  theme_bw() +
  xlab("Mean number of prey") +
  ylab("Number of minimum gut")
  

lm_n <- lm(data = df_min_gut_vs_S, formula = n_min_gut ~ mean_prey + L)
summary(lm_n)
anova(lm_n)

# saveRDS(object = df_min_gut_vs_S, file = paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Misc/df_min_gut_vs_S.RDS"))
