#19.11.2020
#Rule ind
#We plot TSS with number of predator diets sampled and their average body size

fw_name <- "Broadstone Stream size_agg_v2"
dir_N <- 1e5
dir_tol <- 2 #different for ind and rand
n_gut_sample <- 100  #different for ind and rand
propn <- seq(8, 1008, by = 20)
n_sel <- 1
n <- length(propn)
l_acc <- numeric(n)
r_acc <- numeric(n)
mean_acc <- numeric(n)

desc_main <- "TSS_gut_ind"
rule <- "ind"
l_ind <- 1

arr_M <- c()
arr_ndiet <- c()
arr_TSS <- c()
arr_range_M <- c()
  
#Reading the diet matriX which also contains the body size data
diet_mat <- readRDS(paste("data/gut_data/", fw_name, "_", rule, ".gut.Rdata", sep = ""))
bs_data <- diet_mat[1,]
for(ndiet in propn){
  
  desc <- paste(desc_main, '_l', ndiet, sep = '')
  # dir_main <- paste("results/rejection/",fw_name,"/", "rule_", rule,'/N=', dir_N, '_tol=', dir_tol, '_n_diet = ', i,
  #                   sep = "")
  dir_main <- paste("../../../Downloads/rule_ind",'/N=', dir_N, '_tol=', dir_tol, '_n_diet = ', ndiet,
                    sep = "")
  sim_TSS <- c()
  
  #Extracting bodysize data
  fname_sp_ind <- paste("results/rejection/",fw_name,"/rule_ind/diet/", fw_name ,"_", ndiet,".gut_index.Rdata", sep = "")
  species_ind <- readRDS(fname_sp_ind)
  for(n_sample  in 1:n_gut_sample){
    
    #Extracting TSS from an output file
    dirnam <- paste(c(dir_main, '/rn_sample=', n_sample,'_N=', dir_N, '_tol=', dir_tol,"_", desc), collapse = '')
    fname <- paste(dirnam, "/", fw_name, ".Rdata", sep = "")
    prop_acc <- readRDS(fname)
    gut_TSS <- 1 - prop_acc$dist
    gut_TSS_indexes <- order(gut_TSS, decreasing = TRUE)[1:n_sel]
    sim_TSS_temp <- 1 - prop_acc$acc_ss[gut_TSS_indexes]
    
    arr_TSS <- append(arr_TSS, sim_TSS_temp)
    arr_ndiet <- append(arr_ndiet, ndiet)
    
    species_ind_single <- species_ind[n_sample,]
    bs_data_single <- bs_data[species_ind_single]
    mean_bs <- mean(exp(bs_data_single)) #since the body size where in log scale
    arr_M <- append(arr_M, mean_bs)
    
    range_M <- range(exp(bs_data_single))
    diff_range_M <- range_M[2] - range_M[1]
    arr_range_M <- append(arr_range_M, diff_range_M)
    # sim_TSS <- c(sim_TSS, sim_TSS_temp)
  }
  
  # range_acc <- range(sim_TSS)
  # l_acc[l_ind] <- range_acc[1]
  # r_acc[l_ind] <- range_acc[2]
  # mean_acc[l_ind] <- mean(sim_TSS)
  # l_ind <- l_ind+1
}

dd_org <- data.frame(diet = arr_ndiet, M = arr_M, TSS = arr_TSS)

dd_lower <- dd %>%
  filter(arr_M <= 1.586283) %>%
  mutate(bodysize = "less than mean")

dd_higher <- dd %>%
  filter(arr_M > 1.586283) %>%
  mutate(bodysize = "higher than mean")

dd <- rbind(dd_lower, dd_higher)

dd_mut <- dd %>%
  group_by(diet, bodysize) %>%
  mutate(mean_TSS = mean(TSS), lower_TSS = range(TSS)[1], upper_TSS = range(TSS)[2]) %>%
  select(-c("M", "TSS"))


dd_plot <- ggplot(dd_mut) +
  # geom_line(aes(x = propn, y = mean_acc)) +
  geom_line(aes(x = diet, y = mean_TSS, color = bodysize), 
            position=position_dodge(width = 3)) +
  geom_ribbon(aes(x = diet, ymin = lower_TSS, ymax = upper_TSS, fill = bodysize), alpha = 0.5, show.legend = FALSE,
              position=position_dodge(width = 3)) +
  xlab("Number of different predator guts sampled") +
  ylab("TSS (predicted food web, observed food web)") +
  # theme_classic() +
  scale_color_manual(name = "gut data", values = c("red", "blue")) +
  theme(axis.text = element_text(family = "Times New Roman", size = 20),
        axis.title = element_text(family = "Times New Roman", size = 20),
        legend.title = element_text(family = "Times New Roman", size = 20),
        legend.text = element_text(family = "Times New Roman", size = 20)) +
  scale_x_discrete(limits=propn[c(1,11,21,31,41,51)])



ggsave(filename = "results/rejection/Broadstone Stream size_agg_v2/rule_ind/TSS_with_n_pred_prop_bs.png", plot = dd_plot,
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


ggplot(dd_higher) +
  geom_point(aes(x = diet, y = TSS))



