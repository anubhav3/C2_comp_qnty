# 04.05.2021
# Compute TSS for partial food web wrt ngut

# fw_name <- "Celtic Sea size_agg"
# fw_name <- "Broadstone Stream size_agg_v2"
# fw_name <- "Afon Hirnant size_agg"
# fw_name <- "Coilaco size_agg"
# fw_name <- "Guampoe size_agg"
fw_name <- "Trancura size_agg"

fname_data <- paste("data/", fw_name, ".web.Rdata", sep="")
fw_data <- readRDS(fname_data)
nspecies <- dim(fw_data$predation.matrix)[1]
complete_pred_mat <- fw_data$predation.matrix
pred_nodes <- which(colSums(complete_pred_mat)>0)

rule <- "ind_predator"
fname_gut <- paste("data/gut_data/", fw_name, "_", rule, ".gut.Rdata", sep="")
gut_data_main <- readRDS(file = fname_gut)

desc_main <- "TSS_gut_ind"

propn <- c(seq(1, 74, by = 3), 74)   ### Coilaco
propn <- c(seq(1, 87, by = 3), 87)   ### Guampoe
propn <- seq(1, 47, by = 2)   ### Trancura

n_gut_sample <- 100
TSS_ngut <- data.frame(l_TSS = double(), u_TSS = double(), mean_TSS = double(),
                       ngut = integer(), nsample = integer())

for(ngut in propn){
  
  fname_sp_ind <- paste("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_", rule, "/diet/", fw_name ,"_",ngut,".gut_index.Rdata", sep = "")
  species_ind <- readRDS(fname_sp_ind)
  TSS_array <- c()
  for(n_sample in 1:n_gut_sample){
    
    indexes <- species_ind[n_sample,]
    gut_data <- matrix(0, nrow = nspecies, ncol = nspecies)
    
    partial_diet <- gut_data_main[,indexes]
    
    if(length(indexes) != 1){
      
      pred_ind <- partial_diet[2,]
      uniq_ind <- sort(unique(partial_diet[2,]))
      for(ind in uniq_ind){
        local_ind <- which(pred_ind == ind)
        if(length(local_ind) > 1)
        {
          diet_temp <- rowSums(partial_diet[3:(nspecies+2),local_ind])
          diet_temp[which(diet_temp > 1)] = 1
          gut_data[,ind] <-  diet_temp
        }
        else
        {
          gut_data[,ind] <- partial_diet[3:(nspecies+2),local_ind]
        }
      }
    }
    
    else
    {
      pred_ind <- partial_diet[2]
      uniq_ind <- pred_ind
      gut_data[,pred_ind] <- partial_diet[3:(nspecies+2)]
    }
    
    TSS_calc <- TSS_func(ss_sim = gut_data[,pred_nodes], ss_real = complete_pred_mat[,pred_nodes])
    TSS_array <- rbind(TSS_array, TSS_calc)
  }
  
  l_TSS <- min(TSS_array)
  u_TSS <- max(TSS_array)
  mean_TSS <- mean(TSS_array)
  TSS_ngut <- rbind(TSS_ngut,
                    data.frame(l_TSS = l_TSS, u_TSS = u_TSS, mean_TSS = mean_TSS, ngut = ngut, nsample = n_gut_sample))
}

# saveRDS(object = TSS_ngut, file = "../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Celtic Sea size_agg/rule_ind_predator/empirical_TSS_ngut.Rdata")
# saveRDS(object = TSS_ngut, file = "../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Broadstone Stream size_agg_v2/rule_ind_predator/empirical_TSS_ngut.Rdata")
# saveRDS(object = TSS_ngut, file = "../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Tadnoll Brook size_agg/rule_ind_predator/empirical_TSS_ngut.Rdata")
# saveRDS(object = TSS_ngut, file = "../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Afon Hirnant size_agg/rule_ind_predator/empirical_TSS_ngut.Rdata")
# saveRDS(object = TSS_ngut, file = "../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Coilaco size_agg/rule_ind_predator/empirical_TSS_ngut.Rdata")
# saveRDS(object = TSS_ngut, file = "../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Guampoe size_agg/rule_ind_predator/empirical_TSS_ngut.Rdata")
# saveRDS(object = TSS_ngut, file = "../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Trancura size_agg/rule_ind_predator/empirical_TSS_ngut.Rdata")




ggplot(TSS_ngut) +
  geom_line(aes(x = ngut, y = mean_TSS)) +
  geom_ribbon(aes(x = ngut, ymin = l_TSS, ymax = u_TSS), alpha = 0.5) +
  theme_bw() +
  ylim(c(0,1))


ggplot(TSS_ngut) +
  geom_line(aes(x = ngut, y = mean_TSS, color = as.factor(nsample))) +
  geom_ribbon(aes(x = ngut, ymin = l_TSS, ymax = u_TSS, fill = as.factor(nsample)), alpha = 0.5) +
  theme_bw() +
  ylim(c(0,1))


TSS_ngut_broad <- mutate(TSS_ngut_broad, foodweb = "Broadstone Stream")
TSS_ngut_broad$ngut <- TSS_ngut_broad$ngut/1008
TSS_ngut_celtic <- mutate(TSS_ngut_celtic, foodweb = "Celtic Sea")
TSS_ngut_celtic$ngut <- TSS_ngut_celtic$ngut/491
TSS_ngut <- rbind(TSS_ngut_broad, TSS_ngut_celtic)


TSS_ngut_bs <- readRDS("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Broadstone Stream size_agg_v2/rule_ind_predator/empirical_TSS_ngut.Rdata")
TSS_ngut_cs <- readRDS("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Celtic Sea size_agg/rule_ind_predator/empirical_TSS_ngut.Rdata")
TSS_ngut_tb <- readRDS("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Tadnoll Brook size_agg/rule_ind_predator/empirical_TSS_ngut.Rdata")
TSS_ngut_ah <- readRDS("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Afon Hirnant size_agg/rule_ind_predator/empirical_TSS_ngut.Rdata")
TSS_ngut_co <- readRDS("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Coilaco size_agg/rule_ind_predator/empirical_TSS_ngut.Rdata")
TSS_ngut_gu <- readRDS("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Guampoe size_agg/rule_ind_predator/empirical_TSS_ngut.Rdata")
TSS_ngut_tr <- readRDS("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Trancura size_agg/rule_ind_predator/empirical_TSS_ngut.Rdata")


TSS_ngut_bs_1 <- mutate(TSS_ngut_bs, foodweb = "Broadstone Stream", propn = ngut/1008)
TSS_ngut_cs_1 <- mutate(TSS_ngut_cs, foodweb = "Celtic Sea", propn = ngut/491)
TSS_ngut_tb_1 <- mutate(TSS_ngut_tb, foodweb = "Tadnoll Brook", propn = ngut/688)
TSS_ngut_ah_1 <- mutate(TSS_ngut_ah, foodweb = "Afon Hirnant", propn = ngut/175)
TSS_ngut_co_1 <- mutate(TSS_ngut_co, foodweb = "Coilaco", propn = ngut/74)
TSS_ngut_gu_1 <- mutate(TSS_ngut_gu, foodweb = "Guampoe", propn = ngut/87)
TSS_ngut_tr_1 <- mutate(TSS_ngut_tr, foodweb = "Trancura", propn = ngut/47)

TSS_ngut_1 <- rbind(TSS_ngut_bs_1, TSS_ngut_cs_1, TSS_ngut_tb_1, TSS_ngut_ah_1,
                  TSS_ngut_co_1, TSS_ngut_gu_1, TSS_ngut_tr_1)

TSS_ngut <- rbind(TSS_ngut_bs, TSS_ngut_cs, TSS_ngut_tb, TSS_ngut_ah,
                  TSS_ngut_co, TSS_ngut_gu, TSS_ngut_tr)

plot_TSS_ngut <- ggplot(TSS_ngut_1) +
  geom_line(aes(x = ngut, y = mean_TSS, color = foodweb)) +
  geom_ribbon(aes(x = ngut, ymin = l_TSS, ymax = u_TSS, fill = foodweb), alpha = 0.5) +
  facet_wrap(~ foodweb, scales = "free") +
  theme_classic() +
  scale_color_discrete(name = "Food web") +
  scale_fill_discrete(name = "Food web") +
  ylim(c(0,1)) +
  xlab("Number of gut content data") +
  ylab("True skill statistics") +
  theme(legend.position = "none") +
  scale_x_continuous(expand = c(0,0))

# ggsave(plot = plot_TSS_ngut, filename = "results/misc/plot_empTSS_ngut.png", width = 9, height = 6)
