# 02.03.2022
# We plot the link accumulation curve for the food webs

fw_name <- "Broadstone Stream size_agg_v2"
# fw_name <- "Celtic Sea size_agg"
# fw_name <- "Tadnoll Brook size_agg"
# fw_name <- "Afon Hirnant size_agg"
# fw_name <- "Coilaco size_agg"
# fw_name <- "Guampoe size_agg"
# fw_name <- "Trancura size_agg"

fname_data <- paste("data/", fw_name, ".web.Rdata", sep="")
fw_data <- readRDS(fname_data)
nspecies <- dim(fw_data$predation.matrix)[1]
complete_pred_mat <- fw_data$predation.matrix
pred_nodes <- which(colSums(complete_pred_mat)>0)

rule <- "ind_predator"
fname_gut <- paste("data/gut_data/", fw_name, "_", rule, ".gut.Rdata", sep="")
gut_data_main <- readRDS(file = fname_gut)

desc_main <- "TSS_gut_ind"

# propn <- c(seq(1, 74, by = 3), 74)   ### Coilaco
# propn <- c(seq(1, 87, by = 3), 87)   ### Guampoe
# propn <- seq(1, 47, by = 2)   ### Trancura

propn <- 1:length(gut_data_main[1,])
n_gut_sample <- 100
links_ngut <- data.frame(l_links = double(), u_links = double(), mean_links = double(),
                       ngut = integer(), nsample = integer())

for(ngut in propn){
  
  fname_sp_ind <- paste("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_", rule, "/diet/", fw_name ,"_",ngut,".gut_index.Rdata", sep = "")
  species_ind <- readRDS(fname_sp_ind)
  links_array <- c()
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
    
    links_calc <- sum(gut_data[,pred_nodes])
    links_array <- rbind(links_array, links_calc)
  }
  
  l_links <- min(links_array)
  u_links <- max(links_array)
  mean_links <- mean(links_array)
  links_ngut <- rbind(links_ngut,
                    data.frame(l_links = l_links, u_links = u_links, mean_links = mean_links, ngut = ngut, nsample = n_gut_sample))
}

# saveRDS(object = links_ngut, file = "../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Celtic Sea size_agg/rule_ind_predator/empirical_links_ngut.Rdata")
# saveRDS(object = links_ngut, file = "../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Broadstone Stream size_agg_v2/rule_ind_predator/empirical_links_ngut.Rdata")
# saveRDS(object = links_ngut, file = "../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Tadnoll Brook size_agg/rule_ind_predator/empirical_links_ngut.Rdata")
# saveRDS(object = links_ngut, file = "../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Afon Hirnant size_agg/rule_ind_predator/empirical_links_ngut.Rdata")
# saveRDS(object = links_ngut, file = "../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Coilaco size_agg/rule_ind_predator/empirical_links_ngut.Rdata")
# saveRDS(object = links_ngut, file = "../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Guampoe size_agg/rule_ind_predator/empirical_links_ngut.Rdata")
# saveRDS(object = links_ngut, file = "../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Trancura size_agg/rule_ind_predator/empirical_links_ngut.Rdata")




ggplot(links_ngut) +
  geom_line(aes(x = ngut, y = mean_links)) +
  geom_ribbon(aes(x = ngut, ymin = l_links, ymax = u_links), alpha = 0.5) +
  theme_bw() 


ggplot(links_ngut) +
  geom_line(aes(x = ngut, y = mean_links, color = as.factor(nsample))) +
  geom_ribbon(aes(x = ngut, ymin = l_links, ymax = u_links, fill = as.factor(nsample)), alpha = 0.5) +
  theme_bw() +
  ylim(c(0,1))


links_ngut_broad <- mutate(links_ngut_broad, foodweb = "Broadstone Stream")
links_ngut_broad$ngut <- links_ngut_broad$ngut/1008
links_ngut_celtic <- mutate(links_ngut_celtic, foodweb = "Celtic Sea")
links_ngut_celtic$ngut <- links_ngut_celtic$ngut/491
links_ngut <- rbind(links_ngut_broad, links_ngut_celtic)


links_ngut_bs <- readRDS("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Broadstone Stream size_agg_v2/rule_ind_predator/empirical_links_ngut.Rdata")
links_ngut_cs <- readRDS("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Celtic Sea size_agg/rule_ind_predator/empirical_links_ngut.Rdata")
links_ngut_tb <- readRDS("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Tadnoll Brook size_agg/rule_ind_predator/empirical_links_ngut.Rdata")
links_ngut_ah <- readRDS("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Afon Hirnant size_agg/rule_ind_predator/empirical_links_ngut.Rdata")
links_ngut_co <- readRDS("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Coilaco size_agg/rule_ind_predator/empirical_links_ngut.Rdata")
links_ngut_gu <- readRDS("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Guampoe size_agg/rule_ind_predator/empirical_links_ngut.Rdata")
links_ngut_tr <- readRDS("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Trancura size_agg/rule_ind_predator/empirical_links_ngut.Rdata")



links_ngut_bs_1 <- mutate(links_ngut_bs, foodweb = "Broadstone Stream")
links_ngut_cs_1 <- mutate(links_ngut_cs, foodweb = "Celtic Sea")
links_ngut_tb_1 <- mutate(links_ngut_tb, foodweb = "Tadnoll Brook")
links_ngut_ah_1 <- mutate(links_ngut_ah, foodweb = "Afon Hirnant")
links_ngut_co_1 <- mutate(links_ngut_co, foodweb = "Coilaco")
links_ngut_gu_1 <- mutate(links_ngut_gu, foodweb = "Guampoe")
links_ngut_tr_1 <- mutate(links_ngut_tr, foodweb = "Trancura")

links_ngut_1 <- rbind(links_ngut_bs_1, links_ngut_cs_1, links_ngut_tb_1, links_ngut_ah_1,
                    links_ngut_co_1, links_ngut_gu_1, links_ngut_tr_1)

links_ngut <- rbind(links_ngut_bs, links_ngut_cs, links_ngut_tb, links_ngut_ah,
                  links_ngut_co, links_ngut_gu, links_ngut_tr)

plot_links_ngut <- ggplot(links_ngut_1) +
  geom_line(aes(x = ngut, y = mean_links, color = foodweb)) +
  geom_ribbon(aes(x = ngut, ymin = l_links, ymax = u_links, fill = foodweb), alpha = 0.5) +
  # facet_wrap(~ foodweb, scales = "free") +
  theme_classic() +
  scale_color_discrete(name = "Food web") +
  scale_fill_discrete(name = "Food web") +
  xlab("Number of gut content data") +
  ylab("Number of links") +
  theme(legend.position = "none") +
  scale_x_continuous(expand = c(0,0))

# ggsave(plot = plot_links_ngut, filename = "results/misc/plot_emplinks_ngut.png", width = 9, height = 6)

#### In Black background ####

plot_links_ngut_black <- ggplot(links_ngut_1) +
  geom_line(aes(x = ngut, y = mean_links, color = foodweb)) +
  geom_ribbon(aes(x = ngut, ymin = l_links, ymax = u_links, fill = foodweb), alpha = 0.7) +
  # facet_wrap(~ foodweb, scales = "free") +
  theme_classic() +
  scale_color_discrete(name = "Food web") +
  scale_fill_discrete(name = "Food web") +
  xlab("Number of predator guts") +
  ylab("Number of trophic links") +
  scale_x_continuous(breaks = as.integer(seq(1, 1008, length = 5))) +
  scale_y_continuous(breaks = as.integer(seq(1, 500, length = 5))) +
  theme(text = element_text(size = 20, face = "bold", color = "white"), plot.title=element_text(hjust=0.5, size = 40, vjust = 3.5),
        panel.background = element_rect(fill = "black", colour = "black"),
        plot.background = element_rect(fill = "black", colour = "black"),
        axis.line = element_line(color  = "white"),
        axis.ticks = element_line(colour = "white"),
        axis.text = element_text(colour = "white"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        plot.margin  = margin(t = 25, l = 10, b = 10),
        legend.background = element_rect(fill = "black", colour = "black"))

# ggsave(plot = plot_links_ngut_black, filename = "results/misc/plot_emplinks_ngut_black.png", width = 9, height = 6)




plot_links_ngut <- ggplot(links_ngut_1) +
  geom_line(aes(x = ngut, y = mean_links, color = foodweb)) +
  geom_ribbon(aes(x = ngut, ymin = l_links, ymax = u_links, fill = foodweb), alpha = 0.7) +
  # facet_wrap(~ foodweb, scales = "free") +
  theme_classic() +
  scale_color_discrete(name = "Food web") +
  scale_fill_discrete(name = "Food web") +
  xlab("Number of predator guts") +
  ylab("Number of observed trophic links") +
  scale_x_continuous(breaks = as.integer(seq(1, 1008, length = 5))) +
  scale_y_continuous(breaks = as.integer(seq(1, 500, length = 5))) +
  theme(text = element_text(size = 20), plot.title=element_text(hjust=0.5, size = 40, vjust = 3.5),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        plot.margin  = margin(t = 25, l = 10, b = 10))

# ggsave(plot = plot_links_ngut, filename = "results/misc/plot_emplinks_ngut.png", width = 9, height = 6)
