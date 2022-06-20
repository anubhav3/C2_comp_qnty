# 2022.05.04
# We plot the links accumulation curve wrt to the number of prey items

library(ggplot2)
library(dplyr)
library(zoo)

fw_list <- c("Broadstone Stream size_agg_v2",
             "Celtic Sea size_agg",
             "Tadnoll Brook size_agg",
             "Afon Hirnant size_agg",
             "Coilaco size_agg",
             "Guampoe size_agg",
             "Trancura size_agg")

links_nitems <- data.frame(l_links = double(), u_links = double(), mean_links = double(),
                           nitems = integer(), nsample = integer(), fw = character())
for(fw_name in  fw_list){
  fname_data <- paste("data/", fw_name, ".web.Rdata", sep="")
  fw_data <- readRDS(fname_data)
  nspecies <- dim(fw_data$predation.matrix)[1]
  complete_pred_mat <- fw_data$predation.matrix
  pred_nodes <- which(colSums(complete_pred_mat)>0)
  
  rule <- "ind_predator"
  fname_gut <- paste("data/gut_data/", fw_name, "_", rule, ".gut.Rdata", sep="")
  gut_data_main <- readRDS(file = fname_gut)
  
  desc_main <- "TSS_gut_ind"
  
  nitems <- sum(gut_data_main[-c(1,2),])
  n_gut <- dim(gut_data_main[-c(1,2),])[2]
  n_nodes <- dim(gut_data_main[-c(1,2),])[1]
  items_data_main <- matrix(data = 0, nrow = n_nodes+2, ncol = nitems)
  
  k <- 1
  for(ith_gut in 1:n_gut){
    ith_gut_info <- gut_data_main[-c(1,2),ith_gut]
    
    item_ind <- which(ith_gut_info == 1)
    for(ith_item in item_ind){
      items_data_main[1,k] <-  gut_data_main[1,ith_gut]
      items_data_main[2,k] <-  gut_data_main[2,ith_gut]
      items_data_main[ith_item+2,k] <-  gut_data_main[2 + ith_item, ith_gut]
      k <- k + 1
    }
  }
  
  
  propn <- 1:length(items_data_main[1,])
  n_gut_sample <- 100
  
  for(nitems in propn){
    
    links_array <- c()
    for(n_sample in 1:n_gut_sample){
      
      indexes <- sample(x = propn, size = nitems, replace = FALSE)
      gut_data <- matrix(0, nrow = nspecies, ncol = nspecies)
      
      partial_diet <- items_data_main[,indexes]
      
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
    links_nitems <- rbind(links_nitems,
                          data.frame(l_links = l_links, u_links = u_links, mean_links = mean_links, nitems = nitems, nsample = n_gut_sample,
                                     fw = fw_name))
  }
  print(fw_name)
}


# saveRDS(object = links_nitems, file = "results/links_nitems.RDS")

links_nitems <- readRDS("results/links_nitems.RDS")

plot_links_nitems <- ggplot(links_nitems) +
  geom_line(aes(x = nitems, y = mean_links, color = fw)) +
  geom_ribbon(aes(x = nitems, ymin = l_links, ymax = u_links, fill = fw), alpha = 0.3) +
  # geom_point(aes(x = nitems, y = nitems), size = 0.05) +
  theme_bw() +
  xlab("Number of prey items") +
  ylab("Number of trophic links") +
  labs(color = "Food web", fill = "Food web")

# ggsave(filename = "results/misc/plot_emplinks_nitems.png", plot = plot_links_nitems)

#### Smoothening the curves by taking rolling average ####

links_nitems_ra <- links_nitems %>%
  group_by(fw) %>%
  mutate(ra_mean_links = rollmean(mean_links, 30, fill = NA, align='right'))

plot_links_nitems_ra <- ggplot(links_nitems_ra) +
  geom_line(aes(x = nitems, y = ra_mean_links, color = fw)) +
  xlab("Number of prey items") +
  ylab("Number of trophic links") +
  labs(color = "Food web") +
  theme_bw()
  
# ggsave(filename = "results/misc/plot_emplinks_nitems_ra.png", plot = plot_links_nitems_ra)

#### Computing the slope of the mean_links vs nitems ####

plot_slope_nitems <- links_nitems_ra %>%
  mutate(slope_mean_links  = ra_mean_links - lag(ra_mean_links)) %>%
  ggplot() +
  geom_line(aes(x = nitems, y = slope_mean_links, color = fw), alpha = 0.7) +
  xlab("Number of prey items") +
  ylab("Slope of link accumulation curve") +
  labs(color = "Food web") +
  theme_bw()

# ggsave(filename = "results/misc/plot_slope_emplinks_nitems.png", plot = plot_slope_nitems)


#### Smoothening the slope curves ####

slope_links_nitems_ra <- links_nitems_ra %>%
  mutate(slope_mean_links  = ra_mean_links - lag(ra_mean_links)) %>%
  group_by(fw) %>%
  mutate(ra_slope_mean_links = rollmean(slope_mean_links, 20, fill = NA, align='right'))

plot_slope_links_nitems_ra <- ggplot(slope_links_nitems_ra) +
  geom_line(aes(x = nitems, y = ra_slope_mean_links, color = fw)) +
  xlab("Number of prey items") +
  ylab("Slope of link accumulation curve") +
  labs(color = "Food web")

# ggsave(filename = "results/misc/plot_slope_emplinks_nitems_ra.png", plot = plot_slope_links_nitems_ra)

#### Smoothening the Broadstone Stream curve separately ####

slope_links_nitems_ra_bs <- links_nitems_ra %>%
  filter(fw == "Broadstone Stream size_agg_v2") %>%
  mutate(slope_mean_links  = ra_mean_links - lag(ra_mean_links)) %>%
  group_by(fw) %>%
  mutate(ra_slope_mean_links = rollmean(slope_mean_links, 100, fill = NA, align='right'))

plot_slope_links_nitems_ra_bs <- ggplot(slope_links_nitems_ra_bs) +
  geom_line(aes(x = nitems, y = ra_slope_mean_links, color = fw)) +
  xlab("Number of prey items") +
  ylab("Slope of link accumulation curve") +
  labs(color = "Food web")

# ggsave(filename = "results/misc/plot_slope_emplinks_nitems_ra_bs.png", plot = plot_slope_links_nitems_ra_bs)

#### Computing the factor

min_slope <- min(slope_links_nitems_ra_bs$ra_slope_mean_links, na.rm = TRUE)
max_nitems <- max(slope_links_nitems_ra_bs$nitems)

ggplot(slope_links_nitems_ra_bs) +
  geom_line(aes(x = nitems, y = ra_slope_mean_links, color = fw)) +
  geom_hline(yintercept = min_slope)

min_slope_all <- slope_links_nitems_ra %>%
  group_by(fw) %>%
  summarise(min_slope = min(ra_slope_mean_links, na.rm = TRUE))


#### Computing the min nitems for all the other food webs ####

factor_all <- data.frame(fw = character(), factor = double())

for(fw in fw_list){
  min_slope_fw <- min_slope_all$min_slope[min_slope_all$fw == fw]
  
  slope_links_nitems_ra_fw <- slope_links_nitems_ra %>%
    filter(fw == fw)
  
  nitems <- dim(slope_links_nitems_ra_bs)[1]
  max_nitems_fw <- max(slope_links_nitems_ra_fw$nitems)
  
  for(i in 1:nitems){
    row_single <- slope_links_nitems_ra_bs[i,]
    if(is.na(row_single$slope_mean_links) != TRUE){
      if(row_single$slope_mean_links < min_slope_fw){
        nitems_fw <- row_single$nitems
        break
      }
    }
    print(i)
  }
  factor <- max_nitems/nitems_fw
  factor_all <- rbind(factor_all, 
                           data.frame(fw = fw, factor = factor))
}


factor_all$factor[factor_all$fw == "Broadstone Stream size_agg_v2"] <- 1

names(factor_all) <- c("foodweb", "factor")

# saveRDS(object = factor_all, file = "results/misc/factor_all.RDS")

df_min_gut_vs_S <- readRDS("results/misc/df_min_gut_vs_S.RDS")


df_min_gut_vs_S$foodweb[df_min_gut_vs_S$foodweb == "Broadstone Stream"] = "Broadstone Stream size_agg_v2"
df_min_gut_vs_S$foodweb[df_min_gut_vs_S$foodweb == "Celtic Sea"] = "Celtic Sea size_agg"
df_min_gut_vs_S$foodweb[df_min_gut_vs_S$foodweb == "Tadnoll Brook"] = "Tadnoll Brook size_agg"
df_min_gut_vs_S$foodweb[df_min_gut_vs_S$foodweb == "Afon Hirnant"] = "Afon Hirnant size_agg"
df_min_gut_vs_S$foodweb[df_min_gut_vs_S$foodweb == "Coilaco"] = "Coilaco size_agg"
df_min_gut_vs_S$foodweb[df_min_gut_vs_S$foodweb == "Guampoe"] = "Guampoe size_agg"
df_min_gut_vs_S$foodweb[df_min_gut_vs_S$foodweb == "Trancura"] = "Trancura size_agg"

df_min_gut_vs_S_factor <- merge(x = df_min_gut_vs_S, y = factor_all, by = "foodweb")

df_min_gut_vs_S_factor <- df_min_gut_vs_S_factor %>%
  mutate(corrected_min_gut = n_min_gut*factor)

#### Corrected min number of gut vs S ####

plot_corrected_mingut_vs_S <- ggplot(df_min_gut_vs_S_factor) +
  geom_point(aes(x = S, y = corrected_min_gut, color = foodweb), size = 5) +
  xlab("Number of species (S)") +
  ylab("Corrected minimum number of gut") +
  labs(color = "Food web") +
  theme_bw()
  # geom_smooth(aes(x = S, y = corrected_min_gut), method = "lm") +

# ggsave(filename = "results/misc/plot_corrected_mingut_vs_S.png", plot = plot_corrected_mingut_vs_S)

#### Corrected min number of gut vs L ####

plot_corrected_mingut_vs_L <- ggplot(df_min_gut_vs_S_factor) +
  geom_point(aes(x = L, y = corrected_min_gut, color = foodweb), size = 5) +
  xlab("Number of trophic links (L)") +
  ylab("Corrected minimum number of gut") +
  labs(color = "Food web") +
  theme_bw()

# ggsave(filename = "results/misc/plot_corrected_mingut_vs_L.png", plot = plot_corrected_mingut_vs_L)

#### Corrected min number of gut vs connectance ####

plot_corrected_mingut_vs_C <- ggplot(df_min_gut_vs_S_factor) +
  geom_point(aes(x = C, y = corrected_min_gut, color = foodweb), size = 5) +
  xlab("Connectance (C)") +
  ylab("Corrected minimum number of gut") +
  labs(color = "Food web") +
  theme_bw()

# ggsave(filename = "results/misc/plot_corrected_mingut_vs_C.png", plot = plot_corrected_mingut_vs_C)

ggplot(df_min_gut_vs_S_factor) +
  geom_point(aes(x = C, y = corrected_min_gut, color = foodweb), size = 5)


lm_gut_vs_S <- lm(data = df_min_gut_vs_S_factor, formula = corrected_min_gut ~ S)
summary(lm_gut_vs_S)
