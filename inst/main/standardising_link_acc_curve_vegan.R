# 2022.05.23
# We standardise the link accumulation curves across the food webs with the help of vegan package

library(vegan)
library(ggplot2)
library(dplyr)
library(zoo)

fw_ngut <- data.frame(foodweb = character(), max_gut = integer())

#### Broadstone Stream ####
bs_pred <- readRDS("../C2_comp_qnty/data/Broadstone Stream size_agg_v2.web.Rdata")$predation.matrix
nspecies_bs <- dim(bs_pred)[1]
bs_gut <- readRDS("../C2_comp_qnty/data/gut_data/Broadstone Stream size_agg_v2_ind_predator.gut.Rdata")
max_gut_bs <- dim(bs_gut)[2]
nlinks_bs <- sum(bs_pred)
link_mat_bs <- matrix(data = 0, nrow = max_gut_bs, ncol = nlinks_bs,  dimnames = list(1:max_gut_bs, 1:nlinks_bs))

fw_ngut <- data.frame(foodweb = "Broadstone Stream", max_gut = max_gut_bs)

#### Link mat matrix for Broadstone Stream ####

k <- 1
for(pred_i in 1:nspecies_bs){
  for(prey_j in 1:nspecies_bs){
    if(bs_pred[prey_j, pred_i] == 1){
      colnames(link_mat_bs)[k] <- paste0("P", pred_i, "p", prey_j)
      print(k)
      k <- k + 1
    }
  }
}


#### Filling up the values in the link_mat matrix ####

for(gut_index in 1:max_gut_bs){
  for(prey_j in 1:nspecies_bs){
    gut_i_prey_j <- bs_gut[prey_j + 2, gut_index]
    if(gut_i_prey_j == 1){
      pred_index <-  bs_gut[, gut_index][2]
      prey_index <- prey_j
      gut_index <- gut_index
      col_name_ind <- paste0("P", pred_index, "p", prey_index)
      link_mat_bs[,col_name_ind][gut_index] <- 1
    }
  }
}

#### the vegan package ####

link_acc_curve_bs <- specaccum(link_mat_bs)
plot(link_acc_curve_bs)

link_acc_df <- data.frame(ngut = link_acc_curve_bs$sites,
                          nlink = link_acc_curve_bs$richness,
                          foodweb = "Broadstone Stream")

#### Other food web #####

fw_all <- c("Celtic Sea", "Tadnoll Brook", "Afon Hirnant", "Coilaco", "Guampoe", "Trancura")

for(fw_name in fw_all){
  fw_pred <- readRDS(paste0("../C2_comp_qnty/data/", fw_name, " size_agg.web.Rdata"))$predation.matrix
  nspecies <- dim(fw_pred)[1]
  fw_gut <- readRDS(paste0("../C2_comp_qnty/data/gut_data/", fw_name, " size_agg_ind_predator.gut.Rdata"))
  
  ngut <- dim(fw_gut)[2]
  nlinks <- sum(fw_pred)
  
  fw_ngut <- rbind(fw_ngut, 
                   data.frame(foodweb = fw_name, max_gut = ngut))
  
  link_mat <- matrix(data = 0, nrow = ngut, ncol = nlinks,  dimnames = list(1:ngut, 1:nlinks))
  
  ### Renaming the column names of the link_mat matrix
  k <- 1
  for(pred_i in 1:nspecies){
    for(prey_j in 1:nspecies){
      if(fw_pred[prey_j, pred_i] == 1){
        colnames(link_mat)[k] <- paste0("P", pred_i, "p", prey_j)
        print(k)
        k <- k + 1
      }
    }
  }
  
  
  #### Filling up the values in the link_mat matrix ####
  
  for(gut_index in 1:ngut){
    for(prey_j in 1:nspecies){
      gut_i_prey_j <- fw_gut[prey_j + 2, gut_index]
      if(gut_i_prey_j == 1){
        pred_index <-  fw_gut[, gut_index][2]
        prey_index <- prey_j
        gut_index <- gut_index
        col_name_ind <- paste0("P", pred_index, "p", prey_index)
        link_mat[,col_name_ind][gut_index] <- 1
      }
    }
  }
  
  
  
  #### Using the vegan package ####
  link_acc_curve <- specaccum(link_mat)
  plot(link_acc_curve)
  
  ngut_array <- 1:2500
  
  MODELS <- c("arrhenius", "gleason", "gitay", "lomolino",
              "asymp", "gompertz", "michaelis-menten", "logis",
              "weibull")
  
  sel_model <- character()
  aic_val <- Inf
  for(model in MODELS){
    mod_fit <- fitspecaccum(link_acc_curve, model = model)
    aic_val_loc <- AIC(mod_fit)
    if(aic_val_loc < aic_val){
      sel_model <- model
    }
  }
  
  mod_fit <- fitspecaccum(link_acc_curve, model = sel_model)
  
  pred_mod <- predict(object = mod_fit, newdata = ngut_array)
  
  link_acc_df <- rbind(link_acc_df,
                       data.frame(ngut = ngut_array,
                                  nlink = pred_mod,
                                  foodweb = fw_name))
}



ggplot(link_acc_df) +
  geom_line(aes(x = ngut, y = nlink, color = foodweb))


#### Rolling average ####

link_acc_df_ra <- link_acc_df %>%
  group_by(foodweb) %>%
  mutate(ra_mean_links = rollmean(nlink, 30, fill = NA, align='right'))
  
ggplot(link_acc_df_ra) +
  geom_line(aes(x = ngut, y = nlink, color = foodweb))

#### Compute the slope of nlinks vs ngut for the food webs #####

slope_link_acc_df <- link_acc_df_ra %>%
  mutate(slope = nlink - lag(nlink))

min_slope_bs <- min(slope_link_acc_df$slope[slope_link_acc_df$foodweb == "Broadstone Stream"], na.rm = TRUE)

ggplot(slope_link_acc_df) +
  geom_line(aes(x = ngut, y = slope, color = foodweb)) +
  geom_abline(slope = 0, intercept = min_slope_bs) +
  ylim(c(0, 1))

corrected_max_gut_df <- slope_link_acc_df %>%
  filter(slope >= min_slope_bs) %>%
  group_by(foodweb) %>%
  summarise(corrected_max_gut = max(ngut)) 


factor_df <- merge(corrected_max_gut_df, fw_ngut, by = "foodweb")

factor_df <- factor_df %>%
  mutate(factor = corrected_max_gut/max_gut)


#### Computing the corrected number of links-- which takes into account the undersampling factor ####


link_acc_df_ra_max_gut <- merge(x = link_acc_df_ra, y = corrected_max_gut_df, by = "foodweb")

corrected_links_df <- link_acc_df_ra_max_gut %>%
  filter(ngut == corrected_max_gut) %>%
  mutate(corrected_links = as.integer(ra_mean_links)) %>%
  select(-c(ngut, nlink, ra_mean_links, corrected_max_gut))
  

factor_df <- merge(corrected_links_df, factor_df, by = "foodweb")


# saveRDS(factor_df, file = "results/misc/factor_all_vegan.RDS")

factor_df <- readRDS(file = "results/misc/factor_all_vegan.RDS")


#### Taking into account the factor ####

df_min_gut_vs_S <- readRDS("results/misc/df_min_gut_vs_S.RDS")

df_min_gut_vs_S_factor <- merge(x = df_min_gut_vs_S, y = factor_df, by = "foodweb")

df_min_gut_vs_S_factor <- df_min_gut_vs_S_factor %>%
  mutate(corrected_min_gut = n_min_gut*factor)

df_min_gut_vs_S_factor <- df_min_gut_vs_S_factor %>%
  mutate(S_sq = S^2)
#### Corrected min number of gut vs S ####

ggplot(df_min_gut_vs_S_factor) +
  geom_point(aes(x = S_sq, y = corrected_min_gut), size = 5) +
  xlab(TeX("$(Number~of~species)^2$")) +
  ylab("Corrected minimum number of predator guts") +
  labs(color = "Food web") +
  geom_smooth(aes(x = S_sq, y = corrected_min_gut), method = "lm") +
  theme_classic() +
  theme(text = element_text(size = 20), plot.title=element_text(hjust=0.5, size = 40, vjust = 3.5),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        plot.margin  = margin(t = 25, l = 10, b = 10)) 

# ggsave(filename = "results/misc/plot_corrected_n_min_gut_vs_S_sq.png", dpi = 500,
#        width = 12, height = 7)

lm_S_sq <- lm(corrected_min_gut ~ S_sq, data = df_min_gut_vs_S_factor)

summary(lm_S_sq)


lm_S <- lm(corrected_min_gut ~ S, data = df_min_gut_vs_S_factor)

summary(lm_S)
