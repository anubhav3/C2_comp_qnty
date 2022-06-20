# 2022.05.16
# We standardise link accumulation curves across the food webs i.e. we estimate the number of gut content data for all the 
# food webs that would have resulted in a fully sampled food web

library(dplyr)
library(ggplot2)
library(zoo)

foodweb_list <- c("Broadstone Stream",
             "Celtic Sea",
             "Tadnoll Brook",
             "Afon Hirnant",
             "Coilaco",
             "Guampoe",
             "Trancura")

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

links_ngut <- rbind(links_ngut_bs_1, links_ngut_cs_1, links_ngut_tb_1, links_ngut_ah_1,
                      links_ngut_co_1, links_ngut_gu_1, links_ngut_tr_1)




plot_links_ngut <- ggplot(links_ngut) +
  geom_line(aes(x = ngut, y = mean_links, color = foodweb)) +
  geom_ribbon(aes(x = ngut, ymin = l_links, ymax = u_links, fill = foodweb), alpha = 0.5) +
  # facet_wrap(~ foodweb, scales = "free") +
  theme_classic() +
  scale_color_discrete(name = "Food web") +
  scale_fill_discrete(name = "Food web") +
  xlab("Number of predator guts") +
  ylab("Number of links") +
  scale_x_continuous(breaks = c(1, 250, 500, 750, 1000, 1250)) +
  scale_y_continuous(breaks = c(1, 100, 200, 300, 400, 500)) +
  labs(color = "Food web", fill = "Food web")

# ggsave(filename = "results/misc/plot_emplinks_ngut.png", plot = plot_links_ngut)

#### Smoothening the curves by taking rolling average ####

links_ngut_ra <- links_ngut %>%
  group_by(foodweb) %>%
  mutate(ra_mean_links = rollmean(mean_links, 30, fill = NA, align='right'))

plot_links_ngut_ra <- ggplot(links_ngut_ra) +
  geom_line(aes(x = ngut, y = ra_mean_links, color = foodweb)) +
  xlab("Number of predator guts") +
  ylab("Number of trophic links") +
  labs(color = "Food web") +
  scale_x_continuous(breaks = c(1, 250, 500, 750, 1000, 1250)) +
  scale_y_continuous(breaks = c(1, 100, 200, 300, 400, 500)) +
  theme_bw()

# ggsave(filename = "results/misc/plot_emplinks_ngut_ra.png", plot = plot_links_ngut_ra)


#### Computing the slope of the mean_links vs ngut ####

plot_slope_ngut <- links_ngut_ra %>%
  mutate(slope_mean_links  = ra_mean_links - lag(ra_mean_links)) %>%
  ggplot() +
  geom_line(aes(x = ngut, y = slope_mean_links, color = foodweb), alpha = 0.7) +
  xlab("Number of predator guts") +
  ylab("Slope of link accumulation curve") +
  labs(color = "Food web") +
  theme_bw() +
  scale_x_continuous(breaks = c(1, 250, 500, 750, 1000, 1250))

# ggsave(filename = "results/misc/plot_slope_emplinks_ngut.png", plot = plot_slope_ngut)


#### Smoothening the slope curves ####

slope_links_ngut_ra <- links_ngut_ra %>%
  mutate(slope_mean_links  = ra_mean_links - lag(ra_mean_links)) %>%
  group_by(foodweb) %>%
  mutate(ra_slope_mean_links = rollmean(slope_mean_links, 10, fill = NA, align='right'))

plot_slope_links_ngut_ra <- ggplot(slope_links_ngut_ra) +
  geom_line(aes(x = ngut, y = ra_slope_mean_links, color = foodweb)) +
  xlab("Number of predator guts") +
  ylab("Slope of link accumulation curve") +
  labs(color = "Food web") +
  theme_bw() +
  scale_x_continuous(breaks = c(1, 250, 500, 750, 1000, 1250))

# ggsave(filename = "results/misc/plot_slope_emplinks_ngut_ra.png", plot = plot_slope_links_ngut_ra)


#### Smoothening the Broadstone Stream curve separately ####

slope_links_ngut_ra_bs <- links_ngut_ra %>%
  filter(foodweb == "Broadstone Stream") %>%
  mutate(slope_mean_links  = ra_mean_links - lag(ra_mean_links)) %>%
  group_by(foodweb) %>%
  mutate(ra_slope_mean_links = rollmean(slope_mean_links, 100, fill = NA, align='right'))

plot_slope_links_ngut_ra_bs <- ggplot(slope_links_ngut_ra_bs) +
  geom_line(aes(x = ngut, y = ra_slope_mean_links, color = foodweb)) +
  xlab("Number of predator guts") +
  ylab("Slope of link accumulation curve") +
  labs(color = "Food web") +
  scale_x_continuous(breaks = c(1, 250, 500, 750, 1000, 1250))

# ggsave(filename = "results/misc/plot_slope_emplinks_ngut_ra_bs.png", plot = plot_slope_links_ngut_ra_bs)


#### Computing the factor (by which a food web has been undersampled as compared to the Broadstone Stream food web) ####

min_slope <- min(slope_links_ngut_ra_bs$ra_slope_mean_links, na.rm = TRUE)
max_ngut <- max(slope_links_ngut_ra_bs$ngut)

ggplot(slope_links_ngut_ra_bs) +
  geom_line(aes(x = ngut, y = ra_slope_mean_links, color = foodweb)) +
  geom_hline(yintercept = min_slope)

min_slope_all <- slope_links_ngut_ra %>%
  group_by(foodweb) %>%
  summarise(min_slope = min(ra_slope_mean_links, na.rm = TRUE))


#### Computing the min ngut for all the other food webs ####

factor_all <- data.frame(foodweb = character(), factor = double())

for(foodweb in foodweb_list){
  min_slope_foodweb <- min_slope_all$min_slope[min_slope_all$foodweb == foodweb]
  
  slope_links_ngut_ra_foodweb <- slope_links_ngut_ra %>%
    filter(foodweb == foodweb)
  
  ngut <- dim(slope_links_ngut_ra_bs)[1]
  max_ngut_foodweb <- max(slope_links_ngut_ra_foodweb$ngut)
  
  for(i in 1:ngut){
    row_single <- slope_links_ngut_ra_bs[i,]
    if(is.na(row_single$slope_mean_links) != TRUE){
      if(row_single$slope_mean_links < min_slope_foodweb){
        ngut_foodweb <- row_single$ngut
        break
      }
    }
    print(i)
  }
  factor <- max_ngut/ngut_foodweb
  factor_all <- rbind(factor_all, 
                      data.frame(foodweb = foodweb, factor = factor))
}


factor_all$factor[factor_all$foodweb == "Broadstone Stream"] <- 1

# saveRDS(object = factor_all, file = "results/misc/factor_all_ngut.RDS")

#### Taking into account the factor ####

df_min_gut_vs_S <- readRDS("results/misc/df_min_gut_vs_S.RDS")

df_min_gut_vs_S_factor <- merge(x = df_min_gut_vs_S, y = factor_all, by = "foodweb")

df_min_gut_vs_S_factor <- df_min_gut_vs_S_factor %>%
  mutate(corrected_min_gut = n_min_gut*factor)

#### Corrected min number of gut vs S ####

plot_corrected_mingut_vs_S <- ggplot(df_min_gut_vs_S_factor) +
  geom_point(aes(x = S, y = corrected_min_gut, color = foodweb), size = 5) +
  xlab("Number of species (S)") +
  ylab("Corrected minimum number of gut") +
  labs(color = "Food web") +
  # geom_smooth(aes(x = S, y = corrected_min_gut), method = "lm") +
  theme_bw()


# ggsave(plot = plot_corrected_mingut_vs_S, filename = "results/misc/plot_corrected_mingut_vs_S_ngut.png")




#### Corrected min number of gut vs L ####

plot_corrected_mingut_vs_L <- ggplot(df_min_gut_vs_S_factor) +
  geom_point(aes(x = L, y = corrected_min_gut, color = foodweb), size = 5) +
  xlab("Number of trophic links (L)") +
  ylab("Corrected minimum number of gut") +
  labs(color = "Food web") +
  # geom_smooth(aes(x = S, y = corrected_min_gut), method = "lm") +
  theme_bw()

# ggsave(plot = plot_corrected_mingut_vs_L, filename = "results/misc/plot_corrected_mingut_vs_L_ngut.png")


#### Corrected min number of gut vs connectance ####

plot_corrected_mingut_vs_C <- ggplot(df_min_gut_vs_S_factor) +
  geom_point(aes(x = C, y = corrected_min_gut, color = foodweb), size = 5) +
  xlab("Connectance (C)") +
  ylab("Corrected minimum number of gut") +
  labs(color = "Food web") +
  # geom_smooth(aes(x = S, y = corrected_min_gut), method = "lm") +
  theme_bw()

# ggsave(plot = plot_corrected_mingut_vs_C, filename = "results/misc/plot_corrected_mingut_vs_C_ngut.png")
