# 2022.06.14
# Plot how the minimum number of predator guts varied with food web summary: C, S and L

library(ggplot2)
library(ggpubr)
library(dplyr)

df_min_gut_vs_S <- readRDS("results/misc/df_min_gut_vs_S.RDS")


#### Min gut vs C ####

plot_n_min_gut_vs_C <- ggplot(df_min_gut_vs_S) +
  geom_point(aes(x = C, y = n_min_gut), size = 5) +
  geom_smooth(aes(x = C, y = n_min_gut), method = "lm", color = "black") +
  theme_classic() +
  scale_color_discrete(name = "Food web") +
  xlab("Connectance (C)") +
  labs(tag = "(a)") +
  coord_cartesian(ylim = c(0, 450)) +
  ylab("Minimum number of predator guts") +
  theme(title = element_text(size = 15), text = element_text(size = 15))

lm_n_min_gut_vs_C <- lm(n_min_gut ~ C, data = df_min_gut_vs_S)
summary(lm_n_min_gut_vs_C)

#### Min gut vs L ####

plot_n_min_gut_vs_L <- ggplot(df_min_gut_vs_S) +
  geom_point(aes(x = L, y = n_min_gut), size = 5) +
  geom_smooth(aes(x = L, y = n_min_gut), method = "lm", color = "black") +
  theme_classic() +
  scale_color_discrete(name = "Food web") +
  xlab("Number of trophic links (L)") +
  labs(tag = "(a)") +
  coord_cartesian(ylim = c(0, 450)) +
  ylab("Minimum number of predator guts") +
  theme(title = element_text(size = 15), text = element_text(size = 15))

lm_n_min_gut_vs_L <- lm(n_min_gut ~ L, data = df_min_gut_vs_S)
summary(lm_n_min_gut_vs_L)

#### Min gut vs S ####

plot_n_min_gut_vs_S <- ggplot(df_min_gut_vs_S) +
  geom_point(aes(x = S, y = n_min_gut), size = 5) +
  # geom_smooth(aes(x = S, y = n_min_gut), method = "lm", color = "black") +
  theme_classic() +
  scale_color_discrete(name = "Food web") +
  xlab("Number of species (S)") +
  labs(tag = "(b)") +
  coord_cartesian(ylim = c(0, 450)) +
  ylab("Minimum number of predator guts") +
  theme(title = element_text(size = 15), text = element_text(size = 15),
        axis.title.y = element_blank())


#### Combining the above plots ####

plot_n_min_gut_vs_CSL <- ggarrange(plot_n_min_gut_vs_C, plot_n_min_gut_vs_L, plot_n_min_gut_vs_S,
                                   nrow = 1)

plot_n_min_gut_vs_SL <- ggarrange(plot_n_min_gut_vs_L, plot_n_min_gut_vs_S,
                                   nrow = 1)

##### Corrected min gut vs CSL ####

factor_df <- readRDS(file = "results/misc/factor_all_vegan.RDS")

df_min_gut_vs_S_factor <- merge(x = df_min_gut_vs_S, y = factor_df, by = "foodweb")

df_min_gut_vs_S_factor <- df_min_gut_vs_S_factor %>%
  mutate(corrected_min_gut = n_min_gut*factor, corrected_C = corrected_links/(S^2))

#### corrected min gut vs corrected C ####

scaleFUN <- function(x) sprintf("%.2f", x)

plot_corrected_n_min_gut_vs_corrected_C <- ggplot(df_min_gut_vs_S_factor) +
  geom_point(aes(x = corrected_C, y = corrected_min_gut), size = 5) +
  # geom_smooth(aes(x = C, y = corrected_min_gut), method = "lm", color = "black") +
  theme_classic() +
  scale_color_discrete(name = "Food web") +
  xlab("Corrected connectance (C)") +
  labs(tag = "(d)") +
  # coord_cartesian(ylim = c(0, 450)) +
  ylab("Corrected minimum number of predator guts") +
  theme(title = element_text(size = 15), text = element_text(size = 15))  +
  scale_x_continuous(labels=scaleFUN)


#### Corrected min gut vs corrected L ####

plot_corrected_n_min_gut_vs_corrected_L <- ggplot(df_min_gut_vs_S_factor) +
  geom_point(aes(x = corrected_links, y = corrected_min_gut), size = 5) +
  geom_smooth(aes(x = corrected_links, y = corrected_min_gut), method = "lm", color = "black") +
  theme_classic() +
  scale_color_discrete(name = "Food web") +
  xlab("Corrected number of trophic links (L)") +
  labs(tag = "(c)") +
  # coord_cartesian(ylim = c(0, 450)) +
  ylab("Corrected minimum number of predator guts") +
  theme(title = element_text(size = 15), text = element_text(size = 15))

lm_corrected_n_min_gut_vs_corrected_L <- lm(corrected_min_gut ~ corrected_links, data = df_min_gut_vs_S_factor)
summary(lm_corrected_n_min_gut_vs_corrected_L)

#### Corrected Min gut vs S ####

plot_corrected_n_min_gut_vs_S <- ggplot(df_min_gut_vs_S_factor) +
  geom_point(aes(x = S, y = corrected_min_gut), size = 5) +
  geom_smooth(aes(x = S, y = corrected_min_gut), method = "lm", color = "black") +
  theme_classic() +
  scale_color_discrete(name = "Food web") +
  xlab("Number of species (S)") +
  labs(tag = "(d)") +
  # coord_cartesian(ylim = c(0, 450)) +
  ylab("Minimum number of predator guts") +
  theme(title = element_text(size = 15), text = element_text(size = 15),
        axis.title.y = element_blank())

lm_corrected_n_min_gut_vs_S <- lm(corrected_min_gut ~ S, data = df_min_gut_vs_S_factor)
summary(lm_corrected_n_min_gut_vs_S)

#### Combining the above plots ####

plot_corrected_n_min_gut_vs_CSL <- ggarrange(plot_corrected_n_min_gut_vs_corrected_C, plot_corrected_n_min_gut_vs_corrected_L, 
                                   plot_corrected_n_min_gut_vs_S,
                                   nrow = 1)
plot_corrected_n_min_gut_vs_SL <- ggarrange(plot_corrected_n_min_gut_vs_corrected_L, 
                                             plot_corrected_n_min_gut_vs_S,
                                             nrow = 1)
plot_all_CSL <- ggarrange(plot_n_min_gut_vs_CSL, plot_corrected_n_min_gut_vs_CSL,
                      nrow = 2)

plot_all <- ggarrange(plot_n_min_gut_vs_SL, plot_corrected_n_min_gut_vs_SL,
                      nrow = 2)

# ggsave(plot = plot_all, filename = "results/misc/plot_n_min_gut_vs_SL.png", width = 10, height = 9)
