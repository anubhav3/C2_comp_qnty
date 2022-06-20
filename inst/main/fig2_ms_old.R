# 09.11.2021

library(ggplot2)
library(ggpubr)
library(dplyr)

df_min_gut_vs_S <- readRDS("results/misc/df_min_gut_vs_S.RDS")

plot_n_min_gut_vs_S <- ggplot(df_min_gut_vs_S) +
  geom_point(aes(x = S, y = n_min_gut, color = foodweb), size = 5) +
  theme_classic() +
  scale_color_discrete(name = "Food web") +
  xlab("Number of species (S)") +
  labs(tag = "(a)") +
  ylab("Number of minimum gut")

plot_n_min_gut_vs_n_max_gut <- ggplot(df_min_gut_vs_S) +
  geom_point(aes(x = max_ngut, y = n_min_gut, color = foodweb), size = 5) +
  theme_classic() +
  scale_color_discrete(name = "Food web") +
  xlab("Number of maximum guts sampled") +
  labs(tag = "(b)") +
  ylab("Number of minimum gut")

plot_n_min_gut_vs_L <- 
  df_min_gut_vs_S %>%
  ggplot() +
  geom_point(aes(x = L, y = n_min_gut, color = foodweb), size = 5) +
  theme_classic() +
  scale_color_discrete(name = "Food web") +
  xlab("Number of links (L)") +
  labs(tag = "(c)") +
  ylab("Number of minimum gut")

plot_n_min_gut_vs_C <- ggplot(df_min_gut_vs_S) +
  geom_point(aes(x = C, y = n_min_gut, color = foodweb), size = 5) +
  theme_classic() +
  scale_color_discrete(name = "Food web") +
  xlab("Connectance (C)") +
  labs(tag = "(d)") +
  ylab("Number of minimum gut") 

plot_n_min_gut_vs_n_pred <- 
  df_min_gut_vs_S %>%
  ggplot() +
  geom_point(aes(x = n_pred, y = n_min_gut, color = foodweb), size = 5) +
  theme_classic() +
  scale_color_discrete(name = "Food web") +
  xlab("Number of predator nodes") +
  labs(tag = "(e)") +
  ylab("Number of minimum gut")

plot_prop_min_gut_vs_S <- ggplot(df_min_gut_vs_S) +
  geom_point(aes(x = S, y = prop_min_gut, color = foodweb), size = 5) +
  theme_classic() +
  scale_color_discrete(name = "Food web") +
  xlab("Number of species (S)") +
  labs(tag = "(f)") +
  ylab("Proportion of minimum gut")

plot_prop_min_gut_vs_L <- ggplot(df_min_gut_vs_S) +
  geom_point(aes(x = L, y = prop_min_gut, color = foodweb), size = 5) +
  theme_classic() +
  scale_color_discrete(name = "Food web") +
  xlab("Number of links (L)") +
  labs(tag = "(g)") +
  ylab("Proportion of minimum gut")

plot_prop_min_gut_vs_C <- ggplot(df_min_gut_vs_S) +
  geom_point(aes(x = C, y = prop_min_gut, color = foodweb), size = 5) +
  theme_classic() +
  scale_color_discrete(name = "Food web") +
  xlab("Connectance (C)") +
  labs(tag = "(h)") +
  ylab("Proportion of minimum gut")


plot_min_gut_vs_X <- ggarrange(plot_n_min_gut_vs_S, plot_n_min_gut_vs_n_max_gut, plot_n_min_gut_vs_L,
                               plot_n_min_gut_vs_C, plot_n_min_gut_vs_n_pred, plot_prop_min_gut_vs_S,
                               plot_prop_min_gut_vs_L, plot_prop_min_gut_vs_C, legend = "bottom",
                               common.legend = TRUE)

# ggsave(plot = plot_min_gut_vs_X, filename = "results/misc/plot_min_gut_vs_X.png", width = 13.5, height = 9)
