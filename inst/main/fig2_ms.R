# 17.03.2022

library(ggplot2)
library(ggpubr)
library(dplyr)

df_min_gut_vs_S <- readRDS("results/misc/df_min_gut_vs_S.RDS")


## min gut vs C ####

plot_n_min_gut_vs_C <- ggplot(df_min_gut_vs_S) +
  geom_point(aes(x = C, y = n_min_gut, color = foodweb), size = 5) +
  geom_smooth(aes(x = C, y = n_min_gut), method = "lm", color = "black") +
  theme_classic() +
  scale_color_discrete(name = "Food web") +
  xlab("Connectance (C)") +
  labs(tag = "(a)") +
  coord_cartesian(ylim = c(0, 450)) +
  ylab("Number of minimum gut") 

lm_n_vs_C <- df_min_gut_vs_S %>%
  # filter(foodweb != "Broadstone Stream") %>%
  lm(formula = n_min_gut ~ C)

summary(lm_n_vs_C)

## min gut vs L ####

plot_n_min_gut_vs_L <- 
  df_min_gut_vs_S %>%
  ggplot() +
  geom_point(aes(x = L, y = n_min_gut, color = foodweb), size = 5) +
  geom_smooth(aes(x = L, y = n_min_gut), method = "lm", color = "black") +
  theme_classic() +
  scale_color_discrete(name = "Food web") +
  xlab("Number of links (L)") +
  labs(tag = "(b)") +
  coord_cartesian(ylim = c(0, 450)) +
  ylab("Number of minimum gut")


lm_n_vs_L <- df_min_gut_vs_S %>%
  # filter(foodweb != "Broadstone Stream") %>%
  lm(formula = n_min_gut ~ L)

summary(lm_n_vs_L)
anova(lm_n_vs_L)

## min gut vs max gut ####

plot_n_min_gut_vs_n_max_gut <- ggplot(df_min_gut_vs_S) +
  geom_point(aes(x = max_ngut, y = n_min_gut, color = foodweb), size = 5) +
  geom_smooth(aes(x = max_ngut, y = n_min_gut), method = "lm", color = "black") +
  theme_classic() +
  scale_color_discrete(name = "Food web") +
  xlab("Number of maximum guts sampled") +
  labs(tag = "(c)") +
  coord_cartesian(ylim = c(0, 450)) +
  ylab("Number of minimum gut") 

lm_n_vs_N <- df_min_gut_vs_S %>%
  lm(formula = n_min_gut ~ max_ngut)

summary(lm_n_vs_N)
anova(lm_n_vs_N)


plot_min_gut_vs_X <- ggarrange(plot_n_min_gut_vs_C,
                               plot_n_min_gut_vs_L, plot_n_min_gut_vs_n_max_gut, legend = "right",
                               common.legend = TRUE)

# ggsave(plot = plot_min_gut_vs_X, filename = "results/misc/plot_min_gut_vs_X.png", width = 10, height = 6)
