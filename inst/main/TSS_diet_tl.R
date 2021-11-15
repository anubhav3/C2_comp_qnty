

library(ggplot2)
library(dplyr)
dd <- readRDS("results/rejection/sim_Broadstone Stream size_agg_v2/rule_s_r_2_predator/N=1e+05_tol=Inf_n_pred = 29/rn_gut=1_N=1e+05_tol=Inf_TSS_sir_gut_s_r_2_l29_prop_1/sim_Broadstone Stream size_agg_v2.Rdata")


df <- data.frame(dist_diet = dd$dist_diet, dist_tl = dd$dist_tl, TSS = dd$TSS_fw)

df <- mutate(df, TSS_round = round(TSS, digits = 1), dist_diet_round = round(dist_diet, digits = 1))

# df <- df[sample(x = 1:100000, size = 10000, replace = FALSE ),]

df %>%
  filter(dist_tl < 1 & TSS_round > 0) %>%
  # filter(dist_diet == 0) %>%
ggplot() +
  geom_point(aes(x = dist_diet, y = dist_tl, color = as.factor(TSS_round)), position = "jitter", size = 4, alpha = 0.5) +
  theme_classic() +
  scale_color_brewer(name = "TSS", palette = "Paired")


df %>%
  filter(dist_tl < 3 & TSS_round > 0) %>%
  ggplot(aes(x = dist_diet, y = dist_tl, fill = TSS)) +
  geom_raster()

df %>%
filter(dist_tl < 3 & TSS_round > 0) %>%
  ggplot() +
    geom_point(aes(x = dist_tl, y = TSS, color = as.factor(dist_diet_round))) +
  theme_bw()


df %>%
  filter(dist_tl < 3 & TSS_round > 0) %>%
  ggplot() +
  geom_point(aes(x = dist_diet, y = TSS)) +
  theme_bw()


hist(log10(dd$dist_tl))

which(dd$dist_tl >= 6.06042e+13)

df_fil <- df %>%
  filter(dist_tl <= 10)

TSS_lm <- lm(TSS ~ dist_tl*dist_diet , data = df_fil)
summary(TSS_lm)
anova(TSS_lm)



df %>%
  filter(dist_tl < 5) %>%
  ggplot() +
  geom_point(aes(x = dist_tl, y = TSS), alpha = 0.2) +
  theme_bw()
