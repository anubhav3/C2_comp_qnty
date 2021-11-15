# 04.05.2021
# Plot model TSS vs empirical TSS


########### Broadstone Stream size_agg_v2

fw_name <- "Broadstone Stream size_agg_v2"

model_TSS <- readRDS("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Broadstone Stream size_agg_v2/rule_ind_predator/dd_bs.Rdata")
data_TSS <- readRDS("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Broadstone Stream size_agg_v2/rule_ind_predator/empirical_TSS_ngut.Rdata")


model_data_TSS_bs <- data.frame(l_model_TSS = model_TSS$l_acc, u_model_TSS = model_TSS$r_acc, mean_model_TSS = model_TSS$mean_acc,
                             l_data_TSS = data_TSS$l_TSS, u_data_TSS = data_TSS$u_TSS, mean_data_TSS = data_TSS$mean_TSS,
                             ngut = model_TSS$propn)

plot_TSS_model_data_Broadstone_Stream_size_agg_v2 <- 
  ggplot(model_data_TSS_bs) +
  geom_point(aes(x = mean_data_TSS, y = mean_model_TSS, color = ngut), size = 5) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_linerange(aes(x = mean_data_TSS, y = mean_model_TSS, ymin = l_model_TSS, ymax = u_model_TSS), alpha = 0.3) +
  geom_linerange(aes(x = mean_data_TSS, y = mean_model_TSS, xmin = l_data_TSS, xmax = u_data_TSS), alpha = 0.3) +
  theme_classic() +
  scale_color_gradient(low = "blue", high = "red", name = "Ngut") +
  xlab("TSS (Gut content data)") +
  ylab("TSS (ADBM and Gut content data)") +
  labs(title = "Broadstone Stream", tag = "(a)") +
  theme(plot.title = element_text(hjust=0.5), plot.tag = element_text(face = "bold")) +
  xlim(c(-0.1, 1)) +
  ylim(c(-0.1, 1))

# ggsave(filename = "results/misc/TSS_model_vs_data_Broadstone_Stream_size_agg_v2.png", plot = plot_TSS_model_empirical)


########### Celtic Sea size_agg

fw_name <- "Celtic Sea size_agg"

model_TSS <- readRDS("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Celtic Sea size_agg/rule_ind_predator/dd_cs.Rdata")
data_TSS <- readRDS("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Celtic Sea size_agg/rule_ind_predator/empirical_TSS_ngut.Rdata")


model_data_TSS_cs <- data.frame(l_model_TSS = model_TSS$l_acc, u_model_TSS = model_TSS$r_acc, mean_model_TSS = model_TSS$mean_acc,
                             l_data_TSS = data_TSS$l_TSS, u_data_TSS = data_TSS$u_TSS, mean_data_TSS = data_TSS$mean_TSS,
                             ngut = model_TSS$propn)

plot_TSS_model_data_Celtic_Sea_size_agg <- 
  ggplot(model_data_TSS_cs) +
  geom_point(aes(x = mean_data_TSS, y = mean_model_TSS, color = ngut), size = 5) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_linerange(aes(x = mean_data_TSS, y = mean_model_TSS, ymin = l_model_TSS, ymax = u_model_TSS), alpha = 0.5) +
  geom_linerange(aes(x = mean_data_TSS, y = mean_model_TSS, xmin = l_data_TSS, xmax = u_data_TSS), alpha = 0.5) +
  theme_classic() +
  scale_color_gradient(low = "blue", high = "red", name = "Ngut") +
  xlab("TSS (Gut content data)") +
  ylab("TSS (ADBM and Gut content data)") +
  labs(title = "Celtic Sea", tag = "(b)") +
  theme(plot.title = element_text(hjust=0.5), plot.tag = element_text(face = "bold")) +
  xlim(c(-0.15, 1)) +
  ylim(c(-0.15, 1))



########### Tadnoll Brook size_agg

fw_name <- "Tadnoll Brook size_agg"

model_TSS <- readRDS("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Tadnoll Brook size_agg/rule_ind_predator/dd_tb.Rdata")
data_TSS <- readRDS("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Tadnoll Brook size_agg/rule_ind_predator/empirical_TSS_ngut.Rdata")

data_TSS <- data_TSS[-51,]

model_data_TSS_tb <- data.frame(l_model_TSS = model_TSS$l_acc, u_model_TSS = model_TSS$r_acc, mean_model_TSS = model_TSS$mean_acc,
                                l_data_TSS = data_TSS$l_TSS, u_data_TSS = data_TSS$u_TSS, mean_data_TSS = data_TSS$mean_TSS,
                                ngut = model_TSS$propn)

plot_TSS_model_data_Tadnoll_Brook_size_agg <- 
  ggplot(model_data_TSS_tb) +
  geom_point(aes(x = mean_data_TSS, y = mean_model_TSS, color = ngut), size = 5) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_linerange(aes(x = mean_data_TSS, y = mean_model_TSS, ymin = l_model_TSS, ymax = u_model_TSS), alpha = 0.5) +
  geom_linerange(aes(x = mean_data_TSS, y = mean_model_TSS, xmin = l_data_TSS, xmax = u_data_TSS), alpha = 0.5) +
  theme_classic() +
  scale_color_gradient(low = "blue", high = "red", name = "Ngut") +
  xlab("TSS (Gut content data)") +
  ylab("TSS (ADBM and Gut content data)") +
  labs(title = "Tadnoll Brook", tag = "(c)") +
  theme(plot.title = element_text(hjust=0.5), plot.tag = element_text(face = "bold")) +
  xlim(c(-0.05, 1)) +
  ylim(c(-0.05, 1))





########### Afon Hirnant size_agg

fw_name <- "Afon Hirnant size_agg"

model_TSS <- readRDS("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Afon Hirnant size_agg/rule_ind_predator/dd_ah.Rdata")
data_TSS <- readRDS("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Afon Hirnant size_agg/rule_ind_predator/empirical_TSS_ngut.Rdata")

data_TSS <- data_TSS[-45,]

model_data_TSS_ah <- data.frame(l_model_TSS = model_TSS$l_acc, u_model_TSS = model_TSS$r_acc, mean_model_TSS = model_TSS$mean_acc,
                                l_data_TSS = data_TSS$l_TSS, u_data_TSS = data_TSS$u_TSS, mean_data_TSS = data_TSS$mean_TSS,
                                ngut = model_TSS$propn)

plot_TSS_model_data_Afon_Hirnant_size_agg <- 
  ggplot(model_data_TSS_ah) +
  geom_point(aes(x = mean_data_TSS, y = mean_model_TSS, color = ngut), size = 5) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_linerange(aes(x = mean_data_TSS, y = mean_model_TSS, ymin = l_model_TSS, ymax = u_model_TSS), alpha = 0.5) +
  geom_linerange(aes(x = mean_data_TSS, y = mean_model_TSS, xmin = l_data_TSS, xmax = u_data_TSS), alpha = 0.5) +
  theme_classic() +
  scale_color_gradient(low = "blue", high = "red", name = "Ngut") +
  xlab("TSS (Gut content data)") +
  ylab("TSS (ADBM and Gut content data)") +
  labs(title = "Afon Hirnant", tag = "(d)") +
  theme(plot.title = element_text(hjust=0.5), plot.tag = element_text(face = "bold")) +
  xlim(c(-0.05, 1)) +
  ylim(c(-0.05, 1))



plot_TSS_model_empirical <- ggarrange(plot_TSS_model_empirical_broadstone_stream_size_agg_v2, plot_TSS_model_empirical_celtic_sea,
                                      nrow = 1, ncol = 2)
# ggsave(filename = "results/misc/TSS_model_vs_data.png", plot = plot_TSS_model_empirical,
#       width = 11, height = 4)





model_data_TSS_bs_ratio <-
  model_data_TSS_bs %>%
  mutate(ratio = mean_model_TSS/mean_data_TSS)

model_data_TSS_cs_ratio <-
  model_data_TSS_cs %>%
  mutate(ratio = mean_model_TSS/mean_data_TSS)

model_data_TSS_tb_ratio <-
  model_data_TSS_tb %>%
  mutate(ratio = mean_model_TSS/mean_data_TSS)

model_data_TSS_ah_ratio <-
  model_data_TSS_ah %>%
  mutate(ratio = mean_model_TSS/mean_data_TSS)

model_data_TSS_tb_ratio <- model_data_TSS_tb_ratio[-1,]

plot_model_data_TSS_bs_ratio <- ggplot(model_data_TSS_bs_ratio) +
  geom_point(aes(x = ngut, ratio), shape = 1) +
  geom_abline(slope = 0, intercept = log10(1), linetype = 2) +
  scale_y_log10() +
  theme_bw() +
  xlab("Number of predator guts") +
  ylab(TeX(r'($\frac{TSS(Model)}{TSS(Data)}$)')) +
  labs(tag = "(b)") +
  theme(plot.tag = element_text(face = "bold")) +
  scale_x_continuous(breaks = as.integer(seq(1, 1008, length = 5)))

plot_model_data_TSS_cs_ratio <- ggplot(model_data_TSS_cs_ratio) +
  geom_point(aes(x = ngut, ratio), shape = 1) +
  geom_abline(slope = 0, intercept = log10(1), linetype = 2) +
  scale_y_log10() +
  theme_bw() +
  xlab("Number of predator guts") +
  ylab(TeX(r'($\frac{TSS(Model)}{TSS(Data)}$)')) +
  labs(tag = "(d)") +
  theme(plot.tag = element_text(face = "bold")) +
  scale_x_continuous(breaks = as.integer(seq(1, 491, length = 5)))

plot_model_data_TSS_tb_ratio <- ggplot(model_data_TSS_tb_ratio) +
  geom_point(aes(x = ngut, ratio), shape = 1) +
  geom_abline(slope = 0, intercept = log10(1), linetype = 2) +
  scale_y_log10() +
  theme_bw() +
  xlab("Number of predator guts") +
  ylab(TeX(r'($\frac{TSS(Model)}{TSS(Data)}$)')) +
  labs(tag = "(f)") +
  theme(plot.tag = element_text(face = "bold")) +
  scale_x_continuous(breaks = as.integer(seq(1, 688, length = 5)))

plot_model_data_TSS_ah_ratio <- ggplot(model_data_TSS_ah_ratio) +
  geom_point(aes(x = ngut, ratio), shape = 1) +
  geom_abline(slope = 0, intercept = log10(1), linetype = 2) +
  scale_y_log10() +
  theme_bw() +
  xlab("Number of predator guts") +
  ylab(TeX(r'($\frac{TSS(Model)}{TSS(Data)}$)')) +
  labs(tag = "(h)") +
  theme(plot.tag = element_text(face = "bold")) +
  scale_x_continuous(breaks = as.integer(seq(1, 174, length = 5)))



# p1 <- ggarrange(dd_plot_broadstone_stream, plot_model_data_TSS_bs_ratio,
#           dd_plot_celtic_sea, plot_model_data_TSS_cs_ratio,
#           dd_plot_Tadnoll_Brook, plot_model_data_TSS_tb_ratio,
#           dd_plot_Afon_Hirnant, plot_model_data_TSS_ah_ratio,
#           nrow = 4, ncol = 2)


p1 <- ggarrange(dd_plot_broadstone_stream, plot_model_data_TSS_bs_ratio)
p1 <- annotate_figure(p1, top = text_grob("Broadstone Stream", size = 20))

p2 <- ggarrange(dd_plot_celtic_sea, plot_model_data_TSS_cs_ratio)
p2 <- annotate_figure(p2, top = text_grob("Celtic Sea", size = 20))

p3 <- ggarrange(dd_plot_Tadnoll_Brook, plot_model_data_TSS_tb_ratio)
p3 <- annotate_figure(p3, top = text_grob("Tadnoll Brook", size = 20))

p4 <- ggarrange(dd_plot_Afon_Hirnant, plot_model_data_TSS_ah_ratio)
p4 <- annotate_figure(p4, top = text_grob("Afon Hirnant", size = 20))


p1234 <- ggarrange(p1, p2, p3, p4, nrow = 4)

# ggsave(plot = p1234, filename = "results/misc/plot_TSS_ratio.png", width = 6, height = 9)

model_data_TSS_tb_ratio_inv <- 
  model_data_TSS_tb_ratio %>%
  mutate(ngut_inv = 1/ngut, log_ratio = log10(ratio))

lin_mod <- lm(ratio ~ ngut_inv, data = model_data_TSS_tb_ratio_inv)
summary(lin_mod)

autoplot(lin_mod)


ggplot(model_data_TSS_tb_ratio_inv) +
  geom_point(aes(x = ngut_inv, log_ratio))
 
  