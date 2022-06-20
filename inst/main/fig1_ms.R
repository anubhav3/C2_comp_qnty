# 14.10.2021
# We construct the Fig. 1 of the ms
library(latex2exp)

df_min_gut_vs_S <- readRDS("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Misc/df_min_gut_vs_S.RDS")


####################################################################################################
fw_name <- "Broadstone Stream size_agg_v2"

dd_bs <- readRDS(paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_ind_predator/dd_bs.Rdata"))

min_gut_bs <- df_min_gut_vs_S$n_min_gut[which(df_min_gut_vs_S$foodweb == "Broadstone Stream")]

tsize <- 30
dd_plot_broadstone_stream <- ggplot(dd_bs) +
  geom_vline(xintercept = min_gut_bs, color = "red") +
  geom_line(aes(x = propn, y = mean_acc), 
            position=position_dodge(width = 3)) +
  geom_ribbon(aes(x = propn, ymin = l_acc, ymax = r_acc), alpha = 0.5, show.legend = FALSE,
              position=position_dodge(width = 3)) +
  xlab("Number of predator guts") +
  ylab("True skill statistic") +
  theme_classic() +
  scale_color_manual(name = "Gut content data", values = c("red", "blue")) +
  scale_x_continuous(breaks = as.integer(seq(1, 1008, length = 5))) +
  labs(tag = "(a)") +
  theme(plot.tag = element_text(face = "bold")) 

model_TSS <- readRDS(paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_ind_predator/dd_bs.Rdata"))
data_TSS <- readRDS(paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_ind_predator/empirical_TSS_ngut.Rdata"))


model_data_TSS_bs <- data.frame(l_model_TSS = model_TSS$l_acc, u_model_TSS = model_TSS$r_acc, mean_model_TSS = model_TSS$mean_acc,
                                l_data_TSS = data_TSS$l_TSS, u_data_TSS = data_TSS$u_TSS, mean_data_TSS = data_TSS$mean_TSS,
                                ngut = model_TSS$propn)

model_data_TSS_bs_ratio <-
  model_data_TSS_bs %>%
  mutate(ratio = mean_model_TSS/mean_data_TSS)

plot_model_data_TSS_bs_ratio <- ggplot(model_data_TSS_bs_ratio) +
  geom_point(aes(x = ngut, ratio), shape = 1) +
  geom_abline(slope = 0, intercept = log10(1), linetype = 2) +
  scale_y_log10() +
  theme_classic() +
  xlab("Number of predator guts") +
  ylab(TeX(r'($\frac{TSS(Model)}{TSS(Data)}$)')) +
  labs(tag = "(b)") +
  theme(plot.tag = element_text(face = "bold")) +
  scale_x_continuous(breaks = as.integer(seq(1, 1008, length = 5)))


p1 <- ggarrange(dd_plot_broadstone_stream, plot_model_data_TSS_bs_ratio)
p1 <- annotate_figure(p1, top = text_grob("Broadstone Stream", size = 20))


####################################################################################################
fw_name <- "Celtic Sea size_agg"

dd_cs <- readRDS(paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_ind_predator/dd_cs.Rdata"))

min_gut_cs <- df_min_gut_vs_S$n_min_gut[which(df_min_gut_vs_S$foodweb == "Celtic Sea")]

tsize <- 30
dd_plot_celtic_sea <- ggplot(dd_cs) +
  geom_vline(xintercept = min_gut_cs, color = "red") +
  geom_line(aes(x = propn, y = mean_acc), 
            position=position_dodge(width = 3)) +
  geom_ribbon(aes(x = propn, ymin = l_acc, ymax = r_acc), alpha = 0.5, show.legend = FALSE,
              position=position_dodge(width = 3)) +
  xlab("Number of predator guts") +
  ylab("True skill statistic") +
  theme_classic() +
  scale_color_manual(name = "Gut content data", values = c("red", "blue")) +
  scale_x_continuous(breaks = as.integer(seq(1, 491, length = 5))) +
  labs(tag = "(c)") +
  theme(plot.tag = element_text(face = "bold")) 

model_TSS <- readRDS(paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_ind_predator/dd_cs.Rdata"))
data_TSS <- readRDS(paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_ind_predator/empirical_TSS_ngut.Rdata"))


model_data_TSS_cs <- data.frame(l_model_TSS = model_TSS$l_acc, u_model_TSS = model_TSS$r_acc, mean_model_TSS = model_TSS$mean_acc,
                                l_data_TSS = data_TSS$l_TSS, u_data_TSS = data_TSS$u_TSS, mean_data_TSS = data_TSS$mean_TSS,
                                ngut = model_TSS$propn)

model_data_TSS_cs_ratio <-
  model_data_TSS_cs %>%
  mutate(ratio = mean_model_TSS/mean_data_TSS)

plot_model_data_TSS_cs_ratio <- ggplot(model_data_TSS_cs_ratio) +
  geom_point(aes(x = ngut, ratio), shape = 1) +
  geom_abline(slope = 0, intercept = log10(1), linetype = 2) +
  scale_y_log10() +
  theme_classic() +
  xlab("Number of predator guts") +
  ylab(TeX(r'($\frac{TSS(Model)}{TSS(Data)}$)')) +
  labs(tag = "(d)") +
  theme(plot.tag = element_text(face = "bold")) +
  scale_x_continuous(breaks = as.integer(seq(1, 491, length = 5)))


p2 <- ggarrange(dd_plot_celtic_sea, plot_model_data_TSS_cs_ratio)
p2 <- annotate_figure(p2, top = text_grob("Celtic Sea", size = 20))


####################################################################################################
fw_name <- "Tadnoll Brook size_agg"

dd_tb <- readRDS(paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_ind_predator/dd_tb.Rdata"))

min_gut_tb <- df_min_gut_vs_S$n_min_gut[which(df_min_gut_vs_S$foodweb == "Tadnoll Brook")]

tsize <- 30
dd_plot_tadnoll_brook <- ggplot(dd_tb) +
  geom_vline(xintercept = min_gut_tb, color = "red") +
  geom_line(aes(x = propn, y = mean_acc), 
            position=position_dodge(width = 3)) +
  geom_ribbon(aes(x = propn, ymin = l_acc, ymax = r_acc), alpha = 0.5, show.legend = FALSE,
              position=position_dodge(width = 3)) +
  xlab("Number of predator guts") +
  ylab("True skill statistic") +
  theme_classic() +
  scale_color_manual(name = "Gut content data", values = c("red", "blue")) +
  scale_x_continuous(breaks = as.integer(seq(1, 688, length = 5))) +
  labs(tag = "(e)") +
  theme(plot.tag = element_text(face = "bold")) 

model_TSS <- readRDS(paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_ind_predator/dd_tb.Rdata"))
data_TSS <- readRDS(paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_ind_predator/empirical_TSS_ngut.Rdata"))

data_TSS <- data_TSS[-51,]

model_data_TSS_tb <- data.frame(l_model_TSS = model_TSS$l_acc, u_model_TSS = model_TSS$r_acc, mean_model_TSS = model_TSS$mean_acc,
                                l_data_TSS = data_TSS$l_TSS, u_data_TSS = data_TSS$u_TSS, mean_data_TSS = data_TSS$mean_TSS,
                                ngut = model_TSS$propn)

model_data_TSS_tb_ratio <-
  model_data_TSS_tb %>%
  mutate(ratio = mean_model_TSS/mean_data_TSS)

plot_model_data_TSS_tb_ratio <- ggplot(model_data_TSS_tb_ratio) +
  geom_point(aes(x = ngut, ratio), shape = 1) +
  geom_abline(slope = 0, intercept = log10(1), linetype = 2) +
  scale_y_log10() +
  theme_classic() +
  xlab("Number of predator guts") +
  ylab(TeX(r'($\frac{TSS(Model)}{TSS(Data)}$)')) +
  labs(tag = "(f)") +
  scale_x_continuous(breaks = as.integer(seq(1, 688, length = 5))) +
  theme(plot.tag = element_text(face = "bold"))


p3 <- ggarrange(dd_plot_tadnoll_brook, plot_model_data_TSS_tb_ratio)
p3 <- annotate_figure(p3, top = text_grob("Tadnoll Brook", size = 20))



####################################################################################################
fw_name <- "Afon Hirnant size_agg"

dd_ah <- readRDS(paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_ind_predator/dd_ah.Rdata"))

min_gut_ah <- df_min_gut_vs_S$n_min_gut[which(df_min_gut_vs_S$foodweb == "Afon Hirnant")]

tsize <- 30
dd_plot_afon_hirnant <- ggplot(dd_ah) +
  geom_vline(xintercept = min_gut_ah, color = "red") +
  geom_line(aes(x = propn, y = mean_acc), 
            position=position_dodge(width = 3)) +
  geom_ribbon(aes(x = propn, ymin = l_acc, ymax = r_acc), alpha = 0.5, show.legend = FALSE,
              position=position_dodge(width = 3)) +
  xlab("Number of predator guts") +
  ylab("True skill statistic") +
  theme_classic() +
  scale_color_manual(name = "Gut content data", values = c("red", "blue")) +
  scale_x_continuous(breaks = as.integer(seq(1, 175, length = 5))) +
  labs(tag = "(g)") +
  theme(plot.tag = element_text(face = "bold")) 

model_TSS <- readRDS(paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_ind_predator/dd_ah.Rdata"))
data_TSS <- readRDS(paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_ind_predator/empirical_TSS_ngut.Rdata"))

data_TSS <- data_TSS[-45,]

model_data_TSS_ah <- data.frame(l_model_TSS = model_TSS$l_acc, u_model_TSS = model_TSS$r_acc, mean_model_TSS = model_TSS$mean_acc,
                                l_data_TSS = data_TSS$l_TSS, u_data_TSS = data_TSS$u_TSS, mean_data_TSS = data_TSS$mean_TSS,
                                ngut = model_TSS$propn)

model_data_TSS_ah_ratio <-
  model_data_TSS_ah %>%
  mutate(ratio = mean_model_TSS/mean_data_TSS)

plot_model_data_TSS_ah_ratio <- ggplot(model_data_TSS_ah_ratio) +
  geom_point(aes(x = ngut, ratio), shape = 1) +
  geom_abline(slope = 0, intercept = log10(1), linetype = 2) +
  scale_y_log10() +
  theme_classic() +
  xlab("Number of predator guts") +
  ylab(TeX(r'($\frac{TSS(Model)}{TSS(Data)}$)')) +
  labs(tag = "(h)") +
  scale_x_continuous(breaks = as.integer(seq(1, 175, length = 5))) +
  theme(plot.tag = element_text(face = "bold"))


p4 <- ggarrange(dd_plot_afon_hirnant, plot_model_data_TSS_ah_ratio)
p4 <- annotate_figure(p4, top = text_grob("Afon Hirnant", size = 20))


####################################################################################################
fw_name <- "Coilaco size_agg"

dd_co <- readRDS(paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_ind_predator/dd_co.Rdata"))

min_gut_co <- df_min_gut_vs_S$n_min_gut[which(df_min_gut_vs_S$foodweb == "Coilaco")]

tsize <- 30
dd_plot_coilaco <- ggplot(dd_co) +
  geom_vline(xintercept = min_gut_co, color = "red") +
  geom_line(aes(x = propn, y = mean_acc), 
            position=position_dodge(width = 3)) +
  geom_ribbon(aes(x = propn, ymin = l_acc, ymax = r_acc), alpha = 0.5, show.legend = FALSE,
              position=position_dodge(width = 3)) +
  xlab("Number of predator guts") +
  ylab("True skill statistic") +
  theme_classic() +
  scale_color_manual(name = "Gut content data", values = c("red", "blue")) +
  scale_x_continuous(breaks = as.integer(seq(1, 74, length = 5))) +
  labs(tag = "(i)") +
  theme(plot.tag = element_text(face = "bold")) 

model_TSS <- readRDS(paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_ind_predator/dd_co.Rdata"))
data_TSS <- readRDS(paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_ind_predator/empirical_TSS_ngut.Rdata"))

model_data_TSS_co <- data.frame(l_model_TSS = model_TSS$l_acc, u_model_TSS = model_TSS$r_acc, mean_model_TSS = model_TSS$mean_acc,
                                l_data_TSS = data_TSS$l_TSS, u_data_TSS = data_TSS$u_TSS, mean_data_TSS = data_TSS$mean_TSS,
                                ngut = model_TSS$propn)

model_data_TSS_co_ratio <-
  model_data_TSS_co %>%
  mutate(ratio = mean_model_TSS/mean_data_TSS)

plot_model_data_TSS_co_ratio <- ggplot(model_data_TSS_co_ratio) +
  geom_point(aes(x = ngut, ratio), shape = 1) +
  geom_abline(slope = 0, intercept = log10(1), linetype = 2) +
  scale_y_log10() +
  theme_classic() +
  xlab("Number of predator guts") +
  ylab(TeX(r'($\frac{TSS(Model)}{TSS(Data)}$)')) +
  labs(tag = "(j)") +
  scale_x_continuous(breaks = as.integer(seq(1, 74, length = 5))) +
  theme(plot.tag = element_text(face = "bold"))


p5 <- ggarrange(dd_plot_coilaco, plot_model_data_TSS_co_ratio)
p5 <- annotate_figure(p5, top = text_grob("Coilaco", size = 20))


####################################################################################################
fw_name <- "Guampoe size_agg"

dd_gu <- readRDS(paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_ind_predator/dd_gu.Rdata"))

min_gut_gu <- df_min_gut_vs_S$n_min_gut[which(df_min_gut_vs_S$foodweb == "Guampoe")]

tsize <- 30
dd_plot_guampoe <- ggplot(dd_gu) +
  geom_vline(xintercept = min_gut_gu, color = "red") +
  geom_line(aes(x = propn, y = mean_acc), 
            position=position_dodge(width = 3)) +
  geom_ribbon(aes(x = propn, ymin = l_acc, ymax = r_acc), alpha = 0.5, show.legend = FALSE,
              position=position_dodge(width = 3)) +
  xlab("Number of predator guts") +
  ylab("True skill statistic") +
  theme_classic() +
  scale_color_manual(name = "Gut content data", values = c("red", "blue")) +
  scale_x_continuous(breaks = as.integer(seq(1, 87, length = 5))) +
  labs(tag = "(k)") +
  theme(plot.tag = element_text(face = "bold")) 

model_TSS <- readRDS(paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_ind_predator/dd_gu.Rdata"))
data_TSS <- readRDS(paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_ind_predator/empirical_TSS_ngut.Rdata"))

model_data_TSS_gu <- data.frame(l_model_TSS = model_TSS$l_acc, u_model_TSS = model_TSS$r_acc, mean_model_TSS = model_TSS$mean_acc,
                                l_data_TSS = data_TSS$l_TSS, u_data_TSS = data_TSS$u_TSS, mean_data_TSS = data_TSS$mean_TSS,
                                ngut = model_TSS$propn)

model_data_TSS_gu_ratio <-
  model_data_TSS_gu %>%
  mutate(ratio = mean_model_TSS/mean_data_TSS)

plot_model_data_TSS_gu_ratio <- ggplot(model_data_TSS_gu_ratio) +
  geom_point(aes(x = ngut, ratio), shape = 1) +
  geom_abline(slope = 0, intercept = log10(1), linetype = 2) +
  scale_y_log10() +
  theme_classic() +
  xlab("Number of predator guts") +
  ylab(TeX(r'($\frac{TSS(Model)}{TSS(Data)}$)')) +
  labs(tag = "(l)") +
  scale_x_continuous(breaks = as.integer(seq(1, 87, length = 5))) +
  theme(plot.tag = element_text(face = "bold"))


p6 <- ggarrange(dd_plot_guampoe, plot_model_data_TSS_gu_ratio)
p6 <- annotate_figure(p6, top = text_grob("Guampoe", size = 20))



####################################################################################################
fw_name <- "Trancura size_agg"

dd_tr <- readRDS(paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_ind_predator/dd_tr.Rdata"))

min_gut_tr <- df_min_gut_vs_S$n_min_gut[which(df_min_gut_vs_S$foodweb == "Trancura")]

tsize <- 30
dd_plot_trancura <- ggplot(dd_tr) +
  geom_vline(xintercept = min_gut_tr, color = "red") +
  geom_line(aes(x = propn, y = mean_acc), 
            position=position_dodge(width = 3)) +
  geom_ribbon(aes(x = propn, ymin = l_acc, ymax = r_acc), alpha = 0.5, show.legend = FALSE,
              position=position_dodge(width = 3)) +
  xlab("Number of predator guts") +
  ylab("True skill statistic") +
  theme_classic() +
  scale_color_manual(name = "Gut content data", values = c("red", "blue")) +
  scale_x_continuous(breaks = as.integer(seq(1, 47, length = 5))) +
  labs(tag = "(m)") +
  theme(plot.tag = element_text(face = "bold")) 

model_TSS <- readRDS(paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_ind_predator/dd_tr.Rdata"))
data_TSS <- readRDS(paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_ind_predator/empirical_TSS_ngut.Rdata"))

model_data_TSS_tr <- data.frame(l_model_TSS = model_TSS$l_acc, u_model_TSS = model_TSS$r_acc, mean_model_TSS = model_TSS$mean_acc,
                                l_data_TSS = data_TSS$l_TSS, u_data_TSS = data_TSS$u_TSS, mean_data_TSS = data_TSS$mean_TSS,
                                ngut = model_TSS$propn)

model_data_TSS_tr_ratio <-
  model_data_TSS_tr %>%
  mutate(ratio = mean_model_TSS/mean_data_TSS)

plot_model_data_TSS_tr_ratio <- ggplot(model_data_TSS_tr_ratio) +
  geom_point(aes(x = ngut, ratio), shape = 1) +
  geom_abline(slope = 0, intercept = log10(1), linetype = 2) +
  scale_y_log10() +
  theme_classic() +
  xlab("Number of predator guts") +
  ylab(TeX(r'($\frac{TSS(Model)}{TSS(Data)}$)')) +
  labs(tag = "(n)") +
  scale_x_continuous(breaks = as.integer(seq(1, 47, length = 5))) +
  theme(plot.tag = element_text(face = "bold"))


p7 <- ggarrange(dd_plot_trancura, plot_model_data_TSS_tr_ratio)
p7 <- annotate_figure(p7, top = text_grob("Trancura", size = 20))



############***********#############

p1234567 <- ggarrange(p1, p2, p3, p4, p5, p6, p7, nrow = 4, ncol = 2)
# ggsave(plot = p1234567, filename = "results/misc/plot_TSS_ratio.png", width = 12, height = 9)
