# 2022.05.26
# We plot the TSS of the predicted food webs wrt to number of predator guts

library(ggplot2)
library(ggpubr)

df_min_gut_vs_S <- readRDS("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Misc/df_min_gut_vs_S.RDS")

#### Broadstone Stream #####

####################################################################################################
fw_name <- "Broadstone Stream size_agg_v2"

dd_bs <- readRDS(paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_ind_predator/dd_bs.Rdata"))

min_gut_bs <- df_min_gut_vs_S$n_min_gut[which(df_min_gut_vs_S$foodweb == "Broadstone Stream")]

tsize <- 30

### Black background ####
# ggplot(dd_bs) +
#   geom_vline(xintercept = min_gut_bs, color = "red", size = 2) +
#   geom_line(aes(x = propn, y = mean_acc), size = 1, color = "white") +
#   geom_ribbon(aes(x = propn, ymin = l_acc, ymax = r_acc), alpha = 0.5, show.legend = FALSE,
#               position=position_dodge(width = 3), fill = "white") +
#   xlab("Number of predator guts") +
#   ylab("True skill statistic") +
#   theme_classic() +
#   scale_color_manual(name = "Gut content data", values = c("red", "blue")) +
#   scale_x_continuous(breaks = as.integer(seq(1, 1008, length = 5))) +
#   ggtitle("Broadstone Stream") +
#   theme(text = element_text(size = 20, face = "bold", color = "white"), plot.title=element_text(hjust=0.5, size = 40, vjust = 3.5),
#         panel.background = element_rect(fill = "black", colour = "black"),
#         plot.background = element_rect(fill = "black", colour = "black"),
#         axis.line = element_line(color  = "white"),
#         axis.ticks = element_line(colour = "white"),
#         axis.text = element_text(colour = "white"),
#         axis.title.y = element_text(margin = margin(r = 10)),
#         axis.title.x = element_text(margin = margin(t = 10)),
#         plot.margin  = margin(t = 25))


ggplot(dd_bs) +
  geom_vline(xintercept = min_gut_bs, color = "red", size = 2) +
  geom_line(aes(x = propn, y = mean_acc), size = 1) +
  geom_ribbon(aes(x = propn, ymin = l_acc, ymax = r_acc), alpha = 0.5, show.legend = FALSE,
              position=position_dodge(width = 3)) +
  xlab("Number of predator guts") +
  ylab("True skill statistic") +
  theme_classic() +
  scale_color_manual(name = "Gut content data", values = c("red", "blue")) +
  scale_x_continuous(breaks = as.integer(seq(1, 1008, length = 5))) +
  ggtitle("Broadstone Stream") +
  theme(text = element_text(size = 20, face = "bold"), plot.title=element_text(hjust=0.5, size = 40, vjust = 3.5),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        plot.margin  = margin(t = 25))

# ggsave(filename = "results/misc/TSS_model_vs_ngut_bs.png", dpi = 600)



##### Combining all the other food webs #####


dd_plot_broadstone_stream <- ggplot(dd_bs) +
  geom_vline(xintercept = min_gut_bs, color = "red", size = 2) +
  geom_line(aes(x = propn, y = mean_acc), color = "white") +
  geom_ribbon(aes(x = propn, ymin = l_acc, ymax = r_acc), alpha = 0.5, show.legend = FALSE,
              fill = "white") +
  xlab("Sampling effort (Number of predator guts)") +
  ylab("True skill statistic") +
  theme_classic() +
  scale_x_continuous(breaks = as.integer(seq(1, 1008, length = 5))) +
  theme(plot.tag = element_text(face = "bold"))  +
  theme(text = element_text(face = "bold", color = "white"), plot.title=element_text(hjust=0.5),
        panel.background = element_rect(fill = "black", colour = "black"),
        plot.background = element_rect(fill = "black", colour = "black"),
        axis.line = element_line(color  = "white"),
        axis.ticks = element_line(colour = "white"),
        axis.text = element_text(colour = "white"),
        axis.title = element_text(colour = "black"))


plot_placeholder <- ggplot(dd_bs) +
  geom_line(aes(x = propn, y = mean_acc), color = "black") +
  geom_ribbon(aes(x = propn, ymin = l_acc, ymax = r_acc), alpha = 0.5, show.legend = FALSE,
              fill = "black") +
  xlab("Sampling effort (Number of predator guts)") +
  ylab("True skill statistic") +
  theme_classic() +
  scale_x_continuous(breaks = as.integer(seq(1, 1008, length = 5))) +
  theme(plot.tag = element_text(face = "bold"))  +
  theme(text = element_text(face = "bold", color = "black"), plot.title=element_text(hjust=0.5),
        panel.background = element_rect(fill = "black", colour = "black"),
        plot.background = element_rect(fill = "black", colour = "black"),
        axis.line = element_line(color  = "black"),
        axis.ticks = element_line(colour = "black"),
        axis.text = element_text(colour = "black"))

p1 <- ggarrange(dd_plot_broadstone_stream, plot_placeholder)
p1 <- annotate_figure(p1, top = text_grob("Broadstone Stream", size = 20, color = "white", vjust = 0.3))+ bgcolor("black")


#### Celtic Sea ####
fw_name <- "Celtic Sea size_agg"

dd_cs <- readRDS(paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_ind_predator/dd_cs.Rdata"))

min_gut_cs <- df_min_gut_vs_S$n_min_gut[which(df_min_gut_vs_S$foodweb == "Celtic Sea")]

tsize <- 30
dd_plot_celtic_sea <- ggplot(dd_cs) +
  geom_vline(xintercept = min_gut_cs, color = "red", size = 2) +
  geom_line(aes(x = propn, y = mean_acc), color = "white") +
  geom_ribbon(aes(x = propn, ymin = l_acc, ymax = r_acc), alpha = 0.5, show.legend = FALSE,
              fill = "white") +
  xlab("Sampling effort (Number of predator guts)") +
  ylab("True skill statistic") +
  theme_classic() +
  scale_x_continuous(breaks = as.integer(seq(1, 491, length = 5))) +
  theme(plot.tag = element_text(face = "bold"))  +
  theme(text = element_text(face = "bold", color = "white"), plot.title=element_text(hjust=0.5),
        panel.background = element_rect(fill = "black", colour = "black"),
        plot.background = element_rect(fill = "black", colour = "black"),
        axis.line = element_line(color  = "white"),
        axis.ticks = element_line(colour = "white"),
        axis.text = element_text(colour = "white"),
        axis.title = element_text(colour = "black"))

p2 <- ggarrange(dd_plot_celtic_sea, plot_placeholder)
p2 <- annotate_figure(p2, top = text_grob("Celtic Sea", size = 20, color = "white", vjust = 0.3))+ bgcolor("black")

#### Tadnoll Brook ####
fw_name <- "Tadnoll Brook size_agg"

dd_tb <- readRDS(paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_ind_predator/dd_tb.Rdata"))

min_gut_tb <- df_min_gut_vs_S$n_min_gut[which(df_min_gut_vs_S$foodweb == "Tadnoll Brook")]

tsize <- 30
dd_plot_tadnoll_brook <- ggplot(dd_tb) +
  geom_vline(xintercept = min_gut_tb, color = "red", size = 2) +
  geom_line(aes(x = propn, y = mean_acc), color = "white") +
  geom_ribbon(aes(x = propn, ymin = l_acc, ymax = r_acc), alpha = 0.5, show.legend = FALSE,
              fill = "white") +
  xlab("Sampling effort (Number of predator guts)") +
  ylab("True skill statistic") +
  theme_classic() +
  scale_x_continuous(breaks = as.integer(seq(1, 688, length = 5))) +
  theme(plot.tag = element_text(face = "bold"))  +
  theme(text = element_text(face = "bold", color = "white"), plot.title=element_text(hjust=0.5),
        panel.background = element_rect(fill = "black", colour = "black"),
        plot.background = element_rect(fill = "black", colour = "black"),
        axis.line = element_line(color  = "white"),
        axis.ticks = element_line(colour = "white"),
        axis.text = element_text(colour = "white"),
        axis.title = element_text(colour = "black"))

p3 <- ggarrange(dd_plot_tadnoll_brook, plot_placeholder)
p3 <- annotate_figure(p3, top = text_grob("Tadnoll Brook", size = 20, color = "white", vjust = 0.3))+ bgcolor("black")


#### Afon Hirnant ####
fw_name <- "Afon Hirnant size_agg"

dd_ah <- readRDS(paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_ind_predator/dd_ah.Rdata"))

min_gut_ah <- df_min_gut_vs_S$n_min_gut[which(df_min_gut_vs_S$foodweb == "Afon Hirnant")]

tsize <- 30
dd_plot_afon_hirnant <- ggplot(dd_ah) +
  geom_vline(xintercept = min_gut_ah, color = "red", size = 2) +
  geom_line(aes(x = propn, y = mean_acc), color = "white") +
  geom_ribbon(aes(x = propn, ymin = l_acc, ymax = r_acc), alpha = 0.5, show.legend = FALSE,
              fill = "white") +
  xlab("Sampling effort (Number of predator guts)") +
  ylab("True skill statistic") +
  theme_classic() +
  scale_x_continuous(breaks = as.integer(seq(1, 175, length = 5))) +
  theme(plot.tag = element_text(face = "bold"))  +
  theme(text = element_text(face = "bold", color = "white"), plot.title=element_text(hjust=0.5),
        panel.background = element_rect(fill = "black", colour = "black"),
        plot.background = element_rect(fill = "black", colour = "black"),
        axis.line = element_line(color  = "white"),
        axis.ticks = element_line(colour = "white"),
        axis.text = element_text(colour = "white"),
        axis.title = element_text(colour = "black"))

p4 <- ggarrange(dd_plot_afon_hirnant, plot_placeholder)
p4 <- annotate_figure(p4, top = text_grob("Afon Hirnant", size = 20, color = "white", vjust = 0.3))+ bgcolor("black")



#### Coilaco ####
fw_name <- "Coilaco size_agg"

dd_co <- readRDS(paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_ind_predator/dd_co.Rdata"))

min_gut_co <- df_min_gut_vs_S$n_min_gut[which(df_min_gut_vs_S$foodweb == "Coilaco")]

tsize <- 30
dd_plot_coilaco <- ggplot(dd_co) +
  geom_vline(xintercept = min_gut_co, color = "red", size = 2) +
  geom_line(aes(x = propn, y = mean_acc), color = "white") +
  geom_ribbon(aes(x = propn, ymin = l_acc, ymax = r_acc), alpha = 0.5, show.legend = FALSE,
              fill = "white") +
  xlab("Sampling effort (Number of predator guts)") +
  ylab("True skill statistic") +
  theme_classic() +
  scale_x_continuous(breaks = as.integer(seq(1, 74, length = 5))) +
  theme(plot.tag = element_text(face = "bold"))  +
  theme(text = element_text(face = "bold", color = "white"), plot.title=element_text(hjust=0.5),
        panel.background = element_rect(fill = "black", colour = "black"),
        plot.background = element_rect(fill = "black", colour = "black"),
        axis.line = element_line(color  = "white"),
        axis.ticks = element_line(colour = "white"),
        axis.text = element_text(colour = "white"),
        axis.title = element_text(colour = "black"))

p5 <- ggarrange(dd_plot_coilaco, plot_placeholder)
p5 <- annotate_figure(p5, top = text_grob("Coilaco", size = 20, color = "white", vjust = 0.3))+ bgcolor("black")


#### Guampoe ####
fw_name <- "Guampoe size_agg"

dd_gu <- readRDS(paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_ind_predator/dd_gu.Rdata"))

min_gut_gu <- df_min_gut_vs_S$n_min_gut[which(df_min_gut_vs_S$foodweb == "Guampoe")]

tsize <- 30
dd_plot_guampoe <- ggplot(dd_gu) +
  geom_vline(xintercept = min_gut_gu, color = "red", size = 2) +
  geom_line(aes(x = propn, y = mean_acc), color = "white") +
  geom_ribbon(aes(x = propn, ymin = l_acc, ymax = r_acc), alpha = 0.5, show.legend = FALSE,
              fill = "white") +
  xlab("Sampling effort (Number of predator guts)") +
  ylab("True skill statistic") +
  theme_classic() +
  scale_x_continuous(breaks = as.integer(seq(1, 87, length = 5))) +
  theme(plot.tag = element_text(face = "bold"))  +
  theme(text = element_text(face = "bold", color = "white"), plot.title=element_text(hjust=0.5),
        panel.background = element_rect(fill = "black", colour = "black"),
        plot.background = element_rect(fill = "black", colour = "black"),
        axis.line = element_line(color  = "white"),
        axis.ticks = element_line(colour = "white"),
        axis.text = element_text(colour = "white"),
        axis.title = element_text(colour = "black"))

p6 <- ggarrange(dd_plot_guampoe, plot_placeholder)
p6 <- annotate_figure(p6, top = text_grob("Guampoe", size = 20, color = "white", vjust = 0.3))+ bgcolor("black")

#### Trancura ####
fw_name <- "Trancura size_agg"

dd_tr <- readRDS(paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_ind_predator/dd_tr.Rdata"))

min_gut_tr <- df_min_gut_vs_S$n_min_gut[which(df_min_gut_vs_S$foodweb == "Trancura")]

tsize <- 30
dd_plot_trancura <- ggplot(dd_tr) +
  geom_vline(xintercept = min_gut_tr, color = "red", size = 2) +
  geom_line(aes(x = propn, y = mean_acc), color = "white") +
  geom_ribbon(aes(x = propn, ymin = l_acc, ymax = r_acc), alpha = 0.5, show.legend = FALSE,
              fill = "white") +
  xlab("Sampling effort (Number of predator guts)") +
  ylab("True skill statistic") +
  theme_classic() +
  scale_x_continuous(breaks = as.integer(seq(1, 47, length = 5))) +
  theme(plot.tag = element_text(face = "bold"))  +
  theme(text = element_text(face = "bold", color = "white"), plot.title=element_text(hjust=0.5),
        panel.background = element_rect(fill = "black", colour = "black"),
        plot.background = element_rect(fill = "black", colour = "black"),
        axis.line = element_line(color  = "white"),
        axis.ticks = element_line(colour = "white"),
        axis.text = element_text(colour = "white"),
        axis.title = element_text(colour = "black"))

p7 <- ggarrange(dd_plot_trancura, plot_placeholder)
p7 <- annotate_figure(p7, top = text_grob("Trancura", size = 20, color = "white", vjust = 0.3))+ bgcolor("black")


p1234567 <- ggarrange(p1, p2, p3, p4, p5, p6, p7, nrow = 4, ncol = 2)
p1234567 <- annotate_figure(p1234567, left = text_grob("True skill statistic", rot = 90, color = "white", size = 20),
                            bottom = text_grob("Number of predator guts", color = "white", size = 20, vjust = 0.1)) + bgcolor("black")

# ggsave(plot = p1234567, filename = "results/misc/plot_TSS_ratio_black_bg.png", width = 12, height = 9)

