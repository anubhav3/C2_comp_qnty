##### Combining all the other food webs #####


dd_plot_broadstone_stream <- ggplot(dd_bs) +
  geom_vline(xintercept = min_gut_bs, color = "red", size = 2) +
  geom_line(aes(x = propn, y = mean_acc), color = "black") +
  geom_ribbon(aes(x = propn, ymin = l_acc, ymax = r_acc), alpha = 0.5, show.legend = FALSE,
              fill = "black") +
  xlab("Sampling effort (Number of predator guts)") +
  ylab("True skill statistic") +
  theme_classic() +
  scale_x_continuous(breaks = as.integer(seq(1, 1008, length = 5))) +
  theme(plot.tag = element_text(face = "bold"))  +
  theme(text = element_text(face = "bold", color = "white"), plot.title=element_text(hjust=0.5),
        
        
        
        
        
        )


plot_placeholder <- ggplot(dd_bs) +
  geom_line(aes(x = propn, y = mean_acc), color = "white") +
  geom_ribbon(aes(x = propn, ymin = l_acc, ymax = r_acc), alpha = 0.5, show.legend = FALSE,
              fill = "white") +
  xlab("Sampling effort (Number of predator guts)") +
  ylab("True skill statistic") +
  theme_classic() +
  scale_x_continuous(breaks = as.integer(seq(1, 1008, length = 5))) +
  theme(plot.tag = element_text(face = "bold"))  +
  theme(text = element_text(face = "bold", color = "white"), plot.title=element_text(hjust=0.5),
        axis.line = element_line(color  = "white"),
        axis.ticks = element_line(colour = "white"),
        axis.text = element_text(colour = "white"))

p1 <- ggarrange(dd_plot_broadstone_stream, plot_placeholder)
p1 <- annotate_figure(p1, top = text_grob("Broadstone Stream", size = 20))


#### Celtic Sea ####
fw_name <- "Celtic Sea size_agg"

dd_cs <- readRDS(paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_ind_predator/dd_cs.Rdata"))

min_gut_cs <- df_min_gut_vs_S$n_min_gut[which(df_min_gut_vs_S$foodweb == "Celtic Sea")]

tsize <- 30
dd_plot_celtic_sea <- ggplot(dd_cs) +
  geom_vline(xintercept = min_gut_cs, color = "red", size = 2) +
  geom_line(aes(x = propn, y = mean_acc), color = "black") +
  geom_ribbon(aes(x = propn, ymin = l_acc, ymax = r_acc), alpha = 0.5, show.legend = FALSE,
              fill = "black") +
  xlab("Sampling effort (Number of predator guts)") +
  ylab("True skill statistic") +
  theme_classic() +
  scale_x_continuous(breaks = as.integer(seq(1, 491, length = 5))) +
  theme(plot.tag = element_text(face = "bold"))  +
  scale_y_continuous(breaks = c(-0.1, 0.1, 0.3, 0.5, 0.7)) +
  theme(text = element_text(face = "bold", color = "white"), plot.title=element_text(hjust=0.5))

p2 <- ggarrange(dd_plot_celtic_sea, plot_placeholder)
p2 <- annotate_figure(p2, top = text_grob("Celtic Sea", size = 20))

#### Tadnoll Brook ####
fw_name <- "Tadnoll Brook size_agg"

dd_tb <- readRDS(paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_ind_predator/dd_tb.Rdata"))

min_gut_tb <- df_min_gut_vs_S$n_min_gut[which(df_min_gut_vs_S$foodweb == "Tadnoll Brook")]

tsize <- 30
dd_plot_tadnoll_brook <- ggplot(dd_tb) +
  geom_vline(xintercept = min_gut_tb, color = "red", size = 2) +
  geom_line(aes(x = propn, y = mean_acc), color = "black") +
  geom_ribbon(aes(x = propn, ymin = l_acc, ymax = r_acc), alpha = 0.5, show.legend = FALSE,
              fill = "black") +
  xlab("Sampling effort (Number of predator guts)") +
  ylab("True skill statistic") +
  theme_classic() +
  scale_x_continuous(breaks = as.integer(seq(1, 688, length = 5))) +
  theme(plot.tag = element_text(face = "bold"))  +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75)) +
  theme(text = element_text(face = "bold", color = "white"), plot.title=element_text(hjust=0.5))

p3 <- ggarrange(dd_plot_tadnoll_brook, plot_placeholder)
p3 <- annotate_figure(p3, top = text_grob("Tadnoll Brook", size = 20))


#### Afon Hirnant ####
fw_name <- "Afon Hirnant size_agg"

dd_ah <- readRDS(paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_ind_predator/dd_ah.Rdata"))

min_gut_ah <- df_min_gut_vs_S$n_min_gut[which(df_min_gut_vs_S$foodweb == "Afon Hirnant")]

tsize <- 30
dd_plot_afon_hirnant <- ggplot(dd_ah) +
  geom_vline(xintercept = min_gut_ah, color = "red", size = 2) +
  geom_line(aes(x = propn, y = mean_acc), color = "black") +
  geom_ribbon(aes(x = propn, ymin = l_acc, ymax = r_acc), alpha = 0.5, show.legend = FALSE,
              fill = "black") +
  xlab("Sampling effort (Number of predator guts)") +
  ylab("True skill statistic") +
  theme_classic() +
  scale_x_continuous(breaks = as.integer(seq(1, 175, length = 5))) +
  theme(plot.tag = element_text(face = "bold"))  +
  scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5)) +
  theme(text = element_text(face = "bold", color = "white"), plot.title=element_text(hjust=0.5),
        
        
        
        
        
        )

p4 <- ggarrange(dd_plot_afon_hirnant, plot_placeholder)
p4 <- annotate_figure(p4, top = text_grob("Afon Hirnant", size = 20))



#### Coilaco ####
fw_name <- "Coilaco size_agg"

dd_co <- readRDS(paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_ind_predator/dd_co.Rdata"))

min_gut_co <- df_min_gut_vs_S$n_min_gut[which(df_min_gut_vs_S$foodweb == "Coilaco")]

tsize <- 30
dd_plot_coilaco <- ggplot(dd_co) +
  geom_vline(xintercept = min_gut_co, color = "red", size = 2) +
  geom_line(aes(x = propn, y = mean_acc), color = "black") +
  geom_ribbon(aes(x = propn, ymin = l_acc, ymax = r_acc), alpha = 0.5, show.legend = FALSE,
              fill = "black") +
  xlab("Sampling effort (Number of predator guts)") +
  ylab("True skill statistic") +
  theme_classic() +
  scale_x_continuous(breaks = as.integer(seq(1, 74, length = 5))) +
  theme(plot.tag = element_text(face = "bold"))  +
  scale_y_continuous(breaks = c(0.0, 0.1, 0.2, 0.3, 0.4)) +
  theme(text = element_text(face = "bold", color = "white"), plot.title=element_text(hjust=0.5))

p5 <- ggarrange(dd_plot_coilaco, plot_placeholder)
p5 <- annotate_figure(p5, top = text_grob("Coilaco", size = 20))


#### Guampoe ####
fw_name <- "Guampoe size_agg"

dd_gu <- readRDS(paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_ind_predator/dd_gu.Rdata"))

min_gut_gu <- df_min_gut_vs_S$n_min_gut[which(df_min_gut_vs_S$foodweb == "Guampoe")]

tsize <- 30
dd_plot_guampoe <- ggplot(dd_gu) +
  geom_vline(xintercept = min_gut_gu, color = "red", size = 2) +
  geom_line(aes(x = propn, y = mean_acc), color = "black") +
  geom_ribbon(aes(x = propn, ymin = l_acc, ymax = r_acc), alpha = 0.5, show.legend = FALSE,
              fill = "black") +
  xlab("Sampling effort (Number of predator guts)") +
  ylab("True skill statistic") +
  theme_classic() +
  scale_x_continuous(breaks = as.integer(seq(1, 87, length = 5))) +
  theme(plot.tag = element_text(face = "bold"))  +
  scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5)) +
  theme(text = element_text(face = "bold", color = "white"), plot.title=element_text(hjust=0.5),
        
        
        
        
        
        )

p6 <- ggarrange(dd_plot_guampoe, plot_placeholder)
p6 <- annotate_figure(p6, top = text_grob("Guampoe", size = 20))

#### Trancura ####
fw_name <- "Trancura size_agg"

dd_tr <- readRDS(paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_ind_predator/dd_tr.Rdata"))

min_gut_tr <- df_min_gut_vs_S$n_min_gut[which(df_min_gut_vs_S$foodweb == "Trancura")]

tsize <- 30
dd_plot_trancura <- ggplot(dd_tr) +
  geom_vline(xintercept = min_gut_tr, color = "red", size = 2) +
  geom_line(aes(x = propn, y = mean_acc), color = "black") +
  geom_ribbon(aes(x = propn, ymin = l_acc, ymax = r_acc), alpha = 0.5, show.legend = FALSE,
              fill = "black") +
  xlab("Sampling effort (Number of predator guts)") +
  ylab("True skill statistic") +
  theme_classic() +
  scale_x_continuous(breaks = as.integer(seq(1, 47, length = 5))) +
  theme(plot.tag = element_text(face = "bold"))  +
  scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5)) +
  theme(text = element_text(face = "bold", color = "white"), plot.title=element_text(hjust=0.5),
        
        
        
        
        
        )

p7 <- ggarrange(dd_plot_trancura, plot_placeholder)
p7 <- annotate_figure(p7, top = text_grob("Trancura", size = 20))


p1234567 <- ggarrange(p1, p2, p3, p4, p5, p6, p7, nrow = 4, ncol = 2)
p1234567 <- annotate_figure(p1234567, left = text_grob("True skill statistic (TSS)", rot = 90, size = 20),
                            bottom = text_grob("Number of predator guts", size = 20, vjust = 0.1)) 

# ggsave(plot = p1234567, filename = "results/misc/plot_TSS_ratio_1.png", width = 12, height = 9)


