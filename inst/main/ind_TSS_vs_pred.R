#Rule ind
#We plot TSS with number of predator diets sampled

## "Broadstone Stream size_agg_v2"
fw_name <- "sim_best_Broadstone Stream size_agg_v2"
dir_N <- 1e5
dir_tol <- 2 #different for ind and rand
n_gut_sample <- 100  
propn <- c(seq(1, 1008, 20), 1008)
n_sel <- 1
n <- length(propn)
l_acc <- numeric(n)
r_acc <- numeric(n)
mean_acc <- numeric(n)

l_FPR <- numeric(n)
r_FPR <- numeric(n)
mean_FPR <- numeric(n)

desc_main <- "TSS_gut_ind"
rule <- "ind_predator"
l_ind <- 1
for(i in propn){
  
  desc <- paste(desc_main, '_l', i, sep = '')
  # dir_main <- paste("results/rejection/",fw_name,"/", "rule_", rule,'/N=', dir_N, '_tol=', dir_tol, '_n_diet = ', i,
  #                   sep = "")
  dir_main <- paste("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_", rule ,'/N=', dir_N, '_tol=', dir_tol, '_n_diet = ', i,
                    sep = "")
  sim_TSS <- c()
  sim_FPR <- c()
  for(n_sample  in 1:n_gut_sample){
    dirnam <- paste(c(dir_main, '/rn_sample=', n_sample,'_N=', dir_N, '_tol=', dir_tol,"_", desc), collapse = '')
    fname <- paste(dirnam, "/", fw_name, ".Rdata", sep = "")
    prop_acc <- readRDS(fname)
    gut_TSS <- 1 - prop_acc$dist_gut
    gut_TSS_indexes <- order(gut_TSS, decreasing = TRUE)[1:n_sel]
    sim_TSS_temp <- prop_acc$TSS_fw[gut_TSS_indexes]
    sim_TSS <- c(sim_TSS, sim_TSS_temp)
    
    sim_FPR_temp <- prop_acc$FPR_fw[gut_TSS_indexes]
    sim_FPR <- c(sim_FPR, sim_FPR_temp)
  }
  
  range_acc <- range(sim_TSS)
  l_acc[l_ind] <- range_acc[1]
  r_acc[l_ind] <- range_acc[2]
  mean_acc[l_ind] <- mean(sim_TSS)
  
  range_FPR <- range(sim_FPR)
  l_FPR[l_ind] <- range_FPR[1]
  r_FPR[l_ind] <- range_FPR[2]
  mean_FPR[l_ind] <- mean(sim_FPR)
  
  l_ind <- l_ind+1
  print(i)
}


dd_bs <- data.frame(l_acc = l_acc, r_acc = r_acc, mean_acc = mean_acc, 
                    l_FPR = l_FPR, r_FPR = r_FPR, mean_FPR = mean_FPR,
                    propn = propn)


# saveRDS(object = dd_bs,
#         file = paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_ind_predator/dd_bs.Rdata"))

min_gut_bs <- df_min_gut_vs_S$n_min_gut[which(df_min_gut_vs_S$foodweb == "Broadstone Stream")]
  
tsize <- 30
dd_plot_broadstone_stream <- ggplot(dd_bs) +
  geom_vline(xintercept = min_gut_bs, color = "red") +
  # geom_line(aes(x = propn, y = mean_acc)) +
  # geom_rect(aes(xmin = 0, xmax = 312, ymin = -Inf, ymax = Inf), alpha = 0.01, fill = "red", color = "red") +
  geom_line(aes(x = propn, y = mean_acc), 
             position=position_dodge(width = 3)) +
  geom_ribbon(aes(x = propn, ymin = l_acc, ymax = r_acc), alpha = 0.5, show.legend = FALSE,
                position=position_dodge(width = 3)) +
  xlab("Number of predator guts") +
  ylab("True skill statistic") +
  theme_classic() +
  scale_color_manual(name = "Gut content data", values = c("red", "blue")) +
  # scale_x_discrete(limits = c(8, 248, 508, 748, 1008)) +
  # labs(title = "Broadstone Stream", tag = "(a)") +
  labs(tag = "(a)") +
  theme(plot.tag = element_text(face = "bold")) 
  # scale_x_continuous(breaks = as.integer(seq(1, 1008, length = 5)))
  # theme(plot.title = element_text(hjust=0.5, vjust = 2.5, size = 20), plot.tag = element_text(face = "bold")) +
  # scale_x_continuous(sec.axis = sec_axis(~./10.08, name = "Amount of predator guts (%)", breaks = c(1, 25, 50, 75, 100),
  #                                        labels = c("1%", "25%", "50%", "75%", "100%")),
  #                    breaks = c(8, 248, 508, 748, 1008))


dd_plot_Broadstone_Stream_FPR <- ggplot(dd_bs) +
  geom_line(aes(x = propn, y = mean_FPR), 
            position=position_dodge(width = 3)) +
  geom_ribbon(aes(x = propn, ymin = l_FPR, ymax = r_FPR), alpha = 0.5, show.legend = FALSE,
              position=position_dodge(width = 3)) 


# ggsave(filename = "results/rejection/Broadstone Stream size_agg_v2/rule_ind/TSS_with_n_pred_prop_n_sel=1.png", plot = dd_plot,
       # width = 15, height = 10)



## Celtic Sea size_agg
fw_name <- "Celtic Sea size_agg"
dir_N <- 1e5
dir_tol <- 2 #different for ind and rand
n_gut_sample <- 100  
propn <- seq(1, 491, by = 10)
n_sel <- 1
n <- length(propn)
l_acc <- numeric(n)
r_acc <- numeric(n)
mean_acc <- numeric(n)


l_FPR <- numeric(n)
r_FPR <- numeric(n)
mean_FPR <- numeric(n)


desc_main <- "TSS_gut_ind"
rule <- "ind_predator"
l_ind <- 1
for(i in propn){
  
  desc <- paste(desc_main, '_l', i, sep = '')
  # dir_main <- paste("results/rejection/",fw_name,"/", "rule_", rule,'/N=', dir_N, '_tol=', dir_tol, '_n_diet = ', i,
  #                   sep = "")
  dir_main <- paste("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_", rule ,'/N=', dir_N, '_tol=', dir_tol, '_n_diet = ', i,
                    sep = "")
  sim_TSS <- c()
  sim_FPR <- c()
  for(n_sample  in 1:n_gut_sample){
    dirnam <- paste(c(dir_main, '/rn_sample=', n_sample,'_N=', dir_N, '_tol=', dir_tol,"_", desc), collapse = '')
    fname <- paste(dirnam, "/", fw_name, ".Rdata", sep = "")
    prop_acc <- readRDS(fname)
    gut_TSS <- 1 - prop_acc$dist_gut
    gut_TSS_indexes <- order(gut_TSS, decreasing = TRUE)[1:n_sel]
    sim_TSS_temp <- prop_acc$TSS_fw[gut_TSS_indexes]
    sim_TSS <- c(sim_TSS, sim_TSS_temp)
    
    sim_FPR_temp <- prop_acc$FPR_fw[gut_TSS_indexes]
    sim_FPR <- c(sim_FPR, sim_FPR_temp)
  }
  
  range_acc <- range(sim_TSS)
  l_acc[l_ind] <- range_acc[1]
  r_acc[l_ind] <- range_acc[2]
  mean_acc[l_ind] <- mean(sim_TSS)
  
  range_FPR <- range(sim_FPR)
  l_FPR[l_ind] <- range_FPR[1]
  r_FPR[l_ind] <- range_FPR[2]
  mean_FPR[l_ind] <- mean(sim_FPR)
  
  l_ind <- l_ind+1
}

dd_cs <- data.frame(l_acc = l_acc, r_acc = r_acc, mean_acc = mean_acc, 
                    l_FPR = l_FPR, r_FPR = r_FPR, mean_FPR = mean_FPR,
                    propn = propn)

# saveRDS(object = dd_cs, file = "../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Celtic Sea size_agg/rule_ind_predator/dd_cs.Rdata")

min_gut_cs <- df_min_gut_vs_S$n_min_gut[which(df_min_gut_vs_S$foodweb == "Celtic Sea")]

dd_plot_celtic_sea <- ggplot(dd_cs) +
  geom_vline(xintercept = min_gut_cs, color = "red") +
  # geom_line(aes(x = propn, y = mean_acc)) +
  # geom_rect(aes(xmin = 0, xmax = 172, ymin = -Inf, ymax = Inf), alpha = 0.025, fill = "red", color = "red") +
  geom_line(aes(x = propn, y = mean_acc), 
            position=position_dodge(width = 3)) +
  geom_ribbon(aes(x = propn, ymin = l_acc, ymax = r_acc), alpha = 0.5, show.legend = FALSE,
              position=position_dodge(width = 3)) +
  xlab("Number of predator guts") +
  ylab("True skill statistic") +
  theme_classic() +
  scale_color_manual(name = "Gut content data", values = c("red", "blue")) +
  # scale_x_discrete(limits = c(8, 248, 508, 748, 1008)) +
  # labs(title = "Celtic Sea", tag = "(b)") +
  labs(tag = "(c)") +
  theme(plot.tag = element_text(face = "bold")) +
  scale_x_continuous(breaks = as.integer(seq(1, 491, length = 5)))
  # theme(plot.title = element_text(hjust=0.5, vjust = 2.5, size = 20), plot.tag = element_text(face = "bold")) 
  # scale_x_continuous(sec.axis = sec_axis(~./4.91, name = "Amount of predator guts (%)", breaks = c(2, 25, 50, 75, 100),
  #                                        labels = c("2%", "25%", "50%", "75%", "100%")),
  #                    breaks = c(11, 131, 251, 371, 491))


dd_plot_celtic_sea_FPR <- ggplot(dd_cs) +
  geom_line(aes(x = propn, y = mean_FPR), 
            position=position_dodge(width = 3)) +
  geom_ribbon(aes(x = propn, ymin = l_FPR, ymax = r_FPR), alpha = 0.5, show.legend = FALSE,
              position=position_dodge(width = 3))
  

# plot_TSS_ngut <- ggarrange(dd_plot_broadstone_stream, dd_plot_celtic_sea,
#                                       nrow = 1, ncol = 2)
# ggsave(filename = "results/misc/plot_TSS_ngut.png", plot = plot_TSS_ngut,
#        width = 9, height = 4)



## Tadnoll Brook size_agg
fw_name <- "Tadnoll Brook size_agg"
dir_N <- 1e5
dir_tol <- 2 #different for ind and rand
n_gut_sample <- 100  
propn <- c(seq(1, 688, by = 14), 688)
n_sel <- 1
n <- length(propn)
l_acc <- numeric(n)
r_acc <- numeric(n)
mean_acc <- numeric(n)

l_FPR <- numeric(n)
r_FPR <- numeric(n)
mean_FPR <- numeric(n)

desc_main <- "TSS_gut_ind"
rule <- "ind_predator"
l_ind <- 1
for(i in propn){
  
  desc <- paste(desc_main, '_l', i, sep = '')
  # dir_main <- paste("results/rejection/",fw_name,"/", "rule_", rule,'/N=', dir_N, '_tol=', dir_tol, '_n_diet = ', i,
  #                   sep = "")
  dir_main <- paste("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_", rule ,'/N=', dir_N, '_tol=', dir_tol, '_n_diet = ', i,
                    sep = "")
  sim_TSS <- c()
  sim_FPR <- c()
  for(n_sample  in 1:n_gut_sample){
    dirnam <- paste(c(dir_main, '/rn_sample=', n_sample,'_N=', dir_N, '_tol=', dir_tol,"_", desc), collapse = '')
    fname <- paste(dirnam, "/", fw_name, ".Rdata", sep = "")
    prop_acc <- readRDS(fname)
    gut_TSS <- 1 - prop_acc$dist_gut
    gut_TSS_indexes <- order(gut_TSS, decreasing = TRUE)[1:n_sel]
    
    sim_TSS_temp <- prop_acc$TSS_fw[gut_TSS_indexes]
    sim_TSS <- c(sim_TSS, sim_TSS_temp)
    
    sim_FPR_temp <- prop_acc$FPR_fw[gut_TSS_indexes]
    sim_FPR <- c(sim_FPR, sim_FPR_temp)
  }
  
  range_acc <- range(sim_TSS)
  l_acc[l_ind] <- range_acc[1]
  r_acc[l_ind] <- range_acc[2]
  mean_acc[l_ind] <- mean(sim_TSS)
  
  range_FPR <- range(sim_FPR)
  l_FPR[l_ind] <- range_FPR[1]
  r_FPR[l_ind] <- range_FPR[2]
  mean_FPR[l_ind] <- mean(sim_FPR)
  
  l_ind <- l_ind+1
}

dd_tb <- data.frame(l_acc = l_acc, r_acc = r_acc, mean_acc = mean_acc, 
                    l_FPR = l_FPR, r_FPR = r_FPR, mean_FPR = mean_FPR,
                    propn = propn)

# saveRDS(object = dd_tb, file = "../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Tadnoll Brook size_agg/rule_ind_predator/dd_tb.Rdata")

min_gut_tb <- df_min_gut_vs_S$n_min_gut[which(df_min_gut_vs_S$foodweb == "Tadnoll Brook")]

dd_plot_Tadnoll_Brook <- ggplot(dd_tb) +
  geom_vline(xintercept = min_gut_tb, color = "red") +
  # geom_line(aes(x = propn, y = mean_acc)) +
  # geom_rect(aes(xmin = 0, xmax = 172, ymin = -Inf, ymax = Inf), alpha = 0.025, fill = "red", color = "red") +
  geom_line(aes(x = propn, y = mean_acc), 
            position=position_dodge(width = 3)) +
  geom_ribbon(aes(x = propn, ymin = l_acc, ymax = r_acc), alpha = 0.5, show.legend = FALSE,
              position=position_dodge(width = 3)) +
  xlab("Number of predator guts") +
  ylab("True skill statistic") +
  theme_classic() +
  scale_color_manual(name = "Gut content data", values = c("red", "blue")) +
  # scale_x_discrete(limits = c(8, 248, 508, 748, 1008)) +
  # labs(title = "Tadnoll Brook", tag = "(c)") +
  # theme(plot.title = element_text(hjust=0.5, vjust = 2.5, size = 20), plot.tag = element_text(face = "bold")) 
  labs(tag = "(e)") +
  theme(plot.tag = element_text(face = "bold")) +
  scale_x_continuous(breaks = as.integer(seq(1, 688, length = 5)))
  # scale_x_continuous(sec.axis = sec_axis(~./6.88, name = "Amount of predator guts (%)", breaks = c(1, 25, 50, 75, 100),
  #                                        labels = c("1%", "25%", "50%", "75%", "100%")),
  #                    breaks = c(8, 168, 348, 528, 688))


dd_plot_Tadnoll_Brook_FPR <- ggplot(dd_tb) +
  geom_line(aes(x = propn, y = mean_FPR), 
            position=position_dodge(width = 3)) +
  geom_ribbon(aes(x = propn, ymin = l_FPR, ymax = r_FPR), alpha = 0.5, show.legend = FALSE,
              position=position_dodge(width = 3))


## Afon Hirnant size_agg
fw_name <- "Afon Hirnant size_agg"
dir_N <- 1e5
dir_tol <- 2
n_gut_sample <- 100  
propn <- c(seq(1, 171, by = 4), 175)
n_sel <- 1
n <- length(propn)
l_acc <- numeric(n)
r_acc <- numeric(n)
mean_acc <- numeric(n)

l_FPR <- numeric(n)
r_FPR <- numeric(n)
mean_FPR <- numeric(n)

desc_main <- "TSS_gut_ind"
rule <- "ind_predator"
l_ind <- 1
for(i in propn){
  
  desc <- paste(desc_main, '_l', i, sep = '')
  # dir_main <- paste("results/rejection/",fw_name,"/", "rule_", rule,'/N=', dir_N, '_tol=', dir_tol, '_n_diet = ', i,
  #                   sep = "")
  dir_main <- paste("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_", rule ,'/N=', dir_N, '_tol=', dir_tol, '_n_diet = ', i,
                    sep = "")
  sim_TSS <- c()
  sim_FPR <- c()
  for(n_sample  in 1:n_gut_sample){
    dirnam <- paste(c(dir_main, '/rn_sample=', n_sample,'_N=', dir_N, '_tol=', dir_tol,"_", desc), collapse = '')
    fname <- paste(dirnam, "/", fw_name, ".Rdata", sep = "")
    prop_acc <- readRDS(fname)
    gut_TSS <- 1 - prop_acc$dist_gut
    gut_TSS_indexes <- order(gut_TSS, decreasing = TRUE)[1:n_sel]
    
    sim_TSS_temp <- prop_acc$TSS_fw[gut_TSS_indexes]
    sim_TSS <- c(sim_TSS, sim_TSS_temp)
    
    sim_FPR_temp <- prop_acc$FPR_fw[gut_TSS_indexes]
    sim_FPR <- c(sim_FPR, sim_FPR_temp)
  }
  
  range_acc <- range(sim_TSS)
  l_acc[l_ind] <- range_acc[1]
  r_acc[l_ind] <- range_acc[2]
  mean_acc[l_ind] <- mean(sim_TSS)
  
  range_FPR <- range(sim_FPR)
  l_FPR[l_ind] <- range_FPR[1]
  r_FPR[l_ind] <- range_FPR[2]
  mean_FPR[l_ind] <- mean(sim_FPR)
  
  l_ind <- l_ind+1
}


dd_ah <- data.frame(l_acc = l_acc, r_acc = r_acc, mean_acc = mean_acc, 
                    l_FPR = l_FPR, r_FPR = r_FPR, mean_FPR = mean_FPR,
                    propn = propn)

# saveRDS(object = dd_ah, file = "../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Afon Hirnant size_agg/rule_ind_predator/dd_ah.Rdata")

min_gut_ah <- df_min_gut_vs_S$n_min_gut[which(df_min_gut_vs_S$foodweb == "Afon Hirnant")]

dd_plot_Afon_Hirnant <- ggplot(dd_ah) +
  geom_vline(xintercept = min_gut_ah, color = "red") +
  # geom_line(aes(x = propn, y = mean_acc)) +
  # geom_rect(aes(xmin = 0, xmax = 172, ymin = -Inf, ymax = Inf), alpha = 0.025, fill = "red", color = "red") +
  geom_line(aes(x = propn, y = mean_acc), 
            position=position_dodge(width = 3)) +
  geom_ribbon(aes(x = propn, ymin = l_acc, ymax = r_acc), alpha = 0.5, show.legend = FALSE,
              position=position_dodge(width = 3)) +
  xlab("Number of predator guts") +
  ylab("True skill statistic") +
  theme_classic() +
  scale_color_manual(name = "Gut content data", values = c("red", "blue")) +
  # scale_x_discrete(limits = c(1, 41, 86, 126, 171)) +
  # labs(title = "Afon Hirnant", tag = "(d)") +
  # theme(plot.title = element_text(hjust=0.5, vjust = 2.5, size = 20), plot.tag = element_text(face = "bold")) 
  labs(tag = "(g)") +
  theme(plot.tag = element_text(face = "bold")) +
  scale_x_continuous(breaks = as.integer(seq(1, 174, length = 5)))
  # scale_x_continuous(sec.axis = sec_axis(~./1.71, name = "Amount of predator guts (%)", breaks = c(1, 25, 50, 75, 100),
  #                                      labels = c("1%", "25%", "50%", "75%", "100%")),
  #                  breaks = c(1, 41, 86, 126, 171))

dd_plot_Afon_Hirnant_FPR <- ggplot(dd_ah) +
  geom_line(aes(x = propn, y = mean_FPR), 
            position=position_dodge(width = 3)) +
  geom_ribbon(aes(x = propn, ymin = l_FPR, ymax = r_FPR), alpha = 0.5, show.legend = FALSE,
              position=position_dodge(width = 3))


plot_TSS_ngut <- ggarrange(dd_plot_broadstone_stream, dd_plot_celtic_sea,
                           dd_plot_Tadnoll_Brook, dd_plot_Afon_Hirnant,
                           nrow = 2, ncol = 2)
# ggsave(filename = "results/misc/plot_TSS_ngut.png", plot = plot_TSS_ngut,
#        width = 9, height = 8)



## "Coilaco size_agg"
fw_name <- "Coilaco size_agg"
dir_N <- 1e5
dir_tol <- 2 #different for ind and rand
n_gut_sample <- 100  
propn <- c(seq(1, 74, 3), 74)
n_sel <- 1
n <- length(propn)
l_acc <- numeric(n)
r_acc <- numeric(n)
mean_acc <- numeric(n)

l_FPR <- numeric(n)
r_FPR <- numeric(n)
mean_FPR <- numeric(n)

desc_main <- "TSS_gut_ind"
rule <- "ind_predator"
l_ind <- 1
for(i in propn){
  
  desc <- paste(desc_main, '_l', i, sep = '')
  # dir_main <- paste("results/rejection/",fw_name,"/", "rule_", rule,'/N=', dir_N, '_tol=', dir_tol, '_n_diet = ', i,
  #                   sep = "")
  dir_main <- paste("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_", rule ,'/N=', dir_N, '_tol=', dir_tol, '_n_diet = ', i,
                    sep = "")
  sim_TSS <- c()
  sim_FPR <- c()
  for(n_sample  in 1:n_gut_sample){
    dirnam <- paste(c(dir_main, '/rn_sample=', n_sample,'_N=', dir_N, '_tol=', dir_tol,"_", desc), collapse = '')
    fname <- paste(dirnam, "/", fw_name, ".Rdata", sep = "")
    prop_acc <- readRDS(fname)
    gut_TSS <- 1 - prop_acc$dist_gut
    gut_TSS_indexes <- order(gut_TSS, decreasing = TRUE)[1:n_sel]
    sim_TSS_temp <- prop_acc$TSS_fw[gut_TSS_indexes]
    sim_TSS <- c(sim_TSS, sim_TSS_temp)
    
    sim_FPR_temp <- prop_acc$FPR_fw[gut_TSS_indexes]
    sim_FPR <- c(sim_FPR, sim_FPR_temp)
  }
  
  range_acc <- range(sim_TSS)
  l_acc[l_ind] <- range_acc[1]
  r_acc[l_ind] <- range_acc[2]
  mean_acc[l_ind] <- mean(sim_TSS)
  
  range_FPR <- range(sim_FPR)
  l_FPR[l_ind] <- range_FPR[1]
  r_FPR[l_ind] <- range_FPR[2]
  mean_FPR[l_ind] <- mean(sim_FPR)
  
  l_ind <- l_ind+1
  print(i)
}


dd_co <- data.frame(l_acc = l_acc, r_acc = r_acc, mean_acc = mean_acc, 
                    l_FPR = l_FPR, r_FPR = r_FPR, mean_FPR = mean_FPR,
                    propn = propn)


# saveRDS(object = dd_co,
#         file = paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_ind_predator/dd_co.Rdata"))

min_gut_bs <- df_min_gut_vs_S$n_min_gut[which(df_min_gut_vs_S$foodweb == "Broadstone Stream")]

tsize <- 30
dd_plot_Coilaco <- ggplot(dd_co) +
  # geom_vline(xintercept = min_gut_bs, color = "red") +
  # geom_line(aes(x = propn, y = mean_acc)) +
  # geom_rect(aes(xmin = 0, xmax = 312, ymin = -Inf, ymax = Inf), alpha = 0.01, fill = "red", color = "red") +
  geom_line(aes(x = propn, y = mean_acc), 
            position=position_dodge(width = 3)) +
  geom_ribbon(aes(x = propn, ymin = l_acc, ymax = r_acc), alpha = 0.5, show.legend = FALSE,
              position=position_dodge(width = 3)) +
  xlab("Number of predator guts") +
  ylab("True skill statistic") +
  theme_classic() +
  scale_color_manual(name = "Gut content data", values = c("red", "blue")) +
  # scale_x_discrete(limits = c(8, 248, 508, 748, 1008)) +
  # labs(title = "Broadstone Stream", tag = "(a)") +
  labs(tag = "(e)") +
  theme(plot.tag = element_text(face = "bold")) 
# scale_x_continuous(breaks = as.integer(seq(1, 1008, length = 5)))
# theme(plot.title = element_text(hjust=0.5, vjust = 2.5, size = 20), plot.tag = element_text(face = "bold")) +
# scale_x_continuous(sec.axis = sec_axis(~./10.08, name = "Amount of predator guts (%)", breaks = c(1, 25, 50, 75, 100),
#                                        labels = c("1%", "25%", "50%", "75%", "100%")),
#                    breaks = c(8, 248, 508, 748, 1008))


dd_plot_Coilaco_FPR <- ggplot(dd_co) +
  geom_line(aes(x = propn, y = mean_FPR), 
            position=position_dodge(width = 3)) +
  geom_ribbon(aes(x = propn, ymin = l_FPR, ymax = r_FPR), alpha = 0.5, show.legend = FALSE,
              position=position_dodge(width = 3)) 


# ggsave(filename = "results/misc/plot_TSS_ngut.png", plot = dd_plot,
# width = 15, height = 10)



## "Guampoe size_agg"
fw_name <- "Guampoe size_agg"
dir_N <- 1e5
dir_tol <- 2 #different for ind and rand
n_gut_sample <- 100  
propn <- c(seq(1, 87, 3), 87)
n_sel <- 1
n <- length(propn)
l_acc <- numeric(n)
r_acc <- numeric(n)
mean_acc <- numeric(n)

l_FPR <- numeric(n)
r_FPR <- numeric(n)
mean_FPR <- numeric(n)

desc_main <- "TSS_gut_ind"
rule <- "ind_predator"
l_ind <- 1
for(i in propn){
  
  desc <- paste(desc_main, '_l', i, sep = '')
  # dir_main <- paste("results/rejection/",fw_name,"/", "rule_", rule,'/N=', dir_N, '_tol=', dir_tol, '_n_diet = ', i,
  #                   sep = "")
  dir_main <- paste("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_", rule ,'/N=', dir_N, '_tol=', dir_tol, '_n_diet = ', i,
                    sep = "")
  sim_TSS <- c()
  sim_FPR <- c()
  for(n_sample  in 1:n_gut_sample){
    dirnam <- paste(c(dir_main, '/rn_sample=', n_sample,'_N=', dir_N, '_tol=', dir_tol,"_", desc), collapse = '')
    fname <- paste(dirnam, "/", fw_name, ".Rdata", sep = "")
    prop_acc <- readRDS(fname)
    gut_TSS <- 1 - prop_acc$dist_gut
    gut_TSS_indexes <- order(gut_TSS, decreasing = TRUE)[1:n_sel]
    sim_TSS_temp <- prop_acc$TSS_fw[gut_TSS_indexes]
    sim_TSS <- c(sim_TSS, sim_TSS_temp)
    
    sim_FPR_temp <- prop_acc$FPR_fw[gut_TSS_indexes]
    sim_FPR <- c(sim_FPR, sim_FPR_temp)
  }
  
  range_acc <- range(sim_TSS)
  l_acc[l_ind] <- range_acc[1]
  r_acc[l_ind] <- range_acc[2]
  mean_acc[l_ind] <- mean(sim_TSS)
  
  range_FPR <- range(sim_FPR)
  l_FPR[l_ind] <- range_FPR[1]
  r_FPR[l_ind] <- range_FPR[2]
  mean_FPR[l_ind] <- mean(sim_FPR)
  
  l_ind <- l_ind+1
  print(i)
}


dd_gu <- data.frame(l_acc = l_acc, r_acc = r_acc, mean_acc = mean_acc, 
                    l_FPR = l_FPR, r_FPR = r_FPR, mean_FPR = mean_FPR,
                    propn = propn)


# saveRDS(object = dd_gu,
#         file = paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_ind_predator/dd_gu.Rdata"))

min_gut_go <- df_min_gut_vs_S$n_min_gut[which(df_min_gut_vs_S$foodweb == "Guampoe")]

tsize <- 30
dd_plot_Guampoe <- ggplot(dd_gu) +
  # geom_vline(xintercept = min_gut_bs, color = "red") +
  # geom_line(aes(x = propn, y = mean_acc)) +
  # geom_rect(aes(xmin = 0, xmax = 312, ymin = -Inf, ymax = Inf), alpha = 0.01, fill = "red", color = "red") +
  geom_line(aes(x = propn, y = mean_acc), 
            position=position_dodge(width = 3)) +
  geom_ribbon(aes(x = propn, ymin = l_acc, ymax = r_acc), alpha = 0.5, show.legend = FALSE,
              position=position_dodge(width = 3)) +
  xlab("Number of predator guts") +
  ylab("True skill statistic") +
  theme_classic() +
  scale_color_manual(name = "Gut content data", values = c("red", "blue")) +
  # scale_x_discrete(limits = c(8, 248, 508, 748, 1008)) +
  # labs(title = "Broadstone Stream", tag = "(a)") +
  labs(tag = "(e)") +
  theme(plot.tag = element_text(face = "bold")) 
# scale_x_continuous(breaks = as.integer(seq(1, 1008, length = 5)))
# theme(plot.title = element_text(hjust=0.5, vjust = 2.5, size = 20), plot.tag = element_text(face = "bold")) +
# scale_x_continuous(sec.axis = sec_axis(~./10.08, name = "Amount of predator guts (%)", breaks = c(1, 25, 50, 75, 100),
#                                        labels = c("1%", "25%", "50%", "75%", "100%")),
#                    breaks = c(8, 248, 508, 748, 1008))


dd_plot_Guampoe_FPR <- ggplot(dd_gu) +
  geom_line(aes(x = propn, y = mean_FPR), 
            position=position_dodge(width = 3)) +
  geom_ribbon(aes(x = propn, ymin = l_FPR, ymax = r_FPR), alpha = 0.5, show.legend = FALSE,
              position=position_dodge(width = 3)) 


# ggsave(filename = "results/misc/plot_TSS_ngut.png", plot = dd_plot,
# width = 15, height = 10)
####################################



## "Trancura size_agg"
fw_name <- "Trancura size_agg"
dir_N <- 1e5
dir_tol <- 2 #different for ind and rand
n_gut_sample <- 100  
propn <- seq(1, 47, 2)
n_sel <- 1
n <- length(propn)
l_acc <- numeric(n)
r_acc <- numeric(n)
mean_acc <- numeric(n)

l_FPR <- numeric(n)
r_FPR <- numeric(n)
mean_FPR <- numeric(n)

desc_main <- "TSS_gut_ind"
rule <- "ind_predator"
l_ind <- 1
for(i in propn){
  
  desc <- paste(desc_main, '_l', i, sep = '')
  # dir_main <- paste("results/rejection/",fw_name,"/", "rule_", rule,'/N=', dir_N, '_tol=', dir_tol, '_n_diet = ', i,
  #                   sep = "")
  dir_main <- paste("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_", rule ,'/N=', dir_N, '_tol=', dir_tol, '_n_diet = ', i,
                    sep = "")
  sim_TSS <- c()
  sim_FPR <- c()
  for(n_sample  in 1:n_gut_sample){
    dirnam <- paste(c(dir_main, '/rn_sample=', n_sample,'_N=', dir_N, '_tol=', dir_tol,"_", desc), collapse = '')
    fname <- paste(dirnam, "/", fw_name, ".Rdata", sep = "")
    prop_acc <- readRDS(fname)
    gut_TSS <- 1 - prop_acc$dist_gut
    gut_TSS_indexes <- order(gut_TSS, decreasing = TRUE)[1:n_sel]
    sim_TSS_temp <- prop_acc$TSS_fw[gut_TSS_indexes]
    sim_TSS <- c(sim_TSS, sim_TSS_temp)
    
    sim_FPR_temp <- prop_acc$FPR_fw[gut_TSS_indexes]
    sim_FPR <- c(sim_FPR, sim_FPR_temp)
  }
  
  range_acc <- range(sim_TSS)
  l_acc[l_ind] <- range_acc[1]
  r_acc[l_ind] <- range_acc[2]
  mean_acc[l_ind] <- mean(sim_TSS)
  
  range_FPR <- range(sim_FPR)
  l_FPR[l_ind] <- range_FPR[1]
  r_FPR[l_ind] <- range_FPR[2]
  mean_FPR[l_ind] <- mean(sim_FPR)
  
  l_ind <- l_ind+1
  print(i)
}


dd_tr <- data.frame(l_acc = l_acc, r_acc = r_acc, mean_acc = mean_acc, 
                    l_FPR = l_FPR, r_FPR = r_FPR, mean_FPR = mean_FPR,
                    propn = propn)


# saveRDS(object = dd_tr,
#         file = paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_ind_predator/dd_tr.Rdata"))


min_gut_tr <- df_min_gut_vs_S$n_min_gut[which(df_min_gut_vs_S$foodweb == "Trancura")]

tsize <- 30
dd_plot_Trancura <- ggplot(dd_tr) +
  # geom_vline(xintercept = min_gut_bs, color = "red") +
  # geom_line(aes(x = propn, y = mean_acc)) +
  # geom_rect(aes(xmin = 0, xmax = 312, ymin = -Inf, ymax = Inf), alpha = 0.01, fill = "red", color = "red") +
  geom_line(aes(x = propn, y = mean_acc), 
            position=position_dodge(width = 3)) +
  geom_ribbon(aes(x = propn, ymin = l_acc, ymax = r_acc), alpha = 0.5, show.legend = FALSE,
              position=position_dodge(width = 3)) +
  xlab("Number of predator guts") +
  ylab("True skill statistic") +
  theme_classic() +
  scale_color_manual(name = "Gut content data", values = c("red", "blue")) +
  # scale_x_discrete(limits = c(8, 248, 508, 748, 1008)) +
  # labs(title = "Broadstone Stream", tag = "(a)") +
  labs(tag = "(e)") +
  theme(plot.tag = element_text(face = "bold")) 
# scale_x_continuous(breaks = as.integer(seq(1, 1008, length = 5)))
# theme(plot.title = element_text(hjust=0.5, vjust = 2.5, size = 20), plot.tag = element_text(face = "bold")) +
# scale_x_continuous(sec.axis = sec_axis(~./10.08, name = "Amount of predator guts (%)", breaks = c(1, 25, 50, 75, 100),
#                                        labels = c("1%", "25%", "50%", "75%", "100%")),
#                    breaks = c(8, 248, 508, 748, 1008))


dd_plot_Trancura_FPR <- ggplot(dd_tr) +
  geom_line(aes(x = propn, y = mean_FPR), 
            position=position_dodge(width = 3)) +
  geom_ribbon(aes(x = propn, ymin = l_FPR, ymax = r_FPR), alpha = 0.5, show.legend = FALSE,
              position=position_dodge(width = 3)) 


# ggsave(filename = "results/misc/plot_TSS_ngut.png", plot = dd_plot,
# width = 15, height = 10)


#############

fw_name <- "Broadstone Stream size_agg_v2"
dd_bs <- readRDS(paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_ind_predator/dd_bs.Rdata"))
dd_bs_1 <- mutate(dd_bs, foodweb = fw_name)

fw_name <- "Celtic Sea size_agg"
dd_cs <- readRDS(paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_ind_predator/dd_cs.Rdata"))
dd_cs_1 <- mutate(dd_cs, foodweb = fw_name)

fw_name <- "Tadnoll Brook size_agg"
dd_tb <- readRDS(paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_ind_predator/dd_tb.Rdata"))
dd_tb_1 <- mutate(dd_tb, foodweb = fw_name)

fw_name <- "Afon Hirnant size_agg"
dd_ah <- readRDS(paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_ind_predator/dd_ah.Rdata"))
dd_ah_1 <- mutate(dd_ah, foodweb = fw_name)

fw_name <- "Coilaco size_agg"
dd_co <- readRDS(paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_ind_predator/dd_co.Rdata"))
dd_co_1 <- mutate(dd_co, foodweb = fw_name)

fw_name <- "Guampoe size_agg"
dd_gu <- readRDS(paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_ind_predator/dd_gu.Rdata"))
dd_gu_1 <- mutate(dd_gu, foodweb = fw_name)

fw_name <- "Trancura size_agg"
dd_tr <- readRDS(paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_ind_predator/dd_tr.Rdata"))
dd_tr_1 <- mutate(dd_tr, foodweb = fw_name)

dd_all <- rbind(dd_bs_1, dd_cs_1, dd_tb_1, dd_ah_1, dd_co_1, dd_gu_1, dd_tr_1)


ggplot(dd_all) +
  geom_line(aes(x = propn, y = mean_acc, color = foodweb), 
            position=position_dodge(width = 3)) +
  geom_ribbon(aes(x = propn, ymin = l_acc, ymax = r_acc, fill = foodweb), alpha = 0.5, show.legend = FALSE,
              position=position_dodge(width = 3)) +
  xlab("Number of predator guts") +
  ylab("True skill statistic") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, vjust = 2.5, size = 20), plot.tag = element_text(face = "bold")) +
  xlim(c(1, 500))


leg_lab <- c("Broadstone Stream size_agg_v2" = "Broadstone Stream",
             "Celtic Sea size_agg" = "Celtic Sea",
             "Tadnoll Brook size_agg" = "Tadnoll Brook",
             "Afon Hirnant size_agg" = "Afon Hirnant",
             "Coilaco size_agg" = "Coilaco",
             "Guampoe size_agg" = "Guampoe",
             "Trancura size_agg" = "Trancura")

dd_plot_FPR <- ggplot(dd_all) +
  geom_line(aes(x = propn, y = mean_FPR, color = foodweb), 
            position=position_dodge(width = 3)) +
  geom_ribbon(aes(x = propn, ymin = l_FPR, ymax = r_FPR, fill = foodweb), alpha = 0.5, show.legend = FALSE,
              position=position_dodge(width = 3)) +
  facet_wrap(~foodweb, scales = "free_x", labeller = as_labeller(leg_lab)) +
  theme_classic() +
  scale_color_discrete(labels = leg_lab) +
  xlab("Number of gut content data") +
  ylab("False positive rate") +
  labs(color = "Food web") +
  theme(legend.position = "none")

# ggsave(plot = dd_plot_FPR, filename = "inst/report/fig_2021_10_15/plot_FPR.png")






