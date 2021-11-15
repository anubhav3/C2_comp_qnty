# 08.09.2021

# We compare the TSS from the true food web being the best food web from ADBM with TSS from the true food web being the 
# observed food web

# True food web being the best estimate of ADBM

fw_name_ADBM <- "sim_best_Broadstone Stream size_agg_v2"
dd_bs_ADBM <- readRDS(paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name_ADBM, "/rule_ind_predator/dd_bs.Rdata"))

threshold_per <- 0.95
dd <- dd_bs_ADBM
max_TSS <- dd$mean_acc[length(dd$mean_acc)]
threshold_TSS <- threshold_per*max_TSS

k <- 1
repeat{
  if(dd$mean_acc[k] > threshold_TSS){
    N <- k - 1
    break
  }
  k <- k + 1
}

n_min_gut_ADBM <- dd$propn[k]

# True food web being the observed food web

fw_name_obs <- "Broadstone Stream size_agg_v2"
dd_bs_obs <- readRDS(paste0("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name_obs, "/rule_ind_predator/dd_bs.Rdata"))

dd <- dd_bs_obs
max_TSS <- dd$mean_acc[length(dd$mean_acc)]
threshold_TSS <- threshold_per*max_TSS

k <- 1
repeat{
  if(dd$mean_acc[k] > threshold_TSS){
    N <- k - 1
    break
  }
  k <- k + 1
}

n_min_gut_obs <- dd$propn[k]


dd_bs <- data.frame(l_TSS = dd_bs_ADBM$l_acc, r_TSS = dd_bs_ADBM$r_acc, mean_TSS = dd_bs_ADBM$mean_acc,
                    ngut = dd_bs_ADBM$propn, true_foodweb = "from ADBM")

dd_bs <- rbind(dd_bs,
               data.frame(l_TSS = dd_bs_obs$l_acc, r_TSS = dd_bs_obs$r_acc, mean_TSS = dd_bs_obs$mean_acc,
                    ngut = dd_bs_obs$propn, true_foodweb = "Observed"))


dd_bs_ADBM_obs  <- ggplot(dd_bs) +
  geom_line(aes(x = ngut, y = mean_TSS, color = true_foodweb), 
            position=position_dodge(width = 3)) +
  geom_ribbon(aes(x = ngut, ymin = l_TSS, ymax = r_TSS, fill = true_foodweb), alpha = 0.5, show.legend = FALSE,
              position=position_dodge(width = 3)) +
  geom_vline(xintercept = n_min_gut_ADBM, color = "red") +
  geom_vline(xintercept = n_min_gut_obs, color = "blue") +
  xlab("Number of predator guts") +
  ylab("True skill statistic") +
  theme_classic() +
  scale_color_manual(name = "Observed food web", values = c("red", "blue"), 
                     labels = c("Observed" = "Emprirical",
                                "from ADBM" = "ADBM's best prediction")) +
  scale_x_continuous(breaks = c(1, 250, 500, 750, 1000))
  

# ggsave(plot = dd_bs_ADBM_obs, filename = "inst/report/fig_2021_10_15/plot_TSS_vs_ngut_ADBM_obs.png")
