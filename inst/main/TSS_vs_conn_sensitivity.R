# 22.04.2021
# We investigate the dependence between TSS and connectance to choose a certain threshold

fw_name <- "Broadstone Stream size_agg_v2"

dd_org <- readRDS(paste0("results/rejection/", fw_name, "/rN=1e+05_tol=2_TSS_lower_a/", fw_name, ".Rdata"))

obs_fw <- readRDS(paste0("data/", fw_name, ".web.Rdata"))
obs_pred_mat <- obs_fw$predation.matrix
# S2 <- length(obs_fw$pred_nodes)*dim(obs_fw$predation.matrix)[2]
S2 <- dim(obs_fw$predation.matrix)[2]*dim(obs_fw$predation.matrix)[2]

obs_connectance <- sum(obs_pred_mat)/S2
l_obs_conn <- 0.975*obs_connectance
u_obs_conn <- 1.05*obs_connectance

dd <- data.frame(TSS = 1 - dd_org$dist, connectance = dd_org$acc_ss)

max_TSS <- max((dd %>%
                 filter(connectance >= l_obs_conn & connectance <= u_obs_conn))$TSS)
l_TSS <- 0.95*(max_TSS)



acc_ind <- which(dd$TSS >= l_TSS & dd$TSS <= max_TSS & dd$connectance >= l_obs_conn & dd$connectance <= u_obs_conn)

dd_fil <- dd_org
dd_fil$acc_ss <- dd_org$acc_ss[acc_ind]
dd_fil$dist <- dd_org$dist[acc_ind]
dd_fil$post_dists <- dd_org$post_dists[acc_ind, ]
nsim <- length(acc_ind)
dd_fil$total_sim <- nsim



fname <- paste0("results/rejection/", fw_name, "/rN=", nsim, "_tol=", NA, "_TSS_conn_lower_a/", fw_name, ".Rdata")
dir.create(paste0("results/rejection/", fw_name, "/rN=", nsim, "_tol=", NA, "_TSS_conn_lower_a"))

# saveRDS(object = dd_fil, file = fname)

ggplot(dd) +
  geom_point(aes(x = connectance, y = TSS)) +
  theme_bw() +
  geom_vline(xintercept = l_obs_conn, color = "red") +
  geom_vline(xintercept = u_obs_conn, color = "red") +
  geom_hline(yintercept = l_TSS, color = "red") +
  geom_hline(yintercept = max_TSS, color = "red")
