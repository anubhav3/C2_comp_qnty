# We draw out posterior parameters from the result of simulation corresponding to rule_s3

data_all <- readRDS("../../../PhD_local/C2_files/sim_Small Reef/rule_s3/N=1e+05_tol=Inf_n_pred = 10/rn_sir=1_N=1e+05_tol=Inf_TSS_sir_s3_l10/sim_Small Reef.Rdata")

n_sim <- data_all$total_sim
n_sel <- n_sim * 0.001
dist_all <- data_all$dist  

less_dist_indexes <- order(dist_all, decreasing = FALSE)[1:n_sel]
# less_dist <- dist_all[less_dist_indexes]
  
# TSS_sel <- 1- data_all$acc_ss[less_dist_indexes]

post_par <- data_all$post_dists[less_dist_indexes,]

# saveRDS(post_par, file = "data/prior_par/sim_Small Reef_s_r_1.prior.Rdata")

