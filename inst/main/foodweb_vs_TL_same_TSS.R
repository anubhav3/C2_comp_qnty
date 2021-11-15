# 10.05.2021
# We look at cases of predicted food web with different TSS but same trophic levels for the top consumer

library(dplyr)

pred_par <- readRDS("results/rejection/sim_Broadstone Stream size_agg_v2/rule_s_r_2_predator/N=1e+05_tol=Inf_n_pred = 29/rn_gut=1_N=1e+05_tol=Inf_TSS_sir_gut_s_r_2_l29_prop_1/sim_Broadstone Stream size_agg_v2.Rdata")

fil_ind <- which(pred_par$dist_tl > 6.06042e+13)

fil_par <- pred_par$post_dists[fil_ind[1],]

sim_par <- data.frame(a = 10^fil_par$a,
                    ai = fil_par$ai,
                    aj = fil_par$aj,
                    r.b = 10^fil_par$r.b)

fw_pred <- sim_foodweb(sim_par = sim_par, fw_name = "Broadstone Stream size_agg_v2")

real_fw <- readRDS("data/sim_Broadstone Stream size_agg_v2.web.Rdata")
pred_nodes <- which(colSums(real_fw$predation.matrix) > 0)

real_tl <- trophic_position(web = real_fw$predation.matrix, title = "sim_Broadstone Stream size_agg_v2")

pred_mat <- fw_pred$predation.matrix
pred_mat[,-pred_nodes] <- 0

trophic_position(web = pred_mat, title = "sim_Broadstone Stream size_agg_v2")

pred_comm <- mat.to.comm(pred.mat = pred_mat, fw_title = "sim_Broadstone Stream size_agg_v2")



# fil_ind <- which(pred_par$dist_tl >= 2.11 & pred_par$dist_tl <= 2.12)
# fil_ind <- fil_ind[c(1,3,5,27)]

fil_ind <- which(pred_par$dist_tl >= 3.11 & pred_par$dist_tl <= 3.12)
fil_ind <- fil_ind[c(10,1,4,3)]

pred_par$dist_tl[fil_ind]
pred_par$TSS_fw[fil_ind]

fil_par <- pred_par$post_dists[fil_ind,]

sim_par <- data.frame(a = 10^fil_par$a,
                      ai = fil_par$ai,
                      aj = fil_par$aj,
                      r.b = 10^fil_par$r.b)

fw_pred_1 <- sim_foodweb(sim_par = sim_par[1,], fw_name = "Broadstone Stream size_agg_v2")
fw_pred_2 <- sim_foodweb(sim_par = sim_par[2,], fw_name = "Broadstone Stream size_agg_v2")
fw_pred_3 <- sim_foodweb(sim_par = sim_par[3,], fw_name = "Broadstone Stream size_agg_v2")
fw_pred_4 <- sim_foodweb(sim_par = sim_par[4,], fw_name = "Broadstone Stream size_agg_v2")


pred_mat_1 <- fw_pred_1$predation.matrix
pred_mat_1[,-pred_nodes] <- 0
pred_mat_2 <- fw_pred_2$predation.matrix
pred_mat_2[,-pred_nodes] <- 0
pred_mat_3 <- fw_pred_3$predation.matrix
pred_mat_3[,-pred_nodes] <- 0
pred_mat_4 <- fw_pred_4$predation.matrix
pred_mat_4[,-pred_nodes] <- 0

pred_comm_1 <- mat.to.comm(pred.mat = pred_mat_1, fw_title = "sim_Broadstone Stream size_agg_v2")
pred_comm_2 <- mat.to.comm(pred.mat = pred_mat_2, fw_title = "sim_Broadstone Stream size_agg_v2")
pred_comm_3 <- mat.to.comm(pred.mat = pred_mat_3, fw_title = "sim_Broadstone Stream size_agg_v2")
pred_comm_4 <- mat.to.comm(pred.mat = pred_mat_4, fw_title = "sim_Broadstone Stream size_agg_v2")


PlotWebByLevel(community = pred_comm_1)
PlotWebByLevel(community = pred_comm_2)
PlotWebByLevel(community = pred_comm_3)
PlotWebByLevel(community = pred_comm_4)

Plot.matrix(pred_mat_1)
Plot.matrix(pred_mat_2)
Plot.matrix(pred_mat_3)
Plot.matrix(pred_mat_4)

pred_mat_2_per <- pred_mat_2
pred_mat_2_per[,c(14,29)] <- pred_mat_2_per[,c(29,14)]

trophic_position(web = pred_mat_2_per, title = "sim_Broadstone Stream size_agg_v2")
