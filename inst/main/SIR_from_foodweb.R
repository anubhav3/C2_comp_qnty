# 26.03.2021
# We simulate SIR data from a simulated food web for rule s5


fw_name <- "sim_Benguela Pelagic"
fw <- readRDS("data/sim_Benguela Pelagic.web.Rdata")

pred_mat <- fw$predation.matrix

# Removing isolated nodes
fw_community <- mat.to.comm(pred.mat = pred_mat, fw_title = fw_name)

# Check if there are any isolated nodes, if yes remove those
isolated_nodes <- IsolatedNodes(community = fw_community)


tp <- trophic_position(web = pred_mat, title = fw_name)



set.seed(1)
sir <- sir_from_Tl(TL = tp, trophic_frac = 3.4, sir_base_mean = 8, sir_base_error = 1)
TL_pred <- TL_from_sir(sir = sir, trophic_frac = 3.4, sir_base = 8)
for(i in 2:1000){
  sir <- sir_from_Tl(TL = tp, trophic_frac = 3.4, sir_base_mean = 8, sir_base_error = 1)
  Trophic_levels <- TL_from_sir(sir = sir, trophic_frac = 3.4, sir_base = 8)
  TL_pred <- rbind(TL_pred, Trophic_levels)
  rownames(TL_pred)[1] <- "Trophic_levels"
}

rule <- "s5"
fname <- paste0("data/sir_data/", fw_name, "_", rule, ".sir.Rdata")
# saveRDS(object = TL_pred, file = fname)






fw_name <- "sim_Broadstone Stream size_agg_v2"
fw <- readRDS("data/sim_Broadstone Stream size_agg_v2.web.Rdata")

pred_mat <- fw$predation.matrix

# Removing isolated nodes
fw_community <- mat.to.comm(pred.mat = pred_mat, fw_title = fw_name)

# Check if there are any isolated nodes, if yes remove those
isolated_nodes <- IsolatedNodes(community = fw_community)


tp <- trophic_position(web = pred_mat, title = fw_name)
# saveRDS(object = tp, file = "data/sir_data/sim_Broadstone Stream size_agg_v2_s_r_2_predator.sir.Rdata")
