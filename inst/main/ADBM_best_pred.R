## 11.08.2021

## Plot the graphs for a case when the ADBM’s best prediction is the true one 


## Broadstone Stream size_agg
# 36 sample correspond to the best prediction


bs_1008_36 <- readRDS("../../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Broadstone Stream size_agg_v2/rule_ind_predator/N=1e+05_tol=2_n_diet = 1008/rn_sample=36_N=1e+05_tol=2_TSS_gut_ind_l1008/Broadstone Stream size_agg_v2.Rdata")

ind_max_TSS <- which.max(bs_1008_36$TSS_fw)

par_max_TSS <- bs_1008_36$post_dists[ind_max_TSS,]



