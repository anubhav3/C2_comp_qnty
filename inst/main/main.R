#Date: 10 June 2020

library(R.utils)

## Relative path from the project directory
sourceDirectory("R", modifiedOnly=FALSE)


sim_fw <- readRDS("data/sim_Small Reef.web.Rdata")

pred_mat <- sim_fw$predation.matrix
M <- sim_fw$species.sizes


# gut_data <- gut_matrix(pred_mat = pred_mat, M = M, pbly_f = pbly_r2)
gut_data_e1r1 <- gut_matrix_e1r1(pred_mat = pred_mat, M = M, n = model_core_par$n, ni = model_core_par$ni, a = sim_fw$a, ai = sim_fw$ai, aj = sim_fw$aj)
gut_data_e1 <- gut_matrix_e1(pred_mat = pred_mat, M = M, n = model_core_par$n, ni = model_core_par$ni, a = sim_fw$a, ai = sim_fw$ai, aj = sim_fw$aj)
gut_data_r1 <- gut_matrix(pred_mat = pred_mat, M = M, pbly_f = pbly_r1)
fname_gut <- "data/gut_data/sim_Small Reef_e1.gut.Rdata"
saveRDS(gut_data, file = fname_gut)




sr <- readRDS("results/rejection/sim_Small Reef/rN=1000_tol=0.2_TSS_gut/sim_Small Reef_prop.Rdata")
sr_web <- sr$web
sr_gut <- readRDS("data/gut_data/sim_Small Reef.gut.Rdata")
sr_real_web <- readRDS("data/sim_Small Reef.web.Rdata")$predation.matrix

Plot.matrix_uncertainty_real_gut(web = sr_web, real_web = sr_real_web, gut_data = sr_gut, nsim = 1000)



sr_all <- readRDS("results/rejection/sim_Small Reef/rN=1e+05_tol=2_TSS_gut/sim_Small Reef.Rdata")
range(sr_all$dist)
hist(sr_all$acc_ss[sr_all$dist>1.2 & sr_all$dist<1.3])


###################################################################
##




# Calculating trophic levels from the predation matrix and saving in a file

trop_levels <- trophic_position(web = sim_fw$predation.matrix, title = sim_fw$web.name)
d_TEF <- 1
d_ref <- 1
sir_data <- stable_isotopes_ratio(trophic_level = trop_levels, d_TEF = d_TEF, d_ref = d_ref)
fname_sir <- "data/sir_data/sim_Small Reef_r1.sir.Rdata"
saveRDS(sir_data, file = fname_sir)



