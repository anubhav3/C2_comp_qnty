# 11.08.2021


## Tadnoll Brook food web
tb_gut <- readRDS("data/gut_data/Tadnoll Brook size_agg_ind_predator.gut.Rdata")

tb_688_1 <- readRDS("../../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Tadnoll Brook size_agg/rule_ind_predator/N=1e+05_tol=2_n_diet = 688/rn_sample=1_N=1e+05_tol=2_TSS_gut_ind_l688/Tadnoll Brook size_agg.Rdata")

tb_688_2 <- readRDS("../../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Tadnoll Brook size_agg/rule_ind_predator/N=1e+05_tol=2_n_diet = 688/rn_sample=2_N=1e+05_tol=2_TSS_gut_ind_l688/Tadnoll Brook size_agg.Rdata")

tb_688_3 <- readRDS("../../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Tadnoll Brook size_agg/rule_ind_predator/N=1e+05_tol=2_n_diet = 688/rn_sample=3_N=1e+05_tol=2_TSS_gut_ind_l688/Tadnoll Brook size_agg.Rdata")

tb_688_74 <- readRDS("../../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Tadnoll Brook size_agg/rule_ind_predator/N=1e+05_tol=2_n_diet = 688/rn_sample=74_N=1e+05_tol=2_TSS_gut_ind_l688/Tadnoll Brook size_agg.Rdata")


dd_688_1 <- data.frame(TSS_fw = tb_688_1$TSS_fw, TSS_gut = 1 - tb_688_1$dist_gut, nsample = "1")

dd_688_2 <- data.frame(TSS_fw = tb_688_2$TSS_fw, TSS_gut = 1 - tb_688_2$dist_gut, nsample = "2")

dd_688_3 <- data.frame(TSS_fw = tb_688_3$TSS_fw, TSS_gut = 1 - tb_688_3$dist_gut, nsample = "2")

dd_688_74 <- data.frame(TSS_fw = tb_688_74$TSS_fw, TSS_gut = 1 - tb_688_74$dist_gut, nsample = "74")


dd_688 <- rbind(dd_688_1, dd_688_74)

ggplot(dd_688) +
  geom_point(aes(x = TSS_gut, y = TSS_fw, color = nsample))

             

## Celtic Sea food web
cs_gut <- readRDS("data/gut_data/Celtic Sea size_agg_ind_predator.gut.Rdata")

cs_491_1 <- readRDS("../../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Celtic Sea size_agg/rule_ind_predator/N=1e+05_tol=2_n_diet = 491/rn_sample=1_N=1e+05_tol=2_TSS_gut_ind_l491/Celtic Sea size_agg.Rdata")

cs_491_2 <- readRDS("../../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Celtic Sea size_agg/rule_ind_predator/N=1e+05_tol=2_n_diet = 491/rn_sample=2_N=1e+05_tol=2_TSS_gut_ind_l491/Celtic Sea size_agg.Rdata")

cs_491_3 <- readRDS("../../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Celtic Sea size_agg/rule_ind_predator/N=1e+05_tol=2_n_diet = 491/rn_sample=3_N=1e+05_tol=2_TSS_gut_ind_l491/Celtic Sea size_agg.Rdata")

cs_491_4 <- readRDS("../../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Celtic Sea size_agg/rule_ind_predator/N=1e+05_tol=2_n_diet = 491/rn_sample=4_N=1e+05_tol=2_TSS_gut_ind_l491/Celtic Sea size_agg.Rdata")


dd_491_1 <- data.frame(TSS_fw = cs_491_1$TSS_fw, TSS_gut = 1 - cs_491_1$dist_gut, nsample = "1")

dd_491_2 <- data.frame(TSS_fw = cs_491_2$TSS_fw, TSS_gut = 1 - cs_491_2$dist_gut, nsample = "2")

dd_491_3 <- data.frame(TSS_fw = cs_491_3$TSS_fw, TSS_gut = 1 - cs_491_3$dist_gut, nsample = "3")

dd_491_4 <- data.frame(TSS_fw = cs_491_4$TSS_fw, TSS_gut = 1 - cs_491_4$dist_gut, nsample = "4")

dd_491 <- rbind(dd_491_1, dd_491_2, dd_491_3, dd_491_4)

ggplot(dd_491) +
  geom_point(aes(x = TSS_gut, y = TSS_fw, color = nsample))

