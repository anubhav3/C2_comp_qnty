mcmc_old <- readRDS("results/mcmc/Benguela Pelagic/mN=258000_tol=0.7_TSS_unif_rall/Benguela Pelagic.Rdata")
mcmc_new <- readRDS("results/mcmc/Benguela Pelagic/mN=258000_tol=0.7_TSS_unif_rall_contd/Benguela Pelagic.Rdata")


mcmc_temp <- mcmc_old

i <- 1
post_dists <- rbind(mcmc_old[[i]]$post_dists, mcmc_new[[i]]$post_dists)
dist <- c(mcmc_old[[i]]$dist, mcmc_new[[i]]$dist)
acc_ss <- c(mcmc_old[[i]]$acc_ss, mcmc_new[[i]]$acc_ss)
total_sim <- mcmc_old[[i]]$total_sim + mcmc_new[[i]]$total_sim

mcmc_temp_1 <- list(post_dists = post_dists, dist = dist,
                  acc_ss  = acc_ss, total_sim = total_sim)

i <- 2
post_dists <- rbind(mcmc_old[[i]]$post_dists, mcmc_new[[i]]$post_dists)
dist <- c(mcmc_old[[i]]$dist, mcmc_new[[i]]$dist)
acc_ss <- c(mcmc_old[[i]]$acc_ss, mcmc_new[[i]]$acc_ss)
total_sim <- mcmc_old[[i]]$total_sim + mcmc_new[[i]]$total_sim

mcmc_temp_2 <- list(post_dists = post_dists, dist = dist,
                    acc_ss  = acc_ss, total_sim = total_sim)

i <- 3
post_dists <- rbind(mcmc_old[[i]]$post_dists, mcmc_new[[i]]$post_dists)
dist <- c(mcmc_old[[i]]$dist, mcmc_new[[i]]$dist)
acc_ss <- c(mcmc_old[[i]]$acc_ss, mcmc_new[[i]]$acc_ss)
total_sim <- mcmc_old[[i]]$total_sim + mcmc_new[[i]]$total_sim

mcmc_temp_3 <- list(post_dists = post_dists, dist = dist,
                    acc_ss  = acc_ss, total_sim = total_sim)



mcmc_final <- list(mcmc_temp_1, mcmc_temp_2, mcmc_temp_3)
fw_name <- "Benguela Pelagic"
dirnam <- "results/mcmc/Benguela Pelagic/mN=516000_tol=0.7_TSS_unif_rall"
dir.create(dirnam)

fname <- paste(dirnam, "/", fw_name,".Rdata", sep = "")
saveRDS(mcmc_final, file = fname)
