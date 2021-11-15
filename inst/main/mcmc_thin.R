foodweb <- "sim_Small Reef"
dirnam <- paste("results/mcmc/",foodweb,"/mN=8e+05_tol=0.3_TSS_unif_rall", sep = "")
fname <- paste(dirnam, "/", foodweb, ".Rdata", sep = "")
raw_result <- readRDS(fname)[[1]]

nlag <- lag_mcmc(raw_result$post_dists)

t_length <- dim(raw_result$post_dists)[1]
n_take <- as.integer(t_length/nlag)
ind <- nlag*(2:n_take)


result <- list(acc_ss = raw_result$acc_ss[ind], dist = raw_result$dist[ind],
               post_dists = raw_result$post_dists[ind,], total_sim = raw_result$total_sim)

fname_new <- paste(dirnam, "/", foodweb, "_thin.Rdata", sep = "")
saveRDS(result, file = fname_new)
