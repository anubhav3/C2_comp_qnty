library(coda)

foodweb <- "Benguela Pelagic"
dirnam <- paste("results/mcmc/",foodweb,"/mN=258000_tol=0.7_TSS_unif_rall", sep = "")
fname <- paste(dirnam, "/", foodweb, ".Rdata", sep = "")
raw_result <- readRDS(fname)


l1 <- raw_result[[1]]$post_dists
l2 <- raw_result[[2]]$post_dists
l3 <- raw_result[[3]]$post_dists


l1 <- as.mcmc(l1)
l2 <- as.mcmc(l2)
l3 <- as.mcmc(l3)

l_mcmc <- mcmc.list(l1,l2,l3)
gelman.diag(l_mcmc)
gelman.plot(l_mcmc)
