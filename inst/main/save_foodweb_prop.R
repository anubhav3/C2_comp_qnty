foodweb <- "sim_Small Reef"
dirnam <- paste("results/mcmc/",foodweb,"/mN=8e+05_tol=0.3_TSS_unif_rall", sep = "")
fname <- paste(dirnam, "/", foodweb, "_thin.Rdata", sep = "")

foodweb_data <- load(paste("data/", foodweb, ".web.Rdata", sep=""))
all.web.info <- get(foodweb_data)


predicted_foodweb <- readRDS(fname)
model_core_par = ADBM_core_par(all.web.info)
properties <- fw_prop(predicted_foodweb, model_core_par, all.web.info = all.web.info)


fname_new <- paste(dirnam, "/", foodweb, "_prop.Rdata", sep = "")
saveRDS(properties, file = fname_new)
