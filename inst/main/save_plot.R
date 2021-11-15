foodweb <- "Benguela Pelagic"
dirnam <- paste("results/rejection/",foodweb,"/rN=1000_tol=0.7_TSS_unif_rall", sep = "")


# foodweb <- "Grasslands"
# dirnam <- paste("results/rejection/",foodweb,"/rN=1000_tol=1_TSS_unif_rall", sep = "")
fname <- paste(dirnam, "/", foodweb, "_prop.Rdata", sep = "")


foodweb_data <- readRDS(paste("data_new/", foodweb, ".web.Rdata", sep=""))


fname_pred_fw <- paste(dirnam, "/", foodweb, ".Rdata", sep = "")
predicted_foodweb <- readRDS(file = fname_pred_fw)

fname_prop <- paste(dirnam, "/", foodweb, "_prop.Rdata", sep = "")
prop_web <- readRDS(file = fname_prop)

model_core_par = ADBM_core_par(foodweb_data)
model_prior_par = ADBM_prior_par(foodweb_data)
model = ratio.power_exp
prior_dist_x = prior_unif_x
desc = "TSS_unif_rall"

plot_foodweb_prop(real_foodweb = foodweb_data, predicted_foodweb = predicted_foodweb,
                  dirnam = dirnam, model_core_par = model_core_par, 
                  model_prior_par = model_prior_par,
                  model = model, desc = desc, prior_dist_x = prior_dist_x, web.to.analyse = foodweb, prop_web = prop_web,
                  true_val_plot = F)
