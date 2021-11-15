# 18.03.2021
# Rule lbs (body sizes less than average)
# We construct the food web from the rule lbs


fw_name <- "Celtic Sea size_agg"
dir_N <- 1e5
dir_tol <- Inf #different for ind and lbs and ubs
n_gut_sample <- 10
propn <- 237 #576 for lbs and 432 for ubs #568 for lbs because that is the last element of seq(8, 576, by = 20)
n_sel <- 1 #number of diets of predator selected with maximum TSS
n <- length(propn)
l_acc <- numeric(n)
r_acc <- numeric(n)
mean_acc <- numeric(n)

desc_main <- "TSS_gut_ind"
rule <- "lbs_predator"

post_dists <- data.frame(a = double(), ai = double(), aj = double(), r.b = double())

for(i in propn){
  desc <- paste(desc_main, '_l', i, sep = '')
  dir_main <- paste("results/rejection/", fw_name, "/rule_", rule ,'/N=', dir_N, '_tol=', dir_tol, '_n_diet = ', i,
                    sep = "")
  TSS_fw <- c()
  for(n_sample  in 1:n_gut_sample){
    dirnam <- paste(c(dir_main, '/rn_sample=', n_sample,'_N=', dir_N, '_tol=', dir_tol,"_", desc), collapse = '')
    fname <- paste(dirnam, "/", fw_name, ".Rdata", sep = "")
    prop_acc <- readRDS(fname)
  
    gut_TSS <- 1 - prop_acc$dist
    gut_TSS_indexes <- order(gut_TSS, decreasing = TRUE)[1:n_sel]
    TSS_fw_temp <- prop_acc$TSS_fw[gut_TSS_indexes]
    
    post_dists <- rbind(post_dists, prop_acc$post_dists[gut_TSS_indexes,])
    TSS_fw <- c(TSS_fw_temp, TSS_fw)
  }
}


output <- list(post_dists = post_dists)
fname_output <- paste0("results/rejection/", fw_name, "/rule_lbs_predator/lbs_par_properties/post_par.Rdata")
# saveRDS(object = output, file = fname_output)

## Computing food web properties
# output <- readRDS(file = fname_output)
fw_data <- readRDS(file = paste0("data/", fw_name, ".web.Rdata"))
model_core_par <- ADBM_core_par(fw_data)
properties <- fw_prop_predator(foodweb = output, other = model_core_par, all.web.info = fw_data)
# fname_prop <- paste0("results/rejection/Broadstone Stream size_agg_v2/rule_lbs_predator/lbs_par_properties/properties.Rdata")
# saveRDS(properties, file = fname_prop)


predicted_foodweb <- output

# fname_prop <- paste(dirnam, "/", fw_name,"_prop.Rdata", sep = "")
prop_web <- properties
dirnam_lbs <- paste0("results/rejection/",fw_name,"/rule_lbs_predator/lbs_par_properties")
## Plotting the real and predicted food web matrix alongwith some food web properties

gut_data_main <- readRDS(file = paste0("data/gut_data/", fw_name, "_lbs_predator.gut.Rdata"))
species_ind <- readRDS(paste0("results/rejection/", fw_name, "/rule_lbs_predator/diet/", fw_name, "_",propn, ".gut_index.Rdata"))
species_ind <- species_ind[1,]
lbs_species <- sort(unique(gut_data_main[2,][species_ind]))
lbs_pred_mat <- matrix(0, nrow = 48, ncol = 48)
lbs_pred_mat[, lbs_species] <- fw_data$predation.matrix[, lbs_species]

plot_foodweb_prop(real_foodweb = fw_data, predicted_foodweb = predicted_foodweb,
                  dirnam = dirnam_lbs, model_core_par = model_core_par, model_prior_par = model_prior_par,
                  model = model, desc = desc, prior_dist_x = prior_dist_x, web.to.analyse = fw_name, prop_web = prop_web,
                  true_val_plot = F, gut_data = lbs_pred_mat)


lbs_nested <- prop_web$prop$nest_prop



real_fw <- readRDS("data/Broadstone Stream size_agg_v2.web.Rdata")
real_prop_c <- real_prop_v2(all.web.info = real_fw)
real_nest <- real_prop_c$nest_prop
