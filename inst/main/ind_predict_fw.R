# 18.03.2021
# Rule ind (all body sizes)
# We construct the food web from the rule ind


fw_name <- "Broadstone Stream size_agg_v2"
dir_N <- 1e5
dir_tol <- Inf #different for ind and lbs and ubs
n_gut_sample <- 100 
propn <- 1008
n_sel <- 1 #number of diets of predator selected with maximum TSS
n <- length(propn)
l_acc <- numeric(n)
r_acc <- numeric(n)
mean_acc <- numeric(n)

desc_main <- "TSS_gut_ind"
rule <- "ind_predator"

post_dists <- data.frame(a = double(), ai = double(), aj = double(), r.b = double())

for(i in propn){
  desc <- paste(desc_main, '_l', i, sep = '')
  dir_main <- paste("../../../PhD_local/C2_files/", fw_name, "/rule_", rule ,'/N=', dir_N, '_tol=', dir_tol, '_n_diet = ', i,
                    sep = "")
  
  for(n_sample  in 1:n_gut_sample){
    dirnam <- paste(c(dir_main, '/rn_sample=', n_sample,'_N=', dir_N, '_tol=', dir_tol,"_", desc), collapse = '')
    fname <- paste(dirnam, "/", fw_name, ".Rdata", sep = "")
    prop_acc <- readRDS(fname)
    
    gut_TSS <- 1 - prop_acc$dist
    gut_TSS_indexes <- order(gut_TSS, decreasing = TRUE)[1:n_sel]
    
    post_dists <- rbind(post_dists, prop_acc$post_dists[gut_TSS_indexes,])
  }
}


output <- list(post_dists = post_dists)
fname_output <- paste0("results/rejection/Broadstone Stream size_agg_v2/rule_ind/ind_par_properties/post_par.Rdata")
# saveRDS(object = output, file = fname_output)

## Computing food web properties
# output <- readRDS(file = fname_output)
properties <- fw_prop_v2(foodweb = output, other = model_core_par, all.web.info = fw_data)
# fname_prop <- paste0("results/rejection/Broadstone Stream size_agg_v2/rule_lbs/lbs_par_properties/properties.Rdata")
# saveRDS(properties, file = fname_prop)


predicted_foodweb <- output

# fname_prop <- paste(dirnam, "/", fw_name,"_prop.Rdata", sep = "")
prop_web <- properties
dirnam_ind <- paste0("results/rejection/Broadstone Stream size_agg_v2/rule_ind/ind_par_properties")
## Plotting the real and predicted food web matrix alongwith some food web properties

gut_data_main <- readRDS(file = "data/gut_data/Broadstone Stream size_agg_v2_ind.gut.Rdata")
species_ind <- readRDS("results/rejection/Broadstone Stream size_agg_v2/rule_ind/diet/Broadstone Stream size_agg_v2_1008.gut_index.Rdata")
species_ind <- species_ind[1,]
ind_species <- sort(unique(gut_data_main[2,][species_ind]))
ind_pred_mat <- matrix(0, nrow = 29, ncol = 29)
ind_pred_mat[, ind_species] <- fw_data$predation.matrix[, ind_species]

# plot_foodweb_prop(real_foodweb = fw_data, predicted_foodweb = predicted_foodweb, 
#                   dirnam = dirnam_lbs, model_core_par = model_core_par, model_prior_par = model_prior_par,
#                   model = model, desc = desc, prior_dist_x = prior_dist_x, web.to.analyse = fw_name, prop_web = prop_web, 
#                   true_val_plot = F, gut_data = ind_pred_mat)


ind_nested <- prop_web$prop$nest_prop

hist(lbs_nested)
hist(ubs_nested)
hist(ind_nested)

mean(lbs_nested)
mean(ubs_nested)
mean(ind_nested)
