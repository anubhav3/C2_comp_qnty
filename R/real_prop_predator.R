# 04.05.2021
# Computes food web properties for a real food web

real_prop_predator <- function(all.web.info){
  
  title <- all.web.info$web.name
  community <- mat.to.comm(all.web.info$predation.matrix, title)
  connectance <- sum(all.web.info$predation.matrix)/(dim(all.web.info$predation.matrix)[1]^2)
  prop_species <- prop_basal_inter_top_herb(pred_mat = all.web.info$predation.matrix)
  prop_basal <- prop_species$prop_basal
  prop_inter <- prop_species$prop_inter
  prop_top <- prop_species$prop_top
  n_singleton <- prop_species$n_singleton
  prop_herb <- prop_species$prop_herb
  
  mtl <- try(mean(ShortWeightedTrophicLevel(community)))
  mean_trop_lvl <- if(is.numeric(mtl)==TRUE) mtl else NA

  maxtl <- try(max(LongestTrophicLevel(community)))
  max_trop_lvl <- if(is.numeric(maxtl)==TRUE) maxtl else NA

  mean_omn <- FractionOmnivorous(community)
  clus_coeff <- ClustF(all.web.info$predation.matrix, type = "directed")$GlobaltotalCC
  
  sd_gen <- sd(InDegree(community))
  sd_vulner <- sd(OutDegree(community))
  
  mean_max_trophic_sim <- MeanMaximumTrophicSimilarity(community)
  
  shortest_paths <- ShortestPaths(community)
  finite_paths <- is.finite(shortest_paths)
  mean_path_lt <- mean(shortest_paths[finite_paths])
  
  nest_prop <- as.numeric(nested(all.web.info$predation.matrix))
  
  real_prop_f <- data.frame(connectance = connectance, prop_basal = prop_basal, prop_inter = prop_inter, prop_top = prop_top,
                            prop_herb = prop_herb, mean_trop_lvl = mean_trop_lvl, max_trop_lvl = max_trop_lvl,
                            mean_omn = mean_omn, clus_coeff = clus_coeff, sd_gen = sd_gen,
                            sd_vulner = sd_vulner, mean_max_trophic_sim = mean_max_trophic_sim, mean_path_lt = mean_path_lt,
                            n_singleton = n_singleton, nest_prop = nest_prop)
  
  return(real_prop_f)
}