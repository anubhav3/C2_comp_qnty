# 03.05.2021
# We compute the food web properties where the predators' diets have been predicted only
#The proportion of basal, intermediate and top species have been computed using a different function

fw_prop_predator <- function(foodweb, other, all.web.info){

  parameters <- foodweb$post_dists
  M <- all.web.info$species.sizes
  title <- all.web.info$web.name
  N <- dim(parameters)[1]
  dim_pred_mat <- dim(all.web.info$predation.matrix)[1]
  best.web_total <- matrix(0, nrow = dim_pred_mat, ncol = dim_pred_mat)
  fw_prop <- data.frame(connectance = double(), prop_basal = double(), prop_inter = double(), prop_top = double(),
                        prop_herb = double(), mean_trop_lvl = double(), max_trop_lvl = double(),
                        mean_omn = double(), clus_coeff = double(), sd_gen = double(),
                        sd_vulner = double(), mean_max_trophic_sim = double(), mean_path_lt = double(),
                        prop.1.corr = double(), prop.0.corr = double(), ACC = double(), TSS = double(),
                        n_singleton = integer(), nest_prop = double())
  
  for(i in 1:N){
    
    best.EHL1 <- Ratio.allometric.EHL(M=M,
                                      e=other$e,
                                      a=10^parameters$a[i], ai=parameters$ai[i], aj=parameters$aj[i],
                                      n=other$n, ni=other$ni,
                                      r.a=other$r.a, r.b=10^parameters$r.b[i])
    
    best.web1 <- Get.web(best.EHL1)
    pred_nodes <- which(colSums(all.web.info$predation.matrix)>0)
    best.web1[,-pred_nodes] <- 0
    community <- mat.to.comm(best.web1, title)
    
    prop.1.corr <- round(sum(best.web1==1 & all.web.info$predation.matrix==1)/sum(all.web.info$predation.matrix), 2)
    prop.0.corr <- round(sum(best.web1==0 & all.web.info$predation.matrix==0)/
                           (dim(all.web.info$predation.matrix)[1]^2-sum(all.web.info$predation.matrix)), 2)
    
    connectance <- sum(best.web1)/(dim(best.web1)[1]^2)
    prop_species <- prop_basal_inter_top_herb(pred_mat = best.web1)
    prop_basal <- prop_species$prop_basal
    prop_inter <- prop_species$prop_inter
    prop_top <- prop_species$prop_top
    n_singleton <- prop_species$n_singleton
    prop_herb <- prop_species$prop_herb
    
    # mean_trop_lvl <- mean(ShortWeightedTrophicLevel(community))
    # max_trop_lvl <- max(LongestTrophicLevel(community))
    
    mtl <- try(mean(ShortWeightedTrophicLevel(community)))
    mean_trop_lvl <- if(is.numeric(mtl)==TRUE) mtl else NA
    maxtl <- try(max(LongestTrophicLevel(community)))
    max_trop_lvl <- if(is.numeric(maxtl)==TRUE) maxtl else NA
    
    mean_omn <- FractionOmnivorous(community)
    clus_coeff <- ClustF(best.web1, type = "directed")$GlobaltotalCC
    sd_gen <- sd(InDegree(community))
    sd_vulner <- sd(OutDegree(community))
    ACC <- 1-dist_ACC(ss_sim = best.web1[, pred_nodes], ss_real = all.web.info$predation.matrix[, pred_nodes])
    TSS <- TSS_func(ss_sim = best.web1[, pred_nodes], ss_real = all.web.info$predation.matrix[, pred_nodes])
    
    mean_max_trophic_sim <- MeanMaximumTrophicSimilarity(community)
    
    
    shortest_paths <- ShortestPaths(community)
    finite_paths <- is.finite(shortest_paths)
    
    mean_path_lt <- mean(shortest_paths[finite_paths])
    
    nest_prop <- as.numeric(nested(web = best.web1)) #the default method is binmatnest2
    
    
    fw_prop_temp <- data.frame(connectance = connectance, prop_basal = prop_basal, prop_inter = prop_inter, prop_top = prop_top,
                          prop_herb = prop_herb, mean_trop_lvl = mean_trop_lvl, max_trop_lvl = max_trop_lvl,
                          mean_omn = mean_omn, clus_coeff = clus_coeff, sd_gen = sd_gen,
                          sd_vulner = sd_vulner, mean_max_trophic_sim = mean_max_trophic_sim, mean_path_lt = mean_path_lt,
                          prop.1.corr = prop.1.corr, prop.0.corr = prop.0.corr, ACC=ACC, TSS=TSS,
                          n_singleton = n_singleton, nest_prop = nest_prop)
    
    fw_prop <- rbind(fw_prop, fw_prop_temp)
    best.web_total <- best.web_total + best.web1
  }
  
  fw_prop_list <- list(prop = fw_prop, web = best.web_total)
  
  return(fw_prop_list)
}
