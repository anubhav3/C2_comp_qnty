#Computes proportion of basal species in a food web with using the predation matrix (where, column represents consumer and row represents resources)

prop_basal_inter_top <- function(pred_mat){
  nspecies <- dim(pred_mat)[1]
  var_colsum <- colSums(pred_mat)
  var_rowsum <- rowSums(pred_mat)
  
  zero_colsum <- which(var_colsum == 0)
  zero_rowsum <- which(var_rowsum == 0)
  
  singleton_species <- intersect(zero_colsum, zero_rowsum) #one which has no links at all including itself
  basal_species <- setdiff(zero_colsum, zero_rowsum)
  top_species <- setdiff(zero_rowsum, zero_colsum)
  
  n_basal <- length(basal_species)
  n_top <- length(top_species)
  n_inter <- nspecies - n_basal - n_top
  n_singleton <- length(singleton_species)
    
  prop_basal <- n_basal/nspecies
  prop_top <- n_top/nspecies
  prop_inter <- n_inter/nspecies
  
  return(list(prop_basal = prop_basal, prop_inter = prop_inter, 
              prop_top = prop_top, n_singleton = n_singleton))
}