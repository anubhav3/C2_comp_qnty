# 31.05.2021
# We compute the structural properties of food webs constructed using partial gut content data only without using ADBM


# fw_name <- "Broadstone Stream size_agg_v2"
fw_name <- "Celtic Sea size_agg"
fname_data <- paste("data/", fw_name, ".web.Rdata", sep="")
fw_data <- readRDS(fname_data)
nspecies <- dim(fw_data$predation.matrix)[1]
complete_pred_mat <- fw_data$predation.matrix
pred_nodes <- which(colSums(complete_pred_mat)>0)

rule <- "ind_predator"
fname_gut <- paste("data/gut_data/", fw_name, "_", rule, ".gut.Rdata", sep="")
gut_data_main <- readRDS(file = fname_gut)


propn <- c(11, 31, 51, 71, 91, 111, 151, 171, 231, 331, 431, 491)

n_gut_sample <- 100

for(ngut in propn){
  fname_sp_ind <- paste("../../../PhD_local/C2_files/", fw_name, "/rule_", rule, "/diet/", fw_name ,"_",ngut,".gut_index.Rdata", sep = "")
  species_ind <- readRDS(fname_sp_ind)
  TSS_array <- c()
  calc_prop <- c()
  for(n_sample in 1:n_gut_sample){
    
    indexes <- species_ind[n_sample,]
    gut_data <- matrix(0, nrow = nspecies, ncol = nspecies)
    
    partial_diet <- gut_data_main[,indexes]
    pred_ind <- partial_diet[2,]
    uniq_ind <- sort(unique(partial_diet[2,]))
    
    for(ind in uniq_ind){
      local_ind <- which(pred_ind == ind)
      if(length(local_ind) > 1)
      {
        diet_temp <- rowSums(partial_diet[3:(nspecies+2),local_ind])
        diet_temp[which(diet_temp > 1)] = 1
        gut_data[,ind] <-  diet_temp
      }
      else
      {
        gut_data[,ind] <- partial_diet[3:(nspecies+2),local_ind]
      }
      
      
    }
    all.web.info <- list(web.name = fw_name, predation.matrix = gut_data)
    calc_prop_temp <- real_prop_predator(all.web.info = all.web.info)
    calc_prop <- rbind(calc_prop, calc_prop_temp)
  }
  
  
  fw_dir <- paste0("../../../PhD_local/C2_files/", fw_name, "/rule_data/", "n_diet=", ngut)
  dir.create(path = fw_dir)
  
  fname_prop <- paste0(fw_dir,"/", fw_name, "_prop.Rdata")
  saveRDS(object = calc_prop, file = fname_prop)
  
  print(ngut)
}

