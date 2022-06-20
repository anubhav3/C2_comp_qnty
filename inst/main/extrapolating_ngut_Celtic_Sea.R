### Let's try with Celtic Sea gut content data

bs_pred <- readRDS("../C2_comp_qnty/data/Trancura size_agg.web.Rdata")$predation.matrix
nspecies <- dim(bs_pred)[1]
bs_gut <- readRDS("../C2_comp_qnty/data/gut_data/Trancura size_agg_ind_predator.gut.Rdata")

ngut <- dim(bs_gut)[2]
nlinks <- sum(bs_pred)

link_mat <- matrix(data = 0, nrow = ngut, ncol = nlinks,  dimnames = list(1:ngut, 1:nlinks))

### Renaming the column names of the link_mat matrix
k <- 1
for(pred_i in 1:nspecies){
  for(prey_j in 1:nspecies){
    if(bs_pred[prey_j, pred_i] == 1){
      colnames(link_mat)[k] <- paste0("P", pred_i, "p", prey_j)
      print(k)
      k <- k + 1
    }
  }
}


#### Filling up the values in the link_mat matrix ####

for(gut_index in 1:ngut){
  for(prey_j in 1:nspecies){
    gut_i_prey_j <- bs_gut[prey_j + 2, gut_index]
    if(gut_i_prey_j == 1){
      pred_index <-  bs_gut[, gut_index][2]
      prey_index <- prey_j
      gut_index <- gut_index
      col_name_ind <- paste0("P", pred_index, "p", prey_index)
      link_mat[,col_name_ind][gut_index] <- 1
    }
  }
}


link_acc_curve <- specaccum(link_mat)
plot(link_acc_curve)

pred <- poolaccum(link_mat)
plot(pred, display = "chao")
summary(pred, display = "chao")

specpool(link_mat)
estimateR(link_mat)
specpool2vect(link_mat)


sp1 <- specaccum(link_mat)
plot(sp1, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")

mod1 <- fitspecaccum(sp1, "lomolino")
plot(mod1, add = TRUE, col=2, lwd=2)


estaccumR(link_mat, permutations = 100, parallel = getOption("mc.cores"))


sp <- specpool(link_mat)
estimateR(link_mat)
