#  2022.05.16
# We estimate the number of gut content required to ensure that the full structure of the food web is constructed

library(vegan)

data(BCI)
sp1 <- specaccum(BCI)
sp2 <- specaccum(BCI, "random")
sp2

summary(sp2)

plot(sp1, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")

boxplot(sp2, col="yellow", add=TRUE, pch="+")


## Fit Lomolino model to the exact accumulation

mod1 <- fitspecaccum(sp1, "lomolino")
coef(mod1)
fitted(mod1)
plot(sp1)

## Add Lomolino model using argument 'add'
plot(mod1, add = TRUE, col=2, lwd=2)


## Fit Arrhenius models to all random accumulations

mods <- fitspecaccum(sp2, "arrh")
plot(mods, col="hotpink")
boxplot(sp2, col = "yellow", border = "blue", lty=1, cex=0.3, add= TRUE)

## Use nls() methods to the list of models
sapply(mods$models, AIC)


## Understanding the function specpool which is used to extrapolate the SAC and estimate the number of unobserved species

data(dune)
data(dune.env)
attach(dune.env)
pool <- specpool(dune, Management)

op <- par(mfrow=c(1,2))
boxplot(specnumber(dune) ~ Management, col="hotpink", border="cyan3", notch=TRUE)

boxplot(specnumber(dune)/specpool2vect(pool) ~ Management, col="hotpink", border="cyan3", notch=TRUE)

par(op)
data(BCI)

## Accumulation model
pool <- poolaccum(BCI)
summary(pool, display = "chao")
plot(pool)

## Quantitative model
estimateR(BCI[1:5,])


### Let's try with Broadstone Stream gut content data

bs_pred <- readRDS("../C2_comp_qnty/data/Broadstone Stream size_agg_v2.web.Rdata")$predation.matrix
nspecies <- dim(bs_pred)[1]
bs_gut <- readRDS("../C2_comp_qnty/data/gut_data/Broadstone Stream size_agg_v2_ind_predator.gut.Rdata")

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

specpool(link_mat)

