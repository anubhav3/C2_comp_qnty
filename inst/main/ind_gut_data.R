# 07.10.2021
# We compile individual gut data 

# 14.05.2021
# We compile individual gut data of predator individuals from the Celtic Sea


library(dplyr)
library(ggplot2)


n_node <- 48

pred_ind <- c()
max_size <- numeric(1)
min_size <- numeric(1)

bs_data <- read_delim("data/Predator_and_prey_body_sizes_in_marine_food_webs_vsn4.txt", 
                      "\t", escape_double = FALSE, trim_ws = TRUE)
bs_data <- bs_data %>%
  filter(!Prey %in% c("fish larvae", "fish unidentified", "flatfish") ) %>%
  filter(!is.na(Prey) & !is.na(Predator)) %>%
  filter(`Geographic location` == "Europe, Celtic Sea ecosystem")

min_size <- log10(min(bs_data$`SI prey mass`, bs_data$`SI predator mass`))
max_size <- log10(max(bs_data$`SI prey mass`, bs_data$`SI predator mass`))

h <- (max_size-min_size)/n_node

mass_to_ind <- function(mass){
  index <- ceiling((mass-min_size)/h)
  if(index == 0) {index = 1}
  return(index)
}

uniq_ind <- unique(bs_data$`Individual ID`)

n_pred_ind <- length(uniq_ind)

#The first row of the diet_mat corresponds to the predator identity denoted by a unique number and second row consists of bin number
diet_mat <- matrix(data = 0, nrow = n_node+2, ncol = n_pred_ind,)
diet_mat[1,] <- c(1:n_pred_ind)
i <- 1 


for(row_no in 1:nrow(bs_data)){
  
  mt <- bs_data[row_no,]
  
  pseudo_pred_ind <- which(uniq_ind == bs_data$`Individual ID`[row_no])
  
  prey_mass <- log10(mt$`SI prey mass`)
  prey_name <- mass_to_ind(prey_mass)
  
  pred_mass <- log10(mt$`SI predator mass`)
  pred_name <- mass_to_ind(pred_mass)
  
  pred_ind <- mt$`Individual ID`
  pseudo_pred_ind <- which(uniq_ind == pred_ind) 
  
  
  
  diet_mat[prey_name+2, pseudo_pred_ind] <- 1
  diet_mat[1, pseudo_pred_ind] <- log10(mt$`SI predator mass`)
  diet_mat[2, pseudo_pred_ind] <- pred_name
  
}

fname <- "../C2_comp_qnty/data/gut_data/Celtic Sea size_agg_ind_predator.gut.Rdata"
# saveRDS(diet_mat, fname)
