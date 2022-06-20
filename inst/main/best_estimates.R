# 07.09.2021

# We consider the best estimates of predicted food web using gut content data

fw_name <- "Trancura size_agg"
dir_N <- 1e5
dir_tol <- 2 #different for ind and rand
n_gut_sample <- 100  
propn <- seq(1, 47, by = 2)
n_sel <- 1
n <- length(propn)
l_acc <- numeric(n)
r_acc <- numeric(n)
mean_acc <- numeric(n)

l_FPR <- numeric(n)
r_FPR <- numeric(n)
mean_FPR <- numeric(n)

desc_main <- "TSS_gut_ind"
rule <- "ind_predator"
l_ind <- 1

df <- data.frame(ndiet = integer(), nsample = integer(), a = double(), ai = double(),
                 aj = double(), r.b = double(), fw_TSS = double(), fw_FPR = double(),
                 gut_TSS = double())

for(i in propn){
  
  desc <- paste(desc_main, '_l', i, sep = '')
  # dir_main <- paste("results/rejection/",fw_name,"/", "rule_", rule,'/N=', dir_N, '_tol=', dir_tol, '_n_diet = ', i,
  #                   sep = "")
  dir_main <- paste("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_", rule ,'/N=', dir_N, '_tol=', dir_tol, '_n_diet = ', i,
                    sep = "")
  sim_TSS <- c()
  sim_FPR <- c()
  for(n_sample  in 1:n_gut_sample){
    dirnam <- paste(c(dir_main, '/rn_sample=', n_sample,'_N=', dir_N, '_tol=', dir_tol,"_", desc), collapse = '')
    fname <- paste(dirnam, "/", fw_name, ".Rdata", sep = "")
    prop_acc <- readRDS(fname)
    gut_TSS <- 1 - prop_acc$dist_gut
    gut_TSS_indexes <- order(gut_TSS, decreasing = TRUE)[1:n_sel]
    sim_TSS_temp <- prop_acc$TSS_fw[gut_TSS_indexes]
    sim_TSS <- c(sim_TSS, sim_TSS_temp)
    
    sim_FPR_temp <- prop_acc$FPR_fw[gut_TSS_indexes]
    sim_FPR <- c(sim_FPR, sim_FPR_temp)
    
    df <- rbind(df,
                data.frame(ndiet = i, nsample = n_sample, a = prop_acc$post_dists$a[gut_TSS_indexes], 
                           ai = prop_acc$post_dists$ai[gut_TSS_indexes],
                           aj = prop_acc$post_dists$aj[gut_TSS_indexes], 
                           r.b = prop_acc$post_dists$r.b[gut_TSS_indexes], 
                           fw_TSS = prop_acc$TSS_fw[gut_TSS_indexes], 
                           fw_FPR = prop_acc$FPR_fw[gut_TSS_indexes], 
                           gut_TSS = 1 - prop_acc$dist_gut[gut_TSS_indexes]))
  }
  
  range_acc <- range(sim_TSS)
  l_acc[l_ind] <- range_acc[1]
  r_acc[l_ind] <- range_acc[2]
  mean_acc[l_ind] <- mean(sim_TSS)
  
  range_FPR <- range(sim_FPR)
  l_FPR[l_ind] <- range_FPR[1]
  r_FPR[l_ind] <- range_FPR[2]
  mean_FPR[l_ind] <- mean(sim_FPR)
  
  l_ind <- l_ind+1
  print(i)
}

fname_df <- paste("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/", fw_name, "/rule_", rule, 
                  "/accepted_par/accepted_par.Rdata",
                  sep = "")

# saveRDS(object = df, file = fname_df)


ggplot(df) +
  geom_point(aes(x = gut_TSS, y = fw_TSS, color = as.factor(ndiet))) +
  theme_bw()
