plot_CI_vs_dist <- function(dist_x, conn_x, dirnam, true_val, fw_name){
  order_index_dist <- order(dist_x)
  
  sort_dist <- dist_x[order_index_dist]
  sort_conn <- conn_x[order_index_dist]
  
  nsim <- length(dist_x)
  nt <- 41
  
  dist <- numeric(nt)
  ci_lower <- numeric(nt)
  ci_upper <- numeric(nt)
  mean_r <- numeric(nt)
  dist_ind <- numeric(nt)
  
  for(i in 1:nt){
    # dist_ind[i] <- which.max(sort_dist >= i/10)
    # conn <- sort_conn[seq(1,dist_ind[i])]
    conn <- sort_conn[which(sort_dist <= i/20)]
    
    ci <- hdi(conn)
    ci_lower[i] <- ci[1]
    ci_upper[i] <- ci[2]
    mean_r[i] <- mean(conn)
    dist[i] <- i/20
  }
  
  dd <- data.frame(x = dist, y = mean_r, ci_lower = ci_lower, ci_upper = ci_upper)
  
  fname <- paste(dirnam, "/",fw_name, "_connectanceCI_vs_tol.png", sep = "")
  
  ggplot(dd) +
    geom_point(aes(x  = dist, y = mean_r)) +
    geom_line(aes(x  = dist, y = mean_r)) +
    geom_errorbar(aes(x  = dist, ymin = ci_lower, ymax = ci_upper), col = "red") +
    xlab("distance tolerance") +
    ylab("connectance") +
    # labs(title = "Connectance vs distance (ss = TSS)") +
    # scale_x_reverse() +
    geom_hline(yintercept = true_val, col = "green")+
    # theme_classic() +
    theme(axis.title = element_text(size=30, family="Times New Roman"), 
          axis.text = element_text(size=20, family="Times New Roman")) +
    # xlim(c(0,2)) +
    ylim(c(0,1))
  
  ggsave(filename = fname, width = 10, height = 10)
  
}


plot_CI_vs_dist_sir <- function(dist_x, conn_x, dirnam, true_val, fw_name){
  order_index_dist <- order(dist_x)
  
  sort_dist <- dist_x[order_index_dist]
  sort_conn <- conn_x[order_index_dist]
  
  nsim <- length(dist_x)
  n_steps <- 20
  
  dist <- numeric(n_steps)
  ci_lower <- numeric(n_steps)
  ci_upper <- numeric(n_steps)
  mean_r <- numeric(n_steps)
  dist_ind <- numeric(n_steps)
  
  
  min_x <- min(dist_x)
  max_x <- max(dist_x)
  h_x <- (max_x-min_x)/20
  range_x <- min_x + h_x*c(1:n_steps)
    
  for(i in 1:n_steps){
    # dist_ind[i] <- which.max(sort_dist >= i/10)
    # conn <- sort_conn[seq(1,dist_ind[i])]
    conn <- sort_conn[which(sort_dist <= range_x[i])]
    
    ci <- hdi(conn)
    ci_lower[i] <- ci[1]
    ci_upper[i] <- ci[2]
    mean_r[i] <- mean(conn)
    dist[i] <- range_x[i]
  }
  
  dd <- data.frame(x = dist, y = mean_r, ci_lower = ci_lower, ci_upper = ci_upper)
  
  fname <- paste(dirnam, "/",fw_name, "_connectanceCI_vs_tol.png", sep = "")
  
  ggplot(dd) +
    geom_point(aes(x  = dist, y = mean_r)) +
    geom_line(aes(x  = dist, y = mean_r)) +
    geom_errorbar(aes(x  = dist, ymin = ci_lower, ymax = ci_upper), col = "red") +
    xlab("distance tolerance") +
    ylab("connectance") +
    # labs(title = "Connectance vs distance (ss = TSS)") +
    # scale_x_reverse() +
    geom_hline(yintercept = true_val, col = "green")+
    # theme_classic() +
    theme(axis.title = element_text(size=30, family="Times New Roman"), 
          axis.text = element_text(size=20, family="Times New Roman")) +
    # xlim(c(0,2)) +
    ylim(c(0,1))
  
  ggsave(filename = fname, width = 10, height = 10)
  
}


fw_name <- "sim_Small Reef"
dirnam <- paste("results/rejection/",fw_name, "/rN=1e+05_tol=2000_TSS_sir_r1", sep = "")

fw_data <- readRDS(paste(dirnam, "/", fw_name, ".Rdata", sep = ""))
fw_dist <- fw_data$dist
fw_conn <- fw_data$acc_ss


fname_real <- paste("data/", fw_name, ".web.Rdata", sep= "")
fw_real <- readRDS(file = fname_real)

real_conn <- real_prop(all.web.info = fw_real)$connectance

plot_CI_vs_dist_sir(fw_dist, fw_conn, dirnam, real_conn, fw_name)





