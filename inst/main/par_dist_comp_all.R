# Used to compare predicted parameter values of posterior distribution from different ABC methods

library(HDInterval)
library(ggplot2)
library(readxl)

dist_par_data <- read_excel("data/dist_par_data.xlsx")
ind <- 1
for(foodweb in dist_par_data$foodweb){
  
  foodweb_data <- load(paste("data/", web.to.analyse, ".web.Rdata", sep=""))
  all.web.info <- get(foodweb_data)
  
  dir_rej <- paste("rN=1000_tol=", dist_par_data$dist[ind] ,"_TSS_unif_rall", sep = "")
  dir_mcmc <- paste("mN=1e+05_tol=", dist_par_data$dist[ind] ,"_TSS_unif_rall", sep = "")
  rej_data <- readRDS(paste("results/rejection/", foodweb, "/", dir_rej, "/", foodweb, ".Rdata", sep = ""))
  mcmc_data <- readRDS(paste("results/mcmc/", foodweb, "/", dir_mcmc, "/", foodweb, ".Rdata", sep = ""))
  # smc_data <- readRDS("results/smc/Benguela Pelagic/sN=1000_tol=0.6_TSS_unif_rall_WBF/Benguela Pelagic.Rdata")
  pnas_data <- data.frame(a = log10(dist_par_data$a[ind]), ai = dist_par_data$ai[ind], 
                          aj = dist_par_data$aj[ind], r.b = dist_par_data$r.b[ind])
  
  rej_post <- rej_data$post_dists
  mcmc_post <- mcmc_data[[1]]$post_dists
  after_burn_mcmc_post <- after_burn(mcmc_post)
  after_thin <- thinning(after_burn_mcmc_post, 140)
  # smc_post <- smc_data$post_dists[,,5] * smc_data$w[,5]
  
  rej_par <- hdi(rej_post)
  mcmc_par <- hdi(after_thin)
  # smc_par <- hdi(smc_post)
  pp <- ADBM_prior_par(all.web.info)
  prior_par <- array(dim = c(2,4))
  prior_par[1,1] <- pp$l_log_a
  prior_par[2,1] <- pp$r_log_a
  prior_par[1,2] <- pp$l_ai
  prior_par[2,2] <- pp$r_ai
  prior_par[1,3] <- pp$l_aj
  prior_par[2,3] <- pp$r_aj
  prior_par[1,4] <- pp$l_log_r.b
  prior_par[2,4] <- pp$r_log_r.b
  
  
  i <- 2
  j <- 5
  
  #Prior parameters
  df <- data.frame(x0 = as.double(prior_par[1,]), x1 = as.double(prior_par[2,]), y0 = c(i-1,i+j-1,i+2*j-1, i+3*j-1), y1 = c(i-1,i+j-1,i+2*j-1, i+3*j-1), Estimates = "Prior", par = names(rej_par[1,]))
  
  # rejection
  df <- rbind(df, data.frame(x0 = as.double(rej_par[1,]), x1 = as.double(rej_par[2,]), y0 = c(i,i+j,i+2*j, i+3*j), y1 = c(i,i+j,i+2*j, i+3*j), Estimates = "Rejection", par = names(rej_par[1,])))
  #MCMC
  df <- rbind(df, data.frame(x0 = as.double(mcmc_par[1,]), x1 = as.double(mcmc_par[2,]), y0 = c(i+1,i+j+1,i+2*j+1, i+3*j+1), y1 = c(i+1,i+j+1,i+2*j+1, i+3*j+1), Estimates = "MCMC", par = names(rej_par[1,])))
  #SMC
  # df <- rbind(df, data.frame(x0 = as.double(smc_par[1,]), x1 = as.double(smc_par[2,]), y0 = c(i+2,i+j+2,i+2*j+2, i+3*j+2), y1 = c(i+2,i+j+2,i+2*j+2, i+3*j+2), Method = "SMC", par = names(rej_par[1,])))
  
  #PNAS
  df <- rbind(df, data.frame(x0 = as.double(pnas_data), x1 = as.double(pnas_data), y0 = c(i+3,i+j+3,i+2*j+3, i+3*j+3), y1 = c(i+3,i+j+3,i+2*j+3, i+3*j+3), Estimates = "PNAS 2008", par = names(rej_par[1,])))
  
  df1 <- data.frame(x0 = as.double(pnas_data), x1 = as.double(pnas_data), y0 = c(i+0.5, i+j+0.5, i+2*j+0.5, i+3*j+0.5), y1 = c(i+0.5, i+j+0.5, i+2*j+0.5, i+3*j+0.5), Estimates = "PNAS 2008", par = names(rej_par[1,]))
  
  fname <- paste("results/comparison/", foodweb, "_par.png", sep = "")
  # png(filename = fname, height = 1000, width = 1200, pointsize = 30)
  cols = c("Prior" = "black", "Rejection" = "red", "MCMC" = "blue", "PNAS 2008" = "green")
  labes = c("Prior" = "Prior distribution", "Rejection" = "Rejection approach", "MCMC" = "MCMC approach", "PNAS 2008" = "Point estimates from Petchey et al (2008)")
  
  main_title <- paste("Estimates of model parameters for ", foodweb, "food web")
  
  plot_par <- ggplot() +
    geom_segment(data = df, aes(x = x0, xend = x1, y = y0, yend = y1, color = Estimates), size = 3) +
    geom_point(data = df1, aes(x = x0, y = y0, color = Estimates), size = 5) +
    theme_classic() + 
    # annotate("text", x = -1, y = 1.5, label = 'log[10](a)', parse = TRUE, size = 10,family="Comic Sans MS") +
    # annotate("text", x = 0.75, y = 4.5, label = 'a[i]', parse = TRUE, size = 10,family="Comic Sans MS") +
    # annotate("text", x = -1, y = 7.5, label = 'a[j]', parse = TRUE, size = 10,family="Comic Sans MS") +
    # annotate("text", x = 0, y = 10.5, label = 'log[10](b)', parse = TRUE, size = 10,family="Comic Sans MS") +
    xlab("Values") +
    ylab("Parameters") +
    theme(text = element_text(size=20, family="Comic Sans MS"), axis.line = element_line(colour = 'black', size = 2),
          plot.margin = margin(2, 2, 2, 2, "cm")) +
    scale_color_manual(values = cols, labels = str_wrap(labes, 20))+
    labs(title = str_wrap(main_title, 35))+
    theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold")) +
    # theme(axis.text.y = element_blank()) +
    scale_y_continuous(breaks = c(i+0.5, i+j+0.5, i+2*j+0.5, i+3*j+0.5),
                       labels = c(TeX('$log_{10}a$'), TeX('$a_i$'), TeX('$a_j$'), TeX('$log_{10}b$')))
  
  ggsave(filename = fname, plot = plot_par, height = 10, width = 10)
  
  ind <- ind + 1
}

