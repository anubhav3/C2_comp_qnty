Plot.matrix_multiple <- function(webA, webB, webC, webD, webR, diag.line=T, traits=F){
  
  S <- length(webA[,1])
  # webAplusB <- webA + webB
  
  dimnames(webA) <- list(1:S, 1:S)
  dimnames(webB) <- list(1:S, 1:S)
  dimnames(webC) <- list(1:S, 1:S)
  dimnames(webD) <- list(1:S, 1:S)
  dimnames(webR) <- list(1:S, 1:S)
  consumer <- rep(1:S, each=S)
  resource <- rep(1:S, S)
  
  webA.list <- Matrix.to.list_uncertainty(webA)
  webB.list <- Matrix.to.list_uncertainty(webB)
  webC.list <- Matrix.to.list_uncertainty(webC)
  webD.list <- Matrix.to.list_uncertainty(webD)
  webR.list <- Matrix.to.list_uncertainty(webR)
  # webAplusB.list <- Matrix.to.list_uncertainty(webAplusB)
  
  pch_size <- 19
  cex_size <- 0.3
  
  h_col <- c("#edf8fb", "#2ca25f", "#006d2c")
  n_col <- length(h_col)
  nsim <- 2
  
  #Plotting matrix A
  index_A <- as.numeric(webA.list[,3]) + 1
  dd_A <- data.frame(consumer = as.numeric(webA.list[,2]), resource = S+1-as.numeric(webA.list[,1]), occ = as.factor(index_A))
  
  #Plotting matrix B
  index_B <- as.numeric(webB.list[,3]) + 1
  dd_B <- data.frame(consumer = as.numeric(webB.list[,2]) + 2*S, resource = S+1-as.numeric(webB.list[,1]), occ = as.factor(index_B))
  
  #Plotting matrix C
  index_C <- as.numeric(webC.list[,3]) + 1
  dd_C <- data.frame(consumer = as.numeric(webC.list[,2]) + 2*S, resource = S+1-as.numeric(webC.list[,1])-2*S, occ = as.factor(index_C))
  
  #Plotting matrix D
  index_D <- as.numeric(webD.list[,3]) + 1
  dd_D <- data.frame(consumer = as.numeric(webD.list[,2]), resource = S+1-as.numeric(webD.list[,1])-2*S, occ = as.factor(index_D))
  
  #Plotting matrix R (real)
  index_R <- as.numeric(webR.list[,3]) + 1
  dd_R <- data.frame(consumer = as.numeric(webR.list[,2]) + S, resource = S+1-as.numeric(webR.list[,1])-S, occ = as.factor(index_R))
  
  #Plotting matrix AplusB
  
  # index_AplusB <- as.numeric(webAplusB.list[,3]) + 1
  # dd_AplusB <- data.frame(consumer = as.numeric(webAplusB.list[,2]) + 4*S, resource = S+1-as.numeric(webAplusB.list[,1]), occ = as.factor(index_AplusB))
  
  labels_prop <- c("1" = "0" , "2" = "1", "3" = "2")
  psize <- 2
  plot_multiple <-  ggplot() +
    geom_segment(aes(x = 1, y = S, xend = S, yend = 1), lty = "dashed") +
    geom_segment(aes(x = 2*S+1, y = S, xend = 3*S, yend = 1), lty = "dashed") +
    geom_segment(aes(x = 2*S+1, y = S-2*S, xend = 3*S, yend = 1-2*S), lty = "dashed") +
    geom_segment(aes(x = 1, y = S-2*S, xend = S, yend = 1-2*S), lty = "dashed") +
    geom_segment(aes(x = 1 + S, y = S-S, xend = S+S, yend = 1-S), lty = "dashed") +
    geom_point(data = dd_A, aes(x = consumer, y = resource, col = occ), size = psize) +
    geom_point(data = dd_B, aes(x = consumer, y = resource, col = occ), size = psize) +
    geom_point(data = dd_C, aes(x = consumer, y = resource, col = occ), size = psize) +
    geom_point(data = dd_D, aes(x = consumer, y = resource, col = occ), size = psize) +
    geom_point(data = dd_R, aes(x = consumer, y = resource, col = occ), size = psize) +
    # geom_point(data = dd_AplusB, aes(x = consumer, y = resource, col = occ), size = psize) +
    scale_color_manual(values = h_col, labels = labels_prop, name = expression(paste("Number of occurences \n (out of 2)"))) +
    theme_classic() +
    theme(axis.line=element_blank(), axis.ticks = element_blank(), axis.text = element_blank(),
          axis.title = element_blank(),
          plot.margin = margin(3,.8,3,1.8, "cm"), text = element_text(size = 30, family="Times New Roman"), legend.position = "none")
  
  
  return(plot_multiple)
}

## Getting the parameter values and creating webs corresponding to them

fname <- "results/rejection/Benguela Pelagic/rN=1000_tol=0.7_TSS_unif_rall/Benguela Pelagic.Rdata"
f_data <- readRDS(file = fname)
par_full <- f_data$post_dists
order_a <- order(f_data$post_dists$r.b)
par_full <- par_full[order_a,]
n <- 700
parA <- par_full[600,]
parB <- par_full[620,]
parC <- par_full[630,]
parD <- par_full[640,]

webA = give_web(par = parA, other_par = model_core_par)
webB = give_web(par = parB, other_par = model_core_par)
webC = give_web(par = parC, other_par = model_core_par)
webD = give_web(par = parD, other_par = model_core_par)
webR = readRDS("data_new/Benguela Pelagic.web.Rdata")$predation.matrix

Plot.matrix_multiple(webA = webA, webB = webB, webC = webC, webD = webD, webR = webR)