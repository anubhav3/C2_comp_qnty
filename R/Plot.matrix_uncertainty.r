# #plots the predation matrix 
# 
# Plot.matrix_uncertainty <- function(web, title=" ", point.cex=0.5, trait.cex=1,
#                         diag.line=T, traits=F, by.consumer=T, axes.labels=T, sp.pt.ch=NA,
#                         nsim){
#   colfunc <- colorRampPalette(c("red", "white", "blue"))
#   user_pal <- c("#a6611a", "#dfc27d", "#f5f5f5", "#80cdc1", "#018571")
#   palette <- "Green-Brown"
#   n_col <- nsim
#   h_col <- diverge_hcl(n = n_col, palette = palette)
#   
#   S <- length(web[,1])
# 
#   ##point.cex <- 30/30
#   ##trait.cex <- 30/30
#   
#   dimnames(web) <- list(1:S, 1:S)
#   consumer <- rep(1:S, each=S)
#   resource <- rep(1:S, S)
#   web.list <- Matrix.to.list_uncertainty(web)
#   par(xpd=T)
#   par(mar = c(8, 8, 10, 8))
#   par(fig=c(0,3,0,4)/4)
#   plot(consumer, resource, pch=19, type="n", cex=0.1,
#        ann=F, axes=F,
#        xlim=c(1, S), ylim=c(1, S))
#   if(diag.line==T)
#     lines(1:S, S:1, lty="dashed", col = "black", lwd = 4)
#   
#   if(length(traits)==1)
#   index <- as.numeric(web.list[,3])
#   # index <- ceiling(index/(nsim/n_col))
#   points(web.list[,2], (S+1-as.numeric(web.list[,1])),
#          type="p", pch=19, cex=4,
#          col = h_col[index], lwd = 4)
#   
#   par(xpd=F)
#   mtext(side=3, text=title, line=6.2, cex=4.0, family="Comic Sans MS")
#   if(axes.labels){
#     mtext(side=2, "Resource", line=3.0, cex=3, family="Comic Sans MS")
#     mtext(side=2, "<--- Increasing bodysize ---<", line=1.0, cex=1.5, family="Comic Sans MS")
#     mtext(side=3, "Consumer", line=3.0, cex=3, family="Comic Sans MS")
#     mtext(side=3, ">--- Increasing bodysize --->", line=1.0, cex=1.5, family="Comic Sans MS")
#   }
# 
#   
#   # ## Plots the legend for the matrix
#   par(fig=c(2.5,4,0,4)/4)
#   par(new=T)
#   legend_image <- as.raster(matrix(h_col, ncol = 1))
#   plot(c(0,10),c(0,1),type = 'n', axes = F, xlab = '', ylab = '', main = 'Occurence proportion',
#        cex.main = 2.5, family="Comic Sans MS")
#   text(y = seq(0,1,l=5), x = 8.0, labels = round(seq(nsim, 0,l=5)/nsim,2), cex = 2.5, family="Comic Sans MS")
#   rasterImage(legend_image, 5, 0, 6.5,1, interpolate = FALSE)
#   lines(c(5,6.5),c(0.25,0.25))
#   lines(c(5,6.5),c(0.5,0.5))
#   lines(c(5,6.5),c(0.75,0.75))
#   
#   par(fig=c(0.25,2,0,2)/4)
#   par(new=T)
#   occurence <- data.frame(occ = as.vector(web))
#   hist(occurence$occ, xlab = "Number of occurences",
#        ylab = "Frequency", family = "Comic Sans MS",
#        main = "Distribution of occurences")
#   # ggplot(occurence) +
#   #   geom_histogram(aes(x = occ)) +
#   #   theme_classic() +
#   #   xlab("Number of occurences") +
#   #   ylab("Frequency") +
#   #   theme(text = element_text(size=15, family="Comic Sans MS"))
#   
#   return(web.list)
# }
# 
# 
# ## takes a food web in matrix format and converts it to list format
# Matrix.to.list_uncertainty <- function(web.matrix, predator.first=TRUE){
#   if(length(dimnames(web.matrix)[[1]])==length(web.matrix[1,]))
#     species.names <- dimnames(web.matrix)[[1]]
#   else
#     species.names <- 1:length(web.matrix[,1])
#   web.list <- matrix(0, sum(web.matrix>0), 3)
#   counter <- 1
#   for(i in 1:length(web.matrix[,1]))
#     for(j in 1:length(web.matrix[,1]))
#       if(web.matrix[i,j]>=1){
#         web.list[counter,] <- c(species.names[i],species.names[j], web.matrix[i,j])
#         counter <- counter + 1
#       }
#   if(!predator.first)
#     web.list <- cbind(web.list[,2], web.list[,1])
#   #print(web.list)
#   web.list <- web.list[sort.list(as.numeric(web.list[,3]),),]
#   web.list
# }
# 
# ## For real food web matrix
# Plot.matrix <- function(web, title=" ", point.cex=0.5, trait.cex=1,
#                         diag.line=T, traits=F, by.consumer=T, axes.labels=F, sp.pt.ch=NA){
#   
#   
#   S <- length(web[,1])
#   
#   ##point.cex <- 30/30
#   ##trait.cex <- 30/30
#   
#   dimnames(web) <- list(1:S, 1:S)
#   consumer <- rep(1:S, each=S)
#   resource <- rep(1:S, S)
#   web.list <- Matrix.to.list(web)
#   par(xpd=T)
#   par(mar = c(8, 8, 10, 8))
#   plot(consumer, resource, pch=19, type="n", cex=2,
#        ann=F, axes=F,
#        xlim=c(1, S), ylim=c(1, S),
#        col = "black", bg = "red", lwd = 2)
#   if(diag.line==T)
#     lines(1:S, S:1, lty="dashed", col = "black", lwd = 4)
#   
#   if(length(traits)==1)
#     points(web.list[,2], S+1-as.numeric(web.list[,1]),
#            type="p", pch=21, cex=4,
#            col = "black", bg = "red", lwd = 4)
#   # if(length(traits)==length(web)){
#   #   
#   #   colours.to.use <- rev(heat.colors(30)[c(-1:-5, -26:-30)])
#   #   ##colours.to.use <- rev(gray(1:30/30)[c(-1:-5, -26:-30)])
#   #   
#   #   if(by.consumer){
#   #     integer.traits <- matrix(0, S, S)
#   #     for(i in 1:S){
#   #       traits.01 <- traits[,i]-min(traits[,i])
#   #       traits.01 <- traits.01/max(traits.01)
#   #       integer.traits[,i] <- round(traits.01*19)+1
#   #       integer.traits[traits[,i]==0,i] = NaN
#   #       
#   #     }
#   #   }
#   #   
#   #   if(!by.consumer){
#   #     colours.to.use <- heat.colors(20)
#   #     traits.01 <- traits-min(traits)
#   #     traits.01 <- traits.01/max(traits.01)
#   #     integer.traits <- round(traits.01*19)+1
#   #   }
#   #   
#   #   if(point.cex>trait.cex){
#   #     points(web.list[,2], S+1-as.numeric(web.list[,1]),
#   #            type="p", pch=19, cex=point.cex, col="black")##colours.to.use[integer.traits])
#   #     points(rep(1:S, each=S), rep(S:1, times=S), 
#   #            pch=19, cex=trait.cex, col=colours.to.use[integer.traits])
#   #   }
#   #   if(point.cex<trait.cex){
#   #     points(rep(1:S, each=S), rep(S:1, times=S), 
#   #            pch=19, cex=trait.cex, col=colours.to.use[integer.traits])
#   #     points(web.list[,2], S+1-as.numeric(web.list[,1]),
#   #            type="p", pch=19, cex=point.cex, col="black")##colours.to.use[integer.traits])
#   #   }
#   #   
#   #   if(!is.na(sp.pt.ch))
#   #     points(web.list[,2], S+1-as.numeric(web.list[,1]),
#   #            type="p", pch=sp.pt.ch, cex=point.cex, col="black")##colours.to.use[integer.traits])
#   #   
#   #   
#   # }
#   # par(xpd=F)
#   mtext(side=3, text=title, line=6.2, cex=4.0, family="Comic Sans MS")
#   if(axes.labels){
#     mtext(side=2, "Resource", line=3.0, cex=3, family="Comic Sans MS")
#     mtext(side=2, "<--- Increasing bodysize ---<", line=1.0, cex=1.5, family="Comic Sans MS")
#     mtext(side=3, "Consumer", line=3.0, cex=3, family="Comic Sans MS")
#     mtext(side=3, ">--- Increasing bodysize --->", line=1.0, cex=1.5, family="Comic Sans MS")
#   }
#   
# }
# 
# Plot.matrix_uncertainty_real <- function(web, real_web, title=" ", point.cex=0.5, trait.cex=1,
#                                     diag.line=T, traits=F, by.consumer=T, axes.labels=T, sp.pt.ch=NA,
#                                     nsim){
#   colfunc <- colorRampPalette(c("red", "white", "blue"))
#   palette <- "Green-Brown"
#   n_col <- nsim
#   h_col <- h_col <- diverge_hcl(n = n_col, palette = palette)
#   
#   S <- length(web[,1])
#   
#   ##point.cex <- 30/30
#   ##trait.cex <- 30/30
#   
#   dimnames(web) <- list(1:S, 1:S)
#   dimnames(real_web) <- list(1:S, 1:S)
#   consumer <- rep(1:S, each=S)
#   resource <- rep(1:S, S)
#   web.list <- Matrix.to.list_uncertainty(web)
#   
#   web.list.real <- Matrix.to.list(real_web)
#   
#   par(xpd=T)
#   par(mar = c(8, 8, 10, 8))
#   par(fig=c(0,3,0,4)/4)
#   plot(consumer, resource, pch=19, type="n", cex=0.1,
#        ann=F, axes=F,
#        xlim=c(1, S), ylim=c(1, S))
#   if(diag.line==T)
#     lines(1:S, S:1, lty="dashed", col = "black", lwd = 4)
#   
#   if(length(traits)==1)
#     index <- as.numeric(web.list[,3])
#   index <- ceiling(index/(nsim/n_col))
#   points(web.list[,2], (S+1-as.numeric(web.list[,1])),
#          type="p", pch=19, cex=4,
#          col = h_col[index], lwd = 4)
#   
#   points(web.list.real[,2], (S+1-as.numeric(web.list.real[,1])),
#          type="p", pch=19, cex=1,
#          col = "black", lwd = 4)
#   
#   
#   
#   par(xpd=F)
#   mtext(side=3, text=title, line=6.2, cex=4.0, family="Comic Sans MS")
#   if(axes.labels){
#     mtext(side=2, "Resource", line=3.0, cex=3, family="Comic Sans MS")
#     mtext(side=2, "<--- Increasing bodysize ---<", line=1.0, cex=1.5, family="Comic Sans MS")
#     mtext(side=3, "Consumer", line=3.0, cex=3, family="Comic Sans MS")
#     mtext(side=3, ">--- Increasing bodysize --->", line=1.0, cex=1.5, family="Comic Sans MS")
#   }
#   
#   
#   # ## Plots the legend for the matrix
#   par(fig=c(2.5,4,0,4)/4)
#   par(new=T)
#   legend_image <- as.raster(matrix(h_col, ncol = 1))
#   plot(c(0,10),c(0,1),type = 'n', axes = F, xlab = '', ylab = '', main = 'Occurence proportion',
#        cex.main = 2.5, family="Comic Sans MS")
#   text(y = seq(0,1,l=5), x = 8.0, labels = round(seq(nsim, 0,l=5)/nsim,2), cex = 2.5, family="Comic Sans MS")
#   rasterImage(legend_image, 5, 0, 6.5,1, interpolate = FALSE)
#   lines(c(5,6.5),c(0.25,0.25))
#   lines(c(5,6.5),c(0.5,0.5))
#   lines(c(5,6.5),c(0.75,0.75))
# 
#   
#   par(fig=c(0.25,2,0,2)/4)
#   par(new=T)
#   occurence <- data.frame(occ = as.vector(web))
#   hist(occurence$occ, xlab = "Number of occurences",
#        ylab = "Frequency", family = "Comic Sans MS",
#        main = "Distribution of occurences")
#   # ggplot(occurence) +
#   #   geom_histogram(aes(x = occ)) +
#   #   theme_classic() +
#   #   xlab("Number of occurences") +
#   #   ylab("Frequency") +
#   #   theme(text = element_text(size=15, family="Comic Sans MS"))
#   
#   return(web.list)
# }
# 
# 
# 
# 
# ## takes a food web in matrix format and coverts it to list format
# Matrix.to.list <- function(web.matrix, predator.first=TRUE){
#   if(length(dimnames(web.matrix)[[1]])==length(web.matrix[1,]))
#     species.names <- dimnames(web.matrix)[[1]]
#   else
#     species.names <- 1:length(web.matrix[,1])
#   web.list <- matrix(0, sum(web.matrix), 2)
#   counter <- 1
#   for(i in 1:length(web.matrix[,1]))
#     for(j in 1:length(web.matrix[,1]))
#       if(web.matrix[i,j]==1){
#         web.list[counter,] <- c(species.names[i],species.names[j])
#         counter <- counter + 1
#       }
#   if(!predator.first)
#     web.list <- cbind(web.list[,2], web.list[,1])
#   web.list
# }
# 
# Plot.matrix_AplusB <- function(webA = webA, webB = webB){
#   
#   web <- webA
#   
#   S <- length(web[,1])
#   
#   dimnames(web) <- list(1:S, 1:S)
#   consumer <- rep(1:S, each=S)
#   resource <- rep(1:S, S)
#   web.list <- Matrix.to.list(web)
#   par(xpd=T)
#   par(mar = c(8, 8, 10, 8))
#   plot(consumer, resource, pch=19, type="n", cex=2,
#        ann=F, axes=F,
#        xlim=c(1, S), ylim=c(1, S),
#        col = "black", bg = "red", lwd = 2)
#   if(diag.line==T)
#     lines(1:S, S:1, lty="dashed", col = "black", lwd = 4)
#   
#   if(length(traits)==1)
#     points(web.list[,2], S+1-as.numeric(web.list[,1]),
#            type="p", pch=21, cex=4,
#            col = "black", bg = "red", lwd = 4)
#   
#   
# 
#   mtext(side=3, text=title, line=6.2, cex=4.0, family="Comic Sans MS")
#   if(axes.labels){
#     mtext(side=2, "Resource", line=3.0, cex=3, family="Comic Sans MS")
#     mtext(side=2, "<--- Increasing bodysize ---<", line=1.0, cex=1.5, family="Comic Sans MS")
#     mtext(side=3, "Consumer", line=3.0, cex=3, family="Comic Sans MS")
#     mtext(side=3, ">--- Increasing bodysize --->", line=1.0, cex=1.5, family="Comic Sans MS")
#   }
# }
# 
# 
