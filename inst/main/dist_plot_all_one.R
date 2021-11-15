#Use to plot distribution of food web properties

library(R.utils)
library(tictoc)

## Loading required libraries
library(ggpubr); library(plotrix); library(ggplot2)
library(cowplot); library(latex2exp); library(HDInterval)
library(doParallel); library(foreach); library(raster)
library(cheddar); library(dplyr); library(reshape2)
library(psych); library(DirectedClustering)
library(readxl)

## Relative path from the project directory
sourceDirectory("R", modifiedOnly=FALSE)

library(ggjoy)

dist_par_data <- read_excel("results/check/check_TSS.xlsx")
dist_par_data <- dist_par_data[c(1:15),]
tot_fw <- length(dist_par_data$foodweb)
foodweb_all <- dist_par_data$foodweb[1:tot_fw]

## Just looking at structural properties of a food web

plot_par <- list()
tot_prop <- 13
real_list <- readRDS(file = paste(c("data/","real_prop.Rdata"), collapse = ''))
#
for(foodweb in dist_par_data$foodweb){
  foodweb_data <- load(paste("data/", foodweb, ".web.Rdata", sep=""))
  all.web.info <- get(foodweb_data)
  real_list[[foodweb]] <- real_prop(all.web.info)
}
# saveRDS(real_list, file = paste(c("data/","real_prop.Rdata"), collapse = ''))

prop_name <-  c("connectance", "proportion basal", "proportion intermediate", "proportion top", 
                "proportion herbivores", "mean trophic level", "maximum trophic level", "mean omnivory",
                "clustering coefficient", "SD of generality", "SD of vulnerability", "diet similarity",
                "mean path length")
desc <- "TSS_unif_rall"
for(ind in 1:tot_prop){
  real_array <- as.numeric(tot_fw)
  mean_array <- as.numeric(tot_fw)
  pred_prop <- data.frame(prop = double(), foodweb = character())
  fw_ind <- 1
  for(foodweb in foodweb_all){
    
    dist <- dist_par_data$dist_r[fw_ind]
    dirnam <- paste("results/rejection/",foodweb,"/rN=1000_tol=", dist, "_", desc, sep = "")
    real_array[fw_ind] <- real_list[[foodweb]][ind]
    
    prop_val <- readRDS(paste(dirnam, "/", foodweb,"_prop.Rdata", sep = ""))$prop[,ind]
    
    pred_prop <- rbind(pred_prop, data.frame(prop = prop_val, foodweb = foodweb))
    
    fw_ind <- fw_ind + 1
  }
  
  pred_ci<- pred_prop %>%
      group_by(foodweb) %>%
      summarise(ci_lower = as.numeric(hdi(prop)[1]), ci_higher = as.numeric(hdi(prop)[2]))
  
  mean_array <- pred_prop %>%
                group_by(foodweb) %>%
                summarise(mean_prop = mean(prop))
  
  real_prop_lines <- data.frame(x0 = as.numeric(real_array))
  CI_prop_lines <- data.frame(x0 = c(as.numeric(pred_ci$ci_lower)),
                                x1 = c(as.numeric(pred_ci$ci_higher)))
  mean_lines <- data.frame(x0 = as.numeric(mean_array$mean_prop))
  
  
  plot_par[[ind]] <- ggplot(pred_prop, aes(x = prop, y = foodweb)) +
    geom_joy(aes()) +
    theme_classic() +
    labs(title = prop_name[ind]) +
    theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold")) +
    theme(text = element_text(size=20, family="Times New Roman"), axis.line = element_line(colour = 'black', size = 2),
          plot.margin = margin(2, 2, 2, 2, "cm"))+
    geom_segment(data = CI_prop_lines, aes(x = x0, xend = x0, y = 1:tot_fw,
                                             yend = 1:tot_fw + .9), color = "red", size = 1.5) +
    geom_segment(data = CI_prop_lines, aes(x = x1, xend = x1, y = 1:tot_fw,
                                             yend = 1:tot_fw + .9), color = "red", size = 1.5) +
    geom_segment(data = mean_lines, aes(x = x0, xend = x0, y = 1:tot_fw,
                                        yend = 1:tot_fw + .9), color = "black", size = 1.5) +
    geom_segment(data = real_prop_lines, aes(x = x0, xend = x0, y = 1:tot_fw,
                                            yend = 1:tot_fw + .9), color = "blue", size = 2.0, alpha = 0.5) +
    xlab("values") +
    ylab(NULL)
    
    
    
}

fname <- paste("results/comparison/", "r_properties_", desc, ".png", sep = "")

plot_all <- ggarrange(plot_par[[1]], plot_par[[2]], plot_par[[3]], plot_par[[4]], plot_par[[5]],
                      plot_par[[8]], plot_par[[9]], plot_par[[10]],
                      plot_par[[11]], plot_par[[12]],
                      nrow = 4, ncol = 3)
ggsave(filename = fname, plot = plot_all, height = 30, width = 25)

## Plotting connectane separately
fname_conn <- paste("results/comparison/", "r_connectance_", desc, ".png", sep = "")
ref_plot <- plot_par[[1]] +
  theme(axis.text = element_text(family = "Times New Roman", size = 50), axis.title.x = element_text(family = "Times New Roman", size = 50)) +
  labs(title = "") +
  xlim(c(0,1)) +
  xlab("connectance")
ggsave(filename = fname_conn, plot = ref_plot, height = 30, width = 25)


