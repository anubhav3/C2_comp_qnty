#Use to plot distribution of statistical properties

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

## Just looking at TSS, ACC, prop.0.corr, prop.1.corr

dist_par_data <- read_excel("data/dist_TSS_conn.xlsx")
dist_par_data <- dist_par_data[c(1:4,6:15),]
foodweb_all <- dist_par_data$foodweb

plot_par <- list()
tot_prop <- 4
tot_fw <- length(foodweb_all)
prop_name <-  c("proportion of links correct", "proportion of no links correct",
                "accuracy", "true skill statistics")
desc <- "conn_TSS_1"
for(ind in 1:tot_prop){
  real_array <- rep(1, tot_fw)
  pred_prop <- data.frame(prop = double(), foodweb = character())
  fw_ind <- 1
  for(foodweb in foodweb_all){
    
    dist <- dist_par_data$dist[fw_ind]
    dirnam <- paste("results/rejection/",foodweb,"/rN=1000_tol=", dist, "_", desc, sep = "")
    
    prop_val <- readRDS(paste(c(dirnam,"/properties.Rdata"),collapse = ''))$prop[,ind+13]
    
    pred_prop <- rbind(pred_prop, data.frame(prop = prop_val, foodweb = foodweb))
    
    fw_ind <- fw_ind + 1
  }
  
  pred_ci<- pred_prop %>%
    group_by(foodweb) %>%
    summarise(ci_lower = as.numeric(hdi(prop)[1]), ci_higher = as.numeric(hdi(prop)[2]))
  
  red_prop_lines <- data.frame(x0 = as.numeric(real_array))
  green_prop_lines <- data.frame(x0 = c(as.numeric(pred_ci$ci_lower)),
                                 x1 = c(as.numeric(pred_ci$ci_higher)))
  
  
  
  plot_par[[ind]] <- ggplot(pred_prop, aes(x = prop, y = foodweb)) +
    geom_joy(aes()) +
    theme_classic() +
    labs(title = prop_name[ind]) +
    theme(plot.title = element_text(size = 30, hjust = 0.5, face = "bold")) +
    theme(text = element_text(size=20, family="Times New Roman"), axis.line = element_line(colour = 'black', size = 2),
          plot.margin = margin(2, 1, 2, 1, "cm")) +
    # geom_segment(data = red_prop_lines, aes(x = x0, xend = x0, y = 1:tot_fw,
    #                                         yend = 1:tot_fw + .9), color = "red", size = 0.5) +
    geom_segment(data = green_prop_lines, aes(x = x0, xend = x0, y = 1:tot_fw,
                                              yend = 1:tot_fw + .9), color = "green", size = 0.5) +
    geom_segment(data = green_prop_lines, aes(x = x1, xend = x1, y = 1:tot_fw,
                                              yend = 1:tot_fw + .9), color = "green", size = 0.5) +
    xlab("values") +
    ylab(NULL)
    # 
  
  
}

fname <- paste("results/comparison/", "r_stat_measures_", desc,".png", sep = "")

plot_all <- ggarrange(plot_par[[1]], plot_par[[2]], plot_par[[3]], plot_par[[4]],
                      nrow = 1, ncol = 4)
ggsave(filename = fname, plot = plot_all, height = 30, width = 30)


