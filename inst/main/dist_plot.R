#Use to plot distribution of food web properties and statistical properties

library(R.utils)
library(tictoc)

## Loading required libraries
library(ggpubr); library(plotrix); library(ggplot2)
library(cowplot); library(latex2exp); library(HDInterval)
library(doParallel); library(foreach); library(raster)
library(cheddar); library(dplyr); library(reshape2)
library(psych); library(DirectedClustering)

## Relative path from the project directory
sourceDirectory("R", modifiedOnly=FALSE)

library(ggjoy)

web.to.analyse <- "Benguela Pelagic"
dirnam <- "results/rejection/Benguela Pelagic/rN=1000_tol=0.6_TSS_unif_rall_WBF_3"

prop_val <- readRDS(paste(c(dirnam,"/properties.Rdata"),collapse = ''))$prop

foodweb_data <- load(paste("data/", web.to.analyse, ".web.Rdata", sep=""))
all.web.info <- get(foodweb_data)
real_val <- real_prop(all.web.info)

# dd <- subset(prop_val, select = -c(mean_trop_lvl, sd_vulner, sd_gen, mean_path_lt))
# 
# dd <- cbind(dd, id = 1:length(dd$connectance))
# dd <- melt(data = dd, id.vars = "id" )
# 
# ggplot(dd, aes(x = variable, y = value, fill = variable)) +
#   geom_violin(stat = "ydensity", adjust = 16, draw_quantiles = TRUE, trim = FALSE) +
#   theme_classic() +
#   coord_flip() +
#   geom_vline(data = real_val, xintercept = real_val$connectance) + 
#   scale_color_brewer(palette="Dark2")
# 
# real_val <- subset(real_val, select = -c(mean_trop_lvl))
# 
# real_val1 <- c(real_val$connectance, real_val$prop_basal, real_val$prop_inter, real_val$prop_top,
#                real_val$prop_herb, real_val$max_trop_lvl, real_val$mean_omn, real_val$clus_coeff,
#                real_val$diet_sim, 1, 1, 1, 1)
# 
# iris_lines <- data.frame(Species = 1:13, x0 = as.numeric(real_val1))
# 
# ggplot(dd, aes(x = value, y = variable)) +
#   geom_joy() +
#   theme_joy() +
#   geom_segment(data = iris_lines, aes(x = x0, xend = x0, y = 1:13,
#                                       yend = 1:13 + .9), color = "red")
  
## Just looking at TSS, ACC, prop.0.corr, prop.1.corr
n_metric <- 4
real_metric <- c(real_val$TSS, real_val$ACC, 1, 1)
pred_metric <- subset(prop_val, select = c(TSS, ACC, prop.0.corr, prop.1.corr))
pred_metric <- cbind(pred_metric, id = 1:length(pred_metric$TSS))
pred_metric <- melt(data = pred_metric, id.vars = "id" )
CI <- hdi(subset(prop_val, select = c(TSS, ACC, prop.0.corr, prop.1.corr)))

green_lines <- data.frame(Species = 1:4, x0 = c(CI[1], CI[3], CI[5], CI[7]), x1 = c(CI[2], CI[4], CI[6], CI[8]) )
red_lines <- data.frame(Species = 1:4, x0 = real_metric)
  
png(paste(c(dirnam,"/TSS_ACC_1_0.png"), collapse = ''), height = 3000, width = 3000, pointsize = 200)

ggplot(pred_metric, aes(x = value, y = variable)) +
  geom_joy(scale = 1) +
  theme_joy(font_size = 60) +
  theme(text = element_text(size=60, family="Comic Sans MS")) +
  geom_segment(data = green_lines, aes(x = x0, xend = x0, y = 1:n_metric,
                                      yend = 1:n_metric + .9), color = "green", size = 2) +
  geom_segment(data = green_lines, aes(x = x1, xend = x1, y = 1:n_metric,
                                       yend = 1:n_metric + .9), color = "green", size = 2) +
  scale_y_discrete(expand = c(0.01, 0)) +
  theme(axis.text.y=element_blank()) +
  annotate("text", x = 0.0, y = 1.5, label = str_wrap('True skill statistics', 10), size = 25, angle = 90,family="Comic Sans MS") +
  annotate("text", x = 0.0, y = 2.5, label = 'accuracy', size = 25, angle = 90,family="Comic Sans MS") +
  annotate("text", x = 0.0, y = 3.5, label = str_wrap('proportion of "no links" correct',14), size = 25, angle = 90,family="Comic Sans MS") +
  # annotate("text", x = 0.03, y = 3.5, label = 'zeros correct', size = 25, angle = 90,family="Comic Sans MS") +
  annotate("text", x = 0.0, y = 4.5, label = str_wrap('proportion of links correct',14), size = 25, angle = 90,family="Comic Sans MS") +
  # annotate("text", x = 0.03, y = 4.5, label = 'ones correct', size = 25, angle = 90,family="Comic Sans MS") +
  ylab("measures") +
  labs(title = str_wrap("Distribution of different accuracy measures for Benguela Pelagic food web", 40)) +
  theme(axis.title.x = element_text(size = 80, hjust = 0.5)) +
  theme(axis.title.y = element_text(size = 80, hjust = 0.5)) +
  theme(plot.title = element_text(size = 100, hjust = 0.5))


dev.off()

## Just looking at structural properties of a food web
n_str <- 8
real_str <- c(real_val$connectance, real_val$prop_basal, real_val$prop_inter, real_val$prop_top,
              real_val$prop_herb, real_val$mean_omn, real_val$clus_coeff, real_val$diet_sim)
pred_str <- subset(prop_val, select = c(connectance, prop_basal, prop_inter, prop_top,
                                        prop_herb, mean_omn, clus_coeff, diet_sim))
pred_str <- cbind(pred_str, id = 1:length(pred_str$connectance))
pred_str <- melt(data = pred_str, id.vars = "id" )

CI1 <- hdi(subset(prop_val, select = c(connectance, prop_basal, prop_inter, prop_top,
                                       prop_herb, mean_omn, clus_coeff, diet_sim)))

green_str_lines <- data.frame(Species = 1:n_str, x0 = c(as.numeric(CI1[1,])),
                          x1 = c(as.numeric(CI1[2,])))

red_str_lines <- data.frame(Species = 1:n_str, x0 = real_str)

png(paste(c(dirnam,"/str_prop.png"), collapse = ''), height = 3000, width = 3000, pointsize = 200)

ggplot(pred_str, aes(x = value, y = variable)) +
  geom_joy(aes()) +
  theme_joy(font_size = 60) +
  theme(text = element_text(size=60))+
  xlab("Value") +
  ylab("Structural Properties") +
  geom_segment(data = red_str_lines, aes(x = x0, xend = x0, y = 1:n_str,
                                     yend = 1:n_str + .9), color = "red", size = 2) +
  geom_segment(data = green_str_lines, aes(x = x0, xend = x0, y = 1:n_str,
                                       yend = 1:n_str + .9), color = "green", size = 2) +
  geom_segment(data = green_str_lines, aes(x = x1, xend = x1, y = 1:n_str,
                                       yend = 1:n_str + .9), color = "green", size = 2)

dev.off()

## Just looking at some structural properties of a food web
## diet_similarity, clus_coeff, connectance, mean omnivory

n_str <- 4
real_str <- c(real_val$connectance, real_val$clus_coeff, real_val$mean_omn, real_val$diet_sim)
pred_str <- subset(prop_val, select = c(connectance, mean_omn, clus_coeff, diet_sim))
                                        
pred_str <- cbind(pred_str, id = 1:length(pred_str$connectance))
pred_str <- melt(data = pred_str, id.vars = "id" )

CI1 <- hdi(subset(prop_val, select = c(connectance, mean_omn, clus_coeff, diet_sim)))

green_str_lines <- data.frame(Species = 1:n_str, x0 = c(as.numeric(CI1[1,])),
                              x1 = c(as.numeric(CI1[2,])))

red_str_lines <- data.frame(Species = 1:n_str, x0 = real_str)

png(paste(c(dirnam,"/str_prop.png"), collapse = ''), height = 3000, width = 3000, pointsize = 200)

ggplot(pred_str, aes(x = value, y = variable)) +
  geom_joy(scale = 1) +
  theme_joy(font_size = 60) +
  theme(text = element_text(size=60, family="Comic Sans MS"))+
  xlab("Value") +
  ylab("Structural Properties") +
  geom_segment(data = red_str_lines, aes(x = x0, xend = x0, y = 1:n_str,
                                         yend = 1:n_str + .9), color = "red", size = 4) +
  geom_segment(data = green_str_lines, aes(x = x0, xend = x0, y = 1:n_str,
                                           yend = 1:n_str + .9), color = "green", size = 4) +
  geom_segment(data = green_str_lines, aes(x = x1, xend = x1, y = 1:n_str,
                                           yend = 1:n_str + .9), color = "green", size = 4) +
  labs(title = str_wrap("Distribution of structural properties of Benguela Pelagic food web", 40)) +
  annotate("text", x = -0.1, y = 1.5, label = 'connectance', size = 25, angle = 90,family="Comic Sans MS") +
  annotate("text", x = -0.1, y = 2.5, label = str_wrap('mean omnivory', 10), size = 25, angle = 90,family="Comic Sans MS") +
  annotate("text", x = -0.1, y = 3.5, label = str_wrap('clustering coefficient',10), size = 25, angle = 90,family="Comic Sans MS") +
  annotate("text", x = -0.1, y = 4.5, label = str_wrap('diet similarity',10), size = 25, angle = 90,family="Comic Sans MS") +
  theme(axis.text.y=element_blank()) +
  theme(plot.title = element_text(size=80)) +
  theme(axis.title.x = element_text(size = 80, hjust = 0.5)) +
  theme(axis.title.y = element_text(size = 80, hjust = 0.5)) +
  theme(plot.title = element_text(size = 100, hjust = 0.5))

dev.off()
