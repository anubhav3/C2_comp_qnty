
fw_prop_1 <- readRDS("results/rejection/Benguela Pelagic/rN=1000_tol=0.7_TSS_unif_rall/Benguela Pelagic_prop.Rdata")$prop
fw_prop_2 <- readRDS("results/rejection/Grasslands/rN=1000_tol=1_TSS_unif_rall/Grasslands_prop.Rdata")$prop
fw_dist_1 <- 1-fw_prop_1$TSS
fw_dist_2 <- 1-fw_prop_2$TSS
fw_name_1 <- "Benguela Pelagic"
fw_name_2 <- "Grasslands"
tidy_post <- tibble(prop.1.corr = fw_prop_1$prop.1.corr, prop.0.corr = fw_prop_1$prop.0.corr,
                    ACC = fw_prop_1$ACC, TSS = fw_prop_1$TSS, dist = fw_dist_1, foodweb = fw_name_1)

tidy_post <- bind_rows(tidy_post,
                       tibble(prop.1.corr = fw_prop_2$prop.1.corr, prop.0.corr = fw_prop_2$prop.0.corr,
                              ACC = fw_prop_2$ACC, TSS = fw_prop_2$TSS, dist = fw_dist_2, foodweb = fw_name_2)
                       )
alpha_f <- 0.5


dd <- data.frame(prop = tidy_post$prop.1.corr, foodweb = tidy_post$foodweb)
cols <- c("Benguela Pelagic" = "red", "Grasslands" = "blue")
plot_density <- function(dd, prop_name, cols){
  
  plot_dd<- ggplot(dd) +
      geom_density(aes(x = prop, fill = foodweb), alpha = alpha_f, position = 'identity') +
      theme_classic() +
    theme(legend.position = "none", text = element_text(family = "Times New Roman", size = 20), axis.title.y = element_blank(),
          plot.margin = margin(0,0.5,0.5,1, "cm")) +
    xlab(prop_name) +
    scale_fill_manual(values = cols) +
    xlim(c(0,1))
  
  return(plot_dd)
}
dd1 <- data.frame(prop = tidy_post$prop.1.corr, foodweb = tidy_post$foodweb)
p1 <- plot_density(dd = dd1, prop_name = "proportion of presence of links correct", cols = cols)
dd2 <- data.frame(prop = tidy_post$prop.0.corr, foodweb = tidy_post$foodweb)
p2 <- plot_density(dd = dd2, prop_name = "proportion of absence of links correct", cols = cols)
dd3 <- data.frame(prop = tidy_post$ACC, foodweb = tidy_post$foodweb)
p3 <- plot_density(dd = dd3, prop_name = "ACC", cols = cols)
dd4 <- data.frame(prop = tidy_post$TSS, foodweb = tidy_post$foodweb)
p4 <- plot_density(dd = dd4, prop_name = "True skill statistics", cols = cols)
# dd5 <- data.frame(prop = tidy_post$dist, foodweb = tidy_post$foodweb)
# p5 <- plot_density(dd = dd5, prop_name = "distance", cols = cols)


main_plot <- ggarrange(p1,p2,p3,p4, ncol = 1)
annotate_figure(main_plot, left = text_grob("density", size = 20, family = "Times New Roman", rot = 90))
fname <- "results/comparison/measures_comp_two.png"
ggsave(filename = fname)
