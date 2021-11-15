# returns distribution plot of occurences in ensemble of predicted predation matrix

plot_occurence <- function(pred_mat){
  pred_vect <- as.vector(pred_mat)
  pred_vect_nozero <- pred_vect[pred_vect!=0] 
  no_zeros <- sum(pred_vect==0)
  dd <- data.frame(pred <- pred_vect_nozero/max(pred_vect_nozero))
  
  ggplot(dd)+
    geom_histogram(aes(x = pred), breaks = seq(0.001, 1.001, length.out = 20)) +
    geom_bar(aes(x = pred)) +
    xlab("Proportion of occurences") +
    ylab("Frequency") +
    geom_segment(x = 0, xend = 0, y = 0, yend = no_zeros, color = "red", lwd = 3) +
    theme_classic() +
    ylim(c(0, 1.05*no_zeros)) 
}

