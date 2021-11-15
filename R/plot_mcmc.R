# traceplot <- function(){
#   
#   nchain <- length(predicted_foodweb)
#   nsim <- length(predicted_foodweb[[index]]$post_dists['a'][[1]])
#   index <- 1
#   x_index <- 1:nsim
#   par_all <- tibble(par = predicted_foodweb[[index]]$post_dists['a'][[1]],
#                     chain = index,
#                     x_index = x_index)
#   
#   # for(index in 2:nchain){
#   #   par_all_1 <- tibble(par = predicted_foodweb[[index]]$post_dists['a'][[1]],
#   #                     chain = index,
#   #                     x_index = x_index)
#   #   par_all <- bind_rows(par_all, par_all_1)
#   }
#   
#   ggplot(par_all) +
#     geom_line(aes(x = x_index, y = par, color = factor(chain)))
#     
#     
# }
# 
# 
# plot_mcmc <- function(predicted_foodweb){
#   nchain <- length(predicted_foodweb)
#   
# }