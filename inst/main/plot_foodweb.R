# 2022.05.23
# We plot food webs with the help of cheddar package

library(cheddar)
library(R.utils)

sourceDirectory("../C1_method_v2/R", modifiedOnly=FALSE)

fw_name <- "Trancura size_agg"

fw_data <- readRDS(paste0("data/", fw_name, ".web.Rdata"))
size_nodes <- log10(fw_data$species.sizes)
# fw_data <- mat.to.comm(pred.mat = fw_data$predation.matrix, fw_title = fw_name)

# PlotWebByLevel(fw_data, x.layout = 'wide', round.levels.to.nearest=0.5, y.layout='compress')


#### Changing food web into format -- edge list understood by the ggnet2 package ####
library(network)
library(ggnet)

pred_mat <- fw_data$predation.matrix
nspecies <- dim(pred_mat)[1]
size_nodes_dict <- data.frame(nodes = paste0("n", 1:nspecies), size_nodes)

edge_list <- data.frame(Source = character(), Target = character())
for(predator_i in 1:nspecies){
  for(prey_j in 1:nspecies){
    if(pred_mat[prey_j, predator_i] == 1){
      edge_list <- rbind(edge_list,
                         data.frame(Source = paste0("n", predator_i),
                                    Target = paste0("n", prey_j)))
    }
  }
}

prey_nodes <- unique(edge_list$Target)
predator_nodes <- unique(edge_list$Source)
nodes <- union(predator_nodes, prey_nodes)

nodes_df <- data.frame(nodes = nodes)
nodes_df_size <- merge(x = nodes_df, y = size_nodes_dict, by = "nodes", sort = FALSE)
min_size <- abs(min(nodes_df_size$size_nodes)) + 1

net <- network(edge_list, directed = TRUE, loops = TRUE)
# plot_net <- ggnet2(net, color = "red", edge.size = 0.5, edge.color = "grey",
#        size = nodes_df_size$size_nodes + min_size, legend.position = "None",
#        mode = "target") +
#   theme(panel.background = element_rect(fill = "black"))

# ggnet2(net)$data
# ggsave(plot = plot_net, filename = "results/plot_foodweb/Broadstone_Stream.png", dpi = 900)



ggnet2(net, color = "red", size = 0, edge.color = "black", mode = "kamadakawai", directed = TRUE,
       arrow.size = 5, arrow.gap = 0.015) +
  geom_point(size = nodes_df_size$size_nodes + min_size + 2, color = "darkred") +
  geom_point( size = nodes_df_size$size_nodes + min_size, color = "red") +
  theme(panel.background = element_rect(fill = "white"), legend.position = "None",
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0))
  
# ggsave(filename = paste0("results/plot_foodweb/",fw_name,".png"), dpi = 450, height = 10, width = 10)
