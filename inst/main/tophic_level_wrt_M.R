#Plot trophic level of species with their body sizes

pred_output <- readRDS("../C1_method/results/rejection/Benguela Pelagic/rN=1e+05_tol=2_TSS_only_a/Benguela Pelagic.Rdata")


tl_plot <-  trop_levels %>%
  na.omit() %>%
  # filter(sp_names == c("n20")) %>%
  ggplot() +
  geom_point(aes(x = sp_names, y = tl, color = conn)) +
  xlab("Connectance of food web") +
  ylab("Trophic levels") +
  # scale_color_discrete(name = "Species identity") +
  theme_classic() +
  theme(axis.text = element_text(family = "Times New Roman", size = 20),
        axis.title = element_text(family = "Times New Roman", size = 20),
        legend.title = element_text(family = "Times New Roman", size = 20),
        legend.text = element_text(family = "Times New Roman", size = 20))+
  scale_x_discrete(limits = c("n1", "n2", "n3", "n4", "n5", "n6", "n7", "n8", "n9", "n10",
                              "n11", "n12", "n13", "n14", "n15", "n16", "n17", "n18", "n19", "n20",
                              "n21", "n22", "n23", "n24", "n25", "n26", "n27", "n28", "n29"))
