#Takes in posterior parameter values and calculates food webs

fw_name <- "sim_Benguela Pelagic"
  
pred_output <- readRDS("../C1_method/results/rejection/Benguela Pelagic/rN=1e+05_tol=2_TSS_lower_a/Benguela Pelagic.Rdata")

# pred_output <- readRDS("../C1_method/results/rejection/Benguela Pelagic/rN=1e+05_tol=2_TSS_lower_a/Benguela Pelagic.Rdata")

pred_par <- pred_output$post_dists
n <- length(pred_output$dist)
trop_levels <- data.frame(tl = double(), sp_names = character(), conn = double())
# index <- sample(c(1:n), 1000)
index <- 1:1e5
TL_diet <- data.frame(TL = double(), nprey = factor(), pred = factor(), TSS = double())

for(i in 1:1e4){
  # web.to.analyse <- "sim_Small Reef"
  web.to.analyse <- fw_name
  fname <- paste("data/", web.to.analyse, ".web.Rdata", sep="")
  all.web.info <- readRDS(file = fname)
  
  n_species <- length(all.web.info$species.sizes)
  
  M <- all.web.info$species.sizes
  M <- sort(M)
  
  sim_a <- 10^pred_par$a[i]
  sim_ai <- pred_par$ai[i]
  sim_aj <- pred_par$aj[i]
  sim_r.b <- 10^pred_par$r.b[i]
  
  local_par <- data.frame(a = sim_a, ai = sim_ai, aj = sim_aj, r.b = sim_r.b)
  sim_model_core_par <- list(e = 1, n = 1, ni = -3/4, r.a = 1, M  = M)
  pred_mat <- ratio.power(opt=local_par, x = sim_model_core_par)
  S <- dim(pred_mat)[1]
  conn <- sum(pred_mat)/S^2
  TSS <- 1-pred_output$dist[i]
  
  tp <- trophic_position(web = pred_mat, title = web.to.analyse)
  
  TL_diet <- rbind(TL_diet,
                   data.frame(TL = tp, nprey = colSums(pred_mat), pred = 1:29, TSS = TSS))
  trop_levels <- rbind(trop_levels,
                       data.frame(tl = as.numeric(tp), sp_names = names(tp), conn = conn, TSS = TSS))
  
  
  
}


tl_plot <-  trop_levels %>%
            na.omit() %>%
            # filter(tl > -2.224577e+15) %>%
            # filter(sp_names %in% c("n50","n1", "n10", "n20", "n30", "n40")) %>%
            # filter(sp_names %in% c("n1", "n2", "n5", "n10", "n29")) %>%
            ggplot() +
              # geom_line(aes(x = TSS, y = tl, color = sp_names)) +
              geom_point(aes(x = TSS, y = tl, color = sp_names), size = 0.1) +
              # xlab("Connectance of food web") +
              # xlim(0, 1) +
              ylim(0, 7) +
              xlab("TSS (predicted food web, observed food web)") +
              ylab("Trophic levels") +
              scale_color_discrete(name = "Species identity") +
              theme_bw() +
              theme(axis.text = element_text(family = "Times New Roman", size = 20),
                    axis.title = element_text(family = "Times New Roman", size = 20),
                    legend.title = element_text(family = "Times New Roman", size = 20),
                    legend.text = element_text(family = "Times New Roman", size = 20)) +
              facet_wrap(~sp_names) +
              geom_smooth(aes(x = TSS, y = tl))



sp <- data.frame(n29 = trop_levels$tl[which(trop_levels$sp_names == "n29")],
                 n28 = trop_levels$tl[which(trop_levels$sp_names == "n28")],
                 n11 = trop_levels$tl[which(trop_levels$sp_names == "n11")],
                 n12 = trop_levels$tl[which(trop_levels$sp_names == "n12")])

ggplot(sp) +
  geom_point(aes(x = n29, y = n10))

# ggsave(filename = "results/rejection/sim_Benguela Pelagic/tl_plot_TSS.png", height = 10, width = 15, plot = tl_plot)



dd <- data.frame(a = pred_output$post_dists$a, conn = pred_output$acc_ss)
ggplot(dd) +
  geom_point(aes(x = a, y = conn)) +
  geom_vline(xintercept = -16)


sum(is.na(trop_levels$tl))

dd <- pivot_wider(data = trop_levels, names_from = sp_names, values_from = tl)


TSS_tl <- trop_levels %>%
  group_by(TSS, conn) %>%
  summarise(max_tl = max(tl))

TSS_tl$conn <- round(TSS_tl$conn, 1)

ggplot(TSS_tl) +
  geom_point(aes(x = TSS, y = max_tl, color = as.factor(conn))) +
  theme_bw() +
  scale_color_brewer(palette = "Paired")

TL_diet$TSS <- round(TL_diet_org$TSS, 1)
  
ggplot(TL_diet) +
  geom_point(aes(x = TSS, y = nprey, color = as.factor(round(TL)))) +
  theme_bw() +
  scale_color_brewer(palette = "Paired")



TL_diet$pred <- as.factor(TL_diet$pred)
asd <- pivot_wider(TL_diet, names_from = pred, values_from = nprey)

cbind(TL_diet %>%
  filter(pred == 15) %>%
  mutate(nprey_15 = nprey, TL_15 = TL) %>%
  dplyr::select(-c(TL, nprey, TSS, pred)), 
TL_diet %>%
  filter(pred == 29) %>%
  mutate(nprey_29 = nprey, TL_29 = TL) %>%
  dplyr::select(-c(TL, nprey, pred))) %>%
  filter(TSS <= 0.2, TSS >= 0.15) %>%
  ggplot() +
  geom_point(aes(x = TL_15, y = TL_29)) +
  scale_color_brewer(palette = "Paired") +
  theme_bw()





                   