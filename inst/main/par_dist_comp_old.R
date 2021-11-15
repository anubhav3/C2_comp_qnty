# Used to compare predicted parameter values of posterior distribution from different ABC methods

library(HDInterval)
library(ggplot2)

foodweb <- "Benguela Pelagic"
rej_data <- readRDS("results/rejection/Benguela Pelagic/rN=1000_tol=0.6_TSS_unif_rall_WBF_1/Benguela Pelagic.Rdata")
mcmc_data <- readRDS("results/mcmc/Benguela Pelagic/mN=1e+05_tol=0.6_TSS_unif_rall_WBF/Benguela Pelagic.Rdata")
pnas_data <- data.frame(a = log10(0.0189), ai = -0.491, aj = -0.465, r.b = log10(0.0401))

rej_post <- rej_data$post_dists
mcmc_post <- mcmc_data[[1]]$post_dists
after_burn_mcmc_post <- after_burn(mcmc_post)
after_thin <- thinning(after_burn_mcmc_post, 140)
  
rej_par <- hdi(rej_post)
mcmc_par <- hdi(after_thin)


df <- data.frame(x0 = as.double(rej_par[1,]), x1 = as.double(rej_par[2,]), y0 = c(1,4,7,10), y1 = c(1,4,7,10), Method = "Rejection", par = names(rej_par[1,]))

df <- rbind(df, data.frame(x0 = as.double(mcmc_par[1,]), x1 = as.double(mcmc_par[2,]), y0 = c(2,5,8,11), y1 = c(2,5,8,11), Method = "MCMC", par = names(rej_par[1,])))

df <- rbind(df, data.frame(x0 = as.double(pnas_data), x1 = as.double(pnas_data), y0 = c(3,6,9,12), y1 = c(3,6,9,12), Method = "PNAS 2008", par = names(rej_par[1,])))

df1 <- data.frame(x0 = as.double(pnas_data), x1 = as.double(pnas_data), y0 = c(1.5, 4.5, 7.5, 10.5), y1 = c(1.5, 4.5, 7.5, 10.5), Method = "PNAS 2008", par = names(rej_par[1,]))

fname <- paste("results/comparison/", foodweb, "_par.png", sep = "")
png(filename = fname, height = 1000, width = 1200, pointsize = 30)
cols = c("Rejection" = "red", "MCMC" = "blue", "PNAS 2008" = "green")
labes = c("Rejection" = "Rejection", "MCMC" = "MCMC", "PNAS 2008" = "Point estimates from Petchey et al (2008)")

ggplot() +
  geom_segment(data = df, aes(x = x0, xend = x1, y = y0, yend = y1, color = Method), size = 5) +
  geom_point(data = df1, aes(x = x0, y = y0, color = Method), size = 10) +
  theme_classic() + 
  # annotate("text", x = -1, y = 1.5, label = 'log[10](a)', parse = TRUE, size = 10,family="Comic Sans MS") +
  # annotate("text", x = 0.75, y = 4.5, label = 'a[i]', parse = TRUE, size = 10,family="Comic Sans MS") +
  # annotate("text", x = -1, y = 7.5, label = 'a[j]', parse = TRUE, size = 10,family="Comic Sans MS") +
  # annotate("text", x = 0, y = 10.5, label = 'log[10](b)', parse = TRUE, size = 10,family="Comic Sans MS") +
  xlab("Range") +
  ylab("Parameters") +
  theme(text = element_text(size=40, family="Comic Sans MS"), axis.line = element_line(colour = 'black', size = 2),
        plot.margin = margin(2, 2, 2, 2, "cm")) +
  scale_color_manual(values = cols, labels = str_wrap(labes, 20))+
  labs(title = str_wrap("Estimates of model parameters for Benguela Pelagic food web", 35))+
  theme(plot.title = element_text(size = 50, hjust = 0.5)) +
  # theme(axis.text.y = element_blank()) +
  scale_y_continuous(breaks = c(1.5, 4.5, 7.5, 10.5),
                     labels = c(TeX('$log_{10}a$'), TeX('$a_i$'), TeX('$a_j$'), TeX('$log_{10}b$')))

dev.off()


