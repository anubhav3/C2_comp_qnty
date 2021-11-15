## ai
dist_val <- readRDS("results/rejection/Benguela Pelagic/N=1000_tol=1_TSS_unif_ai/Benguela Pelagic.Rdata")
post_distr <- dist_val$post_dists

prop_val <- readRDS("results/rejection/Benguela Pelagic/N=1000_tol=1_TSS_unif_ai/properties.Rdata")
prop_val <- prop_val$prop

avg_att <- log(foodweb_mat(post_distr, all.web.info))

plot(post_distr$ai, prop_val$connectance)

dd <- data.frame(par = post_distr$ai, conn = prop_val$connectance, avg_att = avg_att)

ggplot(dd) +
  geom_point(aes(x=par, y = conn, color = avg_att))

## aj
dist_val <- readRDS("results/rejection/Benguela Pelagic/N=1000_tol=1_TSS_unif_aj/Benguela Pelagic.Rdata")
post_distr <- dist_val$post_dists

prop_val <- readRDS("results/rejection/Benguela Pelagic/N=1000_tol=1_TSS_unif_aj/properties.Rdata")
prop_val <- prop_val$prop

plot(post_distr$aj,prop_val$connectance)

## a
dist_val <- readRDS("results/rejection/Benguela Pelagic/N=1000_tol=1_TSS_unif_a/Benguela Pelagic.Rdata")
post_distr <- dist_val$post_dists

prop_val <- readRDS("results/rejection/Benguela Pelagic/N=1000_tol=1_TSS_unif_a/properties.Rdata")
prop_val <- prop_val$prop

avg_att <- foodweb_mat(post_distr, all.web.info)

plot(prop_val$connectance, post_distr$a)

## fix avg attackrate
dist_val <- readRDS("results/rejection/Benguela Pelagic/N=1000_tol=1_TSS_fix_att/Benguela Pelagic.Rdata")
post_distr <- dist_val$post_dists

prop_val <- readRDS("results/rejection/Benguela Pelagic/N=1000_tol=1_TSS_fix_att/properties.Rdata")
prop_val <- prop_val$prop

avg_att <- log(foodweb_mat(post_distr, all.web.info))

plot(post_distr$ai, prop_val$connectance)


#connectance plot
library(ggplot2)

## ai
dist_val <- readRDS("results/rejection/Benguela Pelagic/N=1000_tol=2_TSS_unif_ai/Benguela Pelagic.Rdata")
post_distr <- dist_val$post_dists

prop_val_big <- readRDS("results/rejection/Benguela Pelagic/N=1000_tol=2_TSS_unif_ai/properties.Rdata")
prop_val <- prop_val_big$prop

avg_att <- log(foodweb_mat(post_distr, all.web.info))

plot(post_distr$ai, prop_val$connectance)

dd <- data.frame(par = post_distr$ai, conn = prop_val$connectance, avg_att = avg_att, max_trop_lvl = prop_val$max_trop_lvl)


pdf("results/rejection/Benguela Pelagic/N=1000_tol=2_TSS_unif_ai/conn_vs_ai.pdf")

ggplot(dd) +
  geom_point(aes(x=par, y = conn, color = avg_att))

dev.off()

pdf("results/rejection/Benguela Pelagic/N=1000_tol=2_TSS_unif_ai/max_trop_lvl_vs_ai.pdf")

ggplot(dd) +
  geom_point(aes(x=par, y = max_trop_lvl, color = avg_att))

dev.off()


pdf("results/rejection/Benguela Pelagic/N=1000_tol=2_TSS_unif_ai/avg_att_vs_ai.pdf")

ggplot(dd) +
  geom_point(aes(x=par, y = avg_att, color = conn))

dev.off()




## fix avg att
dist_val <- readRDS("results/rejection/Benguela Pelagic/N=1000_tol=2_TSS_fix_att_ai/Benguela Pelagic.Rdata")
post_distr <- dist_val$post_dists

prop_val_big <- readRDS("results/rejection/Benguela Pelagic/N=1000_tol=2_TSS_fix_att_ai/properties.Rdata")
prop_val <- prop_val_big$prop

avg_att <- log(foodweb_mat(post_distr, all.web.info))

plot(post_distr$ai, prop_val$connectance)

dd <- data.frame(ai = post_distr$ai, conn = prop_val$connectance, avg_att = avg_att, max_trop_lvl = prop_val$max_trop_lvl)


pdf("results/rejection/Benguela Pelagic/N=1000_tol=2_TSS_fix_att_ai/conn_vs_ai.pdf")

ggplot(dd) +
  geom_point(aes(x=ai, y = conn, color = avg_att))

dev.off()

pdf("results/rejection/Benguela Pelagic/N=1000_tol=2_TSS_fix_att_ai/max_trop_lvl_vs_ai.pdf")

ggplot(dd) +
  geom_point(aes(x=ai, y = max_trop_lvl, color = avg_att))

dev.off()


pdf("results/rejection/Benguela Pelagic/N=1000_tol=2_TSS_fix_att_ai/avg_att_vs_ai.pdf")

ggplot(dd) +
  geom_point(aes(x=ai, y = avg_att, color = conn))

dev.off()





## a vs ai
dist_val <- readRDS("results/rejection/Benguela Pelagic/N=1000_tol=2_TSS_only_a_weib_ai/Benguela Pelagic.Rdata")
post_distr <- dist_val$post_dists

prop_val_big <- readRDS("results/rejection/Benguela Pelagic/N=1000_tol=2_TSS_only_a_weib_ai/properties.Rdata")
prop_val <- prop_val_big$prop

avg_att <- log(foodweb_mat(post_distr, all.web.info))

plot(post_distr$ai, prop_val$connectance)

dd <- data.frame(ai = post_distr$ai, conn = prop_val$connectance, avg_att = avg_att, max_trop_lvl = prop_val$max_trop_lvl, a = post_distr$a)


pdf("results/rejection/Benguela Pelagic/N=1000_tol=2_TSS_only_a_weib_ai/conn_vs_ai.pdf")

ggplot(dd) +
  geom_point(aes(x=ai, y = conn, color = avg_att))

dev.off()

pdf("results/rejection/Benguela Pelagic/N=1000_tol=2_TSS_only_a_weib_ai/max_trop_lvl_vs_ai.pdf")

ggplot(dd) +
  geom_point(aes(x=ai, y = max_trop_lvl, color = avg_att))

dev.off()


pdf("results/rejection/Benguela Pelagic/N=1000_tol=2_TSS_only_a_weib_ai/avg_att_vs_ai.pdf")

ggplot(dd) +
  geom_point(aes(x=ai, y = avg_att, color = conn))

dev.off()

pdf("results/rejection/Benguela Pelagic/N=1000_tol=2_TSS_only_a_weib_ai/a_vs_ai.pdf")

ggplot(dd) +
  geom_point(aes(x=ai, y = a, color = conn))

dev.off()




#Varying only a

dist_val <- readRDS("results/rejection/Benguela Pelagic/N=100_tol=2_TSS_only_a/Benguela Pelagic.Rdata")
post_distr <- dist_val$post_dists

prop_val_big <- readRDS("results/rejection/Benguela Pelagic/N=100_tol=2_TSS_only_a/properties.Rdata")
prop_val <- prop_val_big$prop

avg_att <- foodweb_mat(post_distr, all.web.info)

plot(post_distr$a, prop_val$connectance)

dd <- data.frame(par = post_distr$a, conn = prop_val$connectance, avg_att = avg_att, max_trop_lvl = prop_val$max_trop_lvl)


pdf("results/rejection/Benguela Pelagic/N=100_tol=2_TSS_only_a/conn_vs_a.pdf")

ggplot(dd) +
  geom_point(aes(x=par, y = conn, color = avg_att))

dev.off()

pdf("results/rejection/Benguela Pelagic/N=100_tol=2_TSS_only_a/max_trop_lvl_vs_a.pdf")

ggplot(dd) +
  geom_point(aes(x=par, y = max_trop_lvl, color = avg_att))

dev.off()


pdf("results/rejection/Benguela Pelagic/N=100_tol=2_TSS_only_a/avg_att_vs_a.pdf")

ggplot(dd) +
  geom_point(aes(x=par, y = avg_att, color = conn))

dev.off()



## MCMC traceplot
dist_val <- readRDS("results/mcmc/Benguela Pelagic/mN=1e+05_tol=0.01_connectance_log_sd_1/Benguela Pelagic.Rdata")
post_distr <- dist_val[[1]]$post_dists

pdf("results/mcmc/Benguela Pelagic/mN=1e+05_tol=0.01_connectance_log_sd_1/traceplot_distn.pdf")
traceplot(dist_val, 4)

dev.off()

prop_val_big <- readRDS("results/mcmc/Benguela Pelagic/mN=1e+05_tol=0.01_connectance_log_sd_1/properties.Rdata")
prop_val <- prop_val_big$prop



