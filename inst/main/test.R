rm(list=ls())
library(ggplot2)

bp_mcmc <- readRDS("results/mcmc/Benguela Pelagic/mN=258000_tol=0.7_TSS_unif_rall/Benguela Pelagic.Rdata")
bp_1 <- bp_mcmc[[1]]

x <- 1:1000
dd <- data.frame(x = x, y = bp$a[1:1000])
ggplot(dd) +
  geom_line(aes(x = x, y = y))


bp_1 <- readRDS("results/mcmc/Benguela Pelagic/mN=10000_tol=0.7_TSS_a_1/Benguela Pelagic.Rdata")[[1]]
bp_5 <- readRDS("results/mcmc/Broadstone Stream/")[[1]]


bp_thin_1 <- readRDS("results/mcmc/Benguela Pelagic/mN=10000_tol=0.7_TSS_a_1/Benguela Pelagic_thin.Rdata")
bp_thin_5 <- readRDS("results/mcmc/Broadstone Stream/mN=10000_tol=0.7_TSS_a_5/Broadstone Stream_thin.Rdata")


br <- readRDS("results/mcmc/Broom/mN=10000_tol=1_TSS_a_5/Broom_thin.Rdata")


main_func <- function(n){
  
  test_func <- function(a,b){
    temp <- n+a+b
    return(temp)
  }
  
  return(test_func)
}
