# 2022.04.29
# We fit the link accumulation curve (to a Type II functional response curve)


links_ngut_bs <- readRDS("../../../../../Volumes/SSD1/Secondary_storage/PhD_local/C2_files/Broadstone Stream size_agg_v2/rule_ind_predator/empirical_links_ngut.Rdata")

df <- links_ngut_bs %>%
  select(c("ngut", "mean_links")) %>%
  mutate("emp_links" = mean_links) %>%
  select(-"mean_links")

lac_func <- function(x, a, b){
  y <- a*x/(b + x)
  return(y)
}

dist_fun <- function(x, par){
  pred_val <- lac_func(x = x, a = par[1], b = par[2])
  return(sum(abs((df$emp_links - pred_val))))
}

a_prior <- seq(100, 200, length = 1001)
b_prior <- seq(50, 150, length = 1001)

ab_prior <- expand.grid(a = a_prior, b = b_prior)
n_par <- dim(ab_prior)[1]
dist_all <- numeric(n_par)

for(i in 1:n_par){
  ab_row <- ab_prior[i,]
  fit_val <- lac_func(x = df$mean_links, a = ab_row$a, b = ab_row$b)
  dist_all[i] <- sum((abs(fit_val - df$emp_links)))
}  


## Using Optim function
par_pred[1] <- 400
res <- optim(par = par_pred, fn = dist_fun, x = df$ngut)


min_ind <- which.min(dist_all)

par_pred <- ab_prior[min_ind,]

par_pred_opt <- res$par

pred_links <- lac_func(x = df$emp_links, a = par_pred_opt[1], b = par_pred_opt[2])


df_with_pred <- df %>%
  mutate(pred_links = pred_links)

df_with_pred_longer <- df_with_pred %>% 
  pivot_longer(!ngut, names_to = "links_type", values_to = "n_links")


ggplot(df_with_pred_longer) +
  geom_point(aes(x = ngut, y = n_links, color = links_type))
