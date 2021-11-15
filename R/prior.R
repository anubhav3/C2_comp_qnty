## Here, we define the prior distributions for the parameter
# Last updated: 09.03.2021

prior_weibull_x <- function(par = par, no = no){
  local_a <- rweibull(no, shape = par$a.shape, scale = par$a.scale)
  local_ai <- runif(no, par$l_ai, par$r_ai)
  local_aj <- runif(no, par$l_aj, par$r_aj)
  local_r.b <- rweibull(no, shape = par$r.b.shape, scale = par$r.b.scale)
  
  return(data.frame(a=local_a, ai=local_ai, aj=local_aj, r.b=local_r.b))
}

prior_weibull_pbly <- function(par = par, model_par ){
  pbly_a <- dweibull(par$a, shape = model_par$a.shape, scale = model_par$a.scale)
  pbly_ai <- dunif(par$ai, model_par$l_ai, model_par$r_ai)
  pbly_aj <- dunif(par$aj, model_par$l_aj, model_par$r_aj)
  pbly_r.b <- dweibull(par$r.b, shape = model_par$r.b.shape, scale = model_par$r.b.scale)
  
  return(pbly_a*pbly_ai*pbly_aj*pbly_r.b)
}

prior_unif_x <- function(par, no){
  local_a <- runif(no, par$l_log_a, par$r_log_a)
  local_ai <- runif(no, par$l_ai, par$r_ai)
  local_aj <- runif(no, par$l_aj, par$r_aj)
  local_r.b <- runif(no, par$l_log_r.b, par$r_log_r.b)
  
  return(data.frame(a=local_a, ai=local_ai, aj=local_aj, r.b=local_r.b))
}

prior_only_ai <- function(par = par, no = no){
  local_a <- 5
  local_ai <- runif(no, par$l_ai, par$r_ai)
  local_aj <- -0.2
  local_r.b <- 0.02
  
  return(data.frame(a=local_a, ai=local_ai, aj=local_aj, r.b=local_r.b))
}

prior_only_r.b <- function(par_ = par, no = no){
  local_a <- log10(8.56e-6)
  local_ai <- -5.39e-1
  local_aj <- 5.80e-1
  local_r.b <- runif(no, par_$l_log_r.b, par_$r_log_r.b)
  
  return(data.frame(a=local_a, ai=local_ai, aj=local_aj, r.b=local_r.b))
}
prior_only_aj <- function(par = par, no = no){
  local_a <- log10(8.56e-6)
  local_ai <- -5.39e-1
  local_aj <- runif(no, par$l_aj, par$r_aj)
  local_r.b <- log10(5.69e-2)
  
  return(data.frame(a=local_a, ai=local_ai, aj=local_aj, r.b=local_r.b))
}

prior_only_a <- function(par = par, no = no){
  local_a <- runif(no, par$l_a, par$r_a)
  local_ai <- -1
  local_aj <- -0.2
  local_r.b <- 0.02
  
  return(data.frame(a=local_a, ai=local_ai, aj=local_aj, r.b=local_r.b))
}

prior_fix_att <- function(par = par, no = no){
  
  df <- data.frame(a=double(), ai=double(), aj=double(), r.b=double())
  
  for(i in 1:no){
    local_ai <- runif(1, par$l_ai, par$r_ai)
    local_aj <- runif(1, par$l_aj, par$r_aj)
    local_r.b <- rweibull(1, shape = par$r.b.shape, scale = par$r.b.scale)
    
    local_a <- fix_avg_att(ai = local_ai, aj = local_aj)
    df_temp <- data.frame(a=local_a, ai=local_ai, aj=local_aj, r.b=local_r.b)
    df <- rbind(df,df_temp)
  }
  
  return(df)
}


prior_only_a_weib_ai <- function(par = par, no = no){
  local_a <- rweibull(no, shape = par$a.shape, scale = par$a.scale)
  local_ai <- runif(1, par$l_ai, par$r_ai)
  local_aj <- -0.2
  local_r.b <- 0.02
  
  return(data.frame(a=local_a, ai=local_ai, aj=local_aj, r.b=local_r.b))
}

prior_prob_smc <- function(par, i, model_par)
{
  if(i==1){prior_prob <- dunif(par, model_par$l_a, model_par$r_a)}
  else if(i==2){prior_prob <- dunif(par, model_par$l_ai, model_par$r_ai)}
  else if(i==3){prior_prob <- dunif(par, model_par$l_aj, model_par$r_aj)}
  else if(i==4){prior_prob <- dunif(par, model_par$l_r.b, model_par$r_r.b)}
  
  return(prior_prob)
}

prior_prob_joint_smc <- function(par, model_par)
{
  prob_a <- dunif(par[1], model_par$l_log_a, model_par$r_log_a)
  prob_ai <- dunif(par[2], model_par$l_ai, model_par$r_ai)
  prob_aj <- dunif(par[3], model_par$l_aj, model_par$r_aj)
  prob_r.b <- dunif(par[4], model_par$l_log_r.b, model_par$r_log_r.b)
  
  pbly <- prob_a*prob_ai*prob_aj*prob_r.b
  
  return(pbly)
}


prior_log_x <- function(par = par, no = no){
  local_a <- exp(runif(no, min = par$l_log_a, max = par$r_log_a))
  local_ai <- runif(no, par$l_ai, par$r_ai)
  local_aj <- runif(no, par$l_aj, par$r_aj)
  local_r.b <- exp(runif(no, min = par$l_log_r.b, max = par$r_log_r.b))
  
  return(data.frame(a=local_a, ai=local_ai, aj=local_aj, r.b=local_r.b))
}

prior_log_pbly <- function(par = par, model_par = model_par){
  pbly_a <- plog_unif(par$a, min = model_par$l_log_a, max = model_par$r_log_a)
  pbly_ai <- dunif(par$ai, model_par$l_ai, model_par$r_ai)
  pbly_aj <- dunif(par$aj, model_par$l_aj, model_par$r_aj)
  pbly_r.b <- plog_unif(par$r.b, min = model_par$l_log_r.b, max = model_par$r_log_r.b)
  
  return(pbly_a*pbly_ai*pbly_aj*pbly_r.b)
}

prior_pbly_unif <- function(par = par, model_par = model_par){
  pbly_a <- dunif(par$a, model_par$l_log_a, model_par$r_log_a)
  pbly_ai <- dunif(par$ai, model_par$l_ai, model_par$r_ai)
  pbly_aj <- dunif(par$aj, model_par$l_aj, model_par$r_aj)
  pbly_r.b <- dunif(par$r.b, model_par$l_log_r.b, model_par$r_log_r.b)
  
  return(pbly_a*pbly_ai*pbly_aj*pbly_r.b)
}

prior_mcmc_contd <- function(){
  post_dist <- readRDS("results/mcmc/Mill Stream/mN=625000_tol=0.9_TSS_unif_rall/Mill Stream.Rdata")
  N <- 625000
  post_dist <- rbind(post_dist[[1]]$post_dists[N,], post_dist[[2]]$post_dists[N,], post_dist[[3]]$post_dists[N,])
  return(post_dist)
}

# Takes a joint distribution as a prior distribtution
# The joint distirbution in this case is a data frame with each row corresponding to a vector of parameter values
prior_s_r_1 <- function(par, no){
  n_values <- dim(par)[1]
  sample_ind <- sample(1:n_values, size = no, replace = TRUE)
  
  sampled_par <- par[sample_ind,]
  
  return(sampled_par)
}
