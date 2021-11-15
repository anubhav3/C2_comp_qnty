# proposal distributions for different methods

proposal_dist_pbly <- function(par, par_prime, ADBM_par_sd){
  pbly.a <- dlnorm(par_prime$a, meanlog = par$a, sdlog = ADBM_par_sd$a)
  pbly.ai <- dnorm(par_prime$ai, mean = par$ai, sd = ADBM_par_sd$ai)
  pbly.aj <- dnorm(par_prime$aj, mean = par$aj, sd = ADBM_par_sd$aj)
  pbly.r.b <- dlnorm(par_prime$r.b, meanlog = par$r.b, sdlog = ADBM_par_sd$r.b)
  
  return(pbly.a*pbly.ai*pbly.aj*pbly.r.b)
}

proposal_dist_x <- function(par, ADBM_par_sd){
  a <- rlnorm(1, meanlog = par$a, sdlog = ADBM_par_sd$a)
  ai <- rnorm(1, mean = par$ai, sd = ADBM_par_sd$ai)
  aj <- rnorm(1, mean = par$aj, sd = ADBM_par_sd$aj)
  r.b <- rlnorm(1, meanlog = par$r.b, sdlog = ADBM_par_sd$r.b)
  
  return(data.frame(a=a, ai=ai, aj=aj, r.b=r.b))
}

proposal_dist_pbly_mcmc <- function(par, par_prime, ADBM_par_sd){
  pbly.a <- dnorm(par_prime$a, mean = par$a, sd = ADBM_par_sd$a)
  pbly.ai <- dnorm(par_prime$ai, mean = par$ai, sd = ADBM_par_sd$ai)
  pbly.aj <- dnorm(par_prime$aj, mean = par$aj, sd = ADBM_par_sd$aj)
  pbly.r.b <- dnorm(par_prime$r.b, mean = par$r.b, sd = ADBM_par_sd$r.b)
  
  return(pbly.a*pbly.ai*pbly.aj*pbly.r.b)
}

proposal_dist_x_mcmc <- function(par, ADBM_par_sd){
  a <- rnorm(1, mean = par$a, sd = ADBM_par_sd$a)
  ai <- rnorm(1, mean = par$ai, sd = ADBM_par_sd$ai)
  aj <- rnorm(1, mean = par$aj, sd = ADBM_par_sd$aj)
  r.b <- rnorm(1, mean = par$r.b, sd = ADBM_par_sd$r.b)
  
  return(data.frame(a=a, ai=ai, aj=aj, r.b=r.b))
}
proposal_dist_x_smc <- function(mean_par, sd_par){
  
  prop_par <- as.numeric(4)
  
  prop_par[1] <- rnorm(1, mean = mean_par[1], sd = sd_par[1])
  prop_par[2] <- rnorm(1, mean = mean_par[2], sd = sd_par[2])
  prop_par[3] <- rnorm(1, mean = mean_par[3], sd = sd_par[3])
  prop_par[4] <- rnorm(1, mean = mean_par[4], sd = sd_par[4])
  
  return(prop_par)
}

proposal_prob_smc <- function(par_new, par_old, tau_sq, i)
{
  if(i==1){prob <- dnorm(par_new, mean = par_old, sd = sqrt(tau_sq))}
  else if(i==2){prob <- dnorm(par_new, mean = par_old, sd = sqrt(tau_sq))}
  else if(i==3){prob <- dnorm(par_new, mean = par_old, sd = sqrt(tau_sq))}
  else if(i==4){prob <- dnorm(par_new, mean = par_old, sd = sqrt(tau_sq))}
  
  return(prob)
}

proposal_dist_joint_x_smc <- function(j_mean, j_sigma){
  pbl_x <- mvtnorm::rmvnorm(1, mean = j_mean, sigma = j_sigma)
  
  return(pbl_x)
}

proposal_prob_joint_smc <- function(x, j_mean, j_sigma){
  pbl <- mvtnorm::dmvnorm(x = x, mean = j_mean, sigma = j_sigma)
  
  return(pbl)
}

proposal_dist_pbly_unif_mcmc <- function(par, par_prime, ADBM_par_sd){
  pbly.a <- dnorm(par_prime$a, mean = par$a, sd = ADBM_par_sd$a)
  pbly.ai <- dnorm(par_prime$ai, mean = par$ai, sd = ADBM_par_sd$ai)
  pbly.aj <- dnorm(par_prime$aj, mean = par$aj, sd = ADBM_par_sd$aj)
  pbly.r.b <- dnorm(par_prime$r.b, mean = par$r.b, sd = ADBM_par_sd$r.b)
  
  return(pbly.a*pbly.ai*pbly.aj*pbly.r.b)
}

proposal_dist_x_unif_mcmc <- function(par, ADBM_par_sd){
  a <- rnorm(1, mean = par$a, sd = ADBM_par_sd$a)
  ai <- rnorm(1, mean = par$ai, sd = ADBM_par_sd$ai)
  aj <- rnorm(1, mean = par$aj, sd = ADBM_par_sd$aj)
  r.b <- rnorm(1, mean = par$r.b, sd = ADBM_par_sd$r.b)
  
  return(data.frame(a=a, ai=ai, aj=aj, r.b=r.b))
}