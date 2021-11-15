trophic_position <- function(web, title){
  
  standard_web <- mat.to.comm(pred.mat = web, fw_title = title)
  tl <- PreyAveragedTrophicLevel(standard_web)
  
  return(tl)
}

stable_isotopes_ratio <- function(trophic_level, d_TEF, d_ref){
  
  d_c = d_ref + trophic_level * d_TEF - d_TEF
  
  return(d_c)
}



sir_from_Tl <- function(TL, trophic_frac, sir_base_mean, sir_base_error){
  
  sir_base <- rnorm(n = 1, mean = sir_base_mean, sd = sir_base_error)
  
  # 1 is the trophic level of basal species
  sir <- trophic_frac * (TL-1) + sir_base
  
  return(sir)
}

TL_from_sir <- function(sir, trophic_frac, sir_base){
  
  TL <- (1/trophic_frac)*(sir - sir_base) + 1
  
  return(TL)
  
}
