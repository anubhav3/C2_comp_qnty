space_clearance_rate <- function(M, Mj, a, ai, aj){
  scr <- a*M^ai*Mj^aj
  return(scr)
}