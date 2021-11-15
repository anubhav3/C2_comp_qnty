# used to calculate the tolerance for a given foodweb 

tolerance_seln <- function(dirnam, fw_name){
  
  file <- paste(c(dirnam,"/",fw_name,".Rdata"), collapse = "")
  fw_data <- readRDS(file)
  distance <- fw_data$dist
  
  sort_dist <- sort(unique(distance))  
  nsim <- length(unique(distance))
  index <- as.integer(nsim*0.001)
  tol <- sort_dist[index]
  
  return(tol)
}
