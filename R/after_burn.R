after_burn <- function(posterior){
  lt <- dim(posterior)[1]
  return(posterior[(lt/2):lt,])
}