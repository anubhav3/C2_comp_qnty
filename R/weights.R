# assigns weight to accpted values in ABC

weights <- function(x,tol,kernel)
{
  if(x<=tol){
    if (kernel == "epanechnikov") 
      w <- (3/4)*(1 - (x/tol)^2)
    else if (kernel == "gaussian")
      w <- 1/sqrt(2 * pi) * exp(-0.5 * (x/(tol/2))^2)
    else if (kernel == "triangular") 
      w <- 1 - abs(x/tol)
    else if (kernel == "biweight") 
      w <- (1 - (x/tol)^2)^2
    else if(kernel == "uniform")
      w <- 1
  }
  else(w <- 0)
  
  return(w)
}
