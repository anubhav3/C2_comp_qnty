## Other parameters
input_parameters <- function(){
  return(data.frame(tol = 2,
                    N = 1e5,
                    n_cores = 5))
}



epsilon_t <- function(){
  final_tol <- input_parameters()$tol
  return(seq(0.9, final_tol, length = 5))
}

