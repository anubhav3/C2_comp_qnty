thinning <- function(posterior, n_lag){
  t_length <- dim(posterior)[1]
  n_take <- as.integer(t_length/n_lag)
  thin_df <- posterior[1,]
  for(i in 2:n_take){
    thin_df[i,] <- posterior[n_lag*i,]
  }
  return(thin_df)
}