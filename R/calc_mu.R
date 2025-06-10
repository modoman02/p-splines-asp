calc_mu <- function(x, y, lambda, beta_hat, ) { # estimate next mu using LRLS
  mu <- t(x) %*% beta_hat
  return(mu)
}
