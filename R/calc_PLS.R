calc_PLS <- function (y, Z, K, lambda) {  # obsolete
  gamma_hat <- solve(t(Z) %*% Z + lambda * K) %*% t(Z) %*% y
  return(gamma_hat)
}
