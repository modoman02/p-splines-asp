calc_PLS <- function (y, Z, K, lambda) {  # identical for both tp and tp = FALSE
  gamma_hat <- solve(t(Z) %*% Z + lambda * K) %*% t(Z) %*% y
  return(gamma_hat)
}
