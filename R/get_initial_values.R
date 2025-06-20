get_initial_values <- function(X, Z, y) { # calcs OLS Estimator for beta and form that the initial mu and sigma and returns sigma_hat and mu_hat, so the update parameters() function can use them as initial values
  beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y  # fit initial values with ordinary OLS
  mu_hat <- X %*% beta_hat
  residuals <- y - mu_hat
  s_hat <- log(abs(residuals)) + 0.635
  gamma_hat <- solve(t(Z) %*% Z) %*% t(Z) %*% s_hat
  sigma_hat <- exp(Z %*% gamma_hat)
  W_hat <- matrix(0, nrow = length(y), ncol = length(y))
  for (i in 1:length(y)) {
    W_hat[i,i] <- 1/sigma_hat[i]
  }
  beta_hat <- solve(t(X) %*% W_hat %*% X) %*% t(X) %*% W_hat %*% y
  mu_hat <- X %*% beta_hat
  return(list(mu_hat = mu_hat, sigma_hat = sigma_hat))
}
