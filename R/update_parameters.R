update_parameters <- function (x, y, X, Z, init_mu, init_sigma, lambda_mu, lambda_sigma, tolerance, K_mu, K_sigma) {  # Wrapper, that calls calc_mu() and calc_sigma() iteratively und updates them while doing so. Also calcs residuals and beta after every iteration
  mu_old <- init_mu
  sigma_old <- init_sigma
  for (i in 1:max) {
    # update mu
    beta_hat <- solve(t(X) %*% K_mu %*% X) %*% t(X) %*% K_mu %*% y
    mu_new <- calc_mu(Z, beta_hat)
    residuals <- y - mu_new
    s_hat <- log(abs(residuals)) + 0.635  # adding an additional very small number can prevent errors from happening in case we get a perfect fit (residuals = 0)
    # update sigma
    gamma_hat <- solve(t(Z) %*% Z) %*% t(Z) %*% s_hat
    sigma_new <- calc_sigma(X, gamma_hat)
    # check convergence
    if (global_deviance() < tolerance) {
      break
    }
  }

}
