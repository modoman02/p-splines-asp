update_parameters <- function (x, y, X, Z, lambda_mu, lambda_sigma, tolerance, max_iterations, K_mu, K_sigma) {  # Wrapper, that calls calc_mu() and calc_sigma() iteratively und updates them while doing so. Also calcs residuals and beta after every iteration
  # initial values for sigma and mu, so the Algorithm can use them as starting points
  mu_mat <- matrix(0, nrow = length(y), ncol = max_iterations)
  sigma_mat <- matrix(0, nrow = length(y), ncol = max_iterations)
  GD_mat <- matrix(0, nrow = 1, ncol = max_iterations)
  for (i in 1:length(y)) {
    W_hat[i,i] <- 1/sigma_hat[i]
  }
  beta_hat <- solve(t(X) %*% W_hat %*% X) %*% t(X) %*% W_hat %*% y
  iterations <- 0
  for (i in 1:max_iterations) {
    # update mu
    beta_hat <- solve(t(X) %*% K_mu %*% X) %*% t(X) %*% K_mu %*% y  # Gewichtung der K-Matrix über lambda fehlt noch
    mu_new <- calc_mu(Z, beta_hat)
    residuals <- y - mu_new
    s_hat <- log(abs(residuals)) + 0.635  # adding an additional very small number can prevent errors from happening in case we get a perfect fit (residuals = 0)
    mu_mat[, i] <- mu_new
    # update sigma
    gamma_hat <- solve(t(Z) %*% Z) %*% t(Z) %*% s_hat
    sigma_new <- calc_sigma(X, gamma_hat)
    sigma_mat[, i] <- sigma_new


    iterations <- iterations + 1
    # check convergence
    GD_mat[i] <- calc_deviance(y = y, mu_hat = mu_new, sigma_hat = sigma_new)
    if (GD_mat[i] < tolerance) {
      break
    }
    else if (iterations == max_iterations) {
      break
    }
  }
  return(list(mu_hat = mu_new,
              sigma_hat = sigma_new,
              iterations = iterations,
              mu_mat = mu_mat,
              sigma_mat = sigma_mat,
              GD_mat = GD_mat))
}
