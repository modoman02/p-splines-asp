update_parameters <- function (y, X, Z, lambda_mu, lambda_sigma, tolerance,
                               max_iterations, K_mu, K_sigma, max_iterations_mu = 10, max_iterations_sigma = 10) {  # updates mu and sigma respectively, each time with an updated value of the other parameter
  n <- length(y)
  mu_hat <- matrix(0, nrow = n, ncol = max_iterations)
  sigma_hat <- matrix(0, nrow = n, ncol = max_iterations)
  init_vals <- get_initial_values(X = X, Z = Z, y = y)
  GD_mat <- matrix(0, nrow = 1, ncol = max_iterations)
  mu_hat[,1] <- init_vals$mu_hat
  sigma_hat[,1] <- init_vals$sigma_hat

  for (i in 1:max_iterations) {
  # iterative updates for sigma and mu respectively
    calc_mu()
  }
  # for (i in 1:max_iterations_mu) {
  #   #  calc score
  #   residuals <- y - mu_hat[,i]
  #   u_mu <- residuals / sigma_hat[,i]^2
  #   W_mu <- diag(1 / sigma_hat[,i]^2, nrow = n, ncol = n)
  #   z_mu <- mu[,i] + u_mu / diag(W_mu)
  #
  #   # update mu
  #   beta_hat <- solve(t(X) %*% W_mu %*% X + lambda_mu * K_mu) %*% t(X) %*% W_mu %*% z_mu  # lambda fehlt noch
  #   mu_new <- X %*% beta_hat
  #
  #   mu_hat[,i+1] <- mu_new
  #   iterations <- iterations + 1
  #   # check convergence
  #   GD_mat[i+1] <- calc_deviance(y = y, mu_hat = mu_new, sigma_hat = sigma_new)   # gucken, wie man das wegen Rundungen etd. regelt
  #   if (GD_mat[i] < tolerance) {
  #     break
  #   }
  #   else if (iterations == max_iterations) {
  #     break
  #   }
  # }
  return(list(mu_hat = mu_new,
              sigma_hat = sigma_new,
              iterations = iterations,
              mu_mat = mu_mat,
              sigma_mat = sigma_mat,
              GD_mat = GD_mat))
}
