# matrices have to be itntialized globally
mu_hat <- matrix()
sigma_hat <- matrix()
GD_mat <- matrix()

update_parameters <- function (y, X, Z, lambda_mu, lambda_sigma, tolerance,
                               max_iterations, K_mu, K_sigma, max_iterations_mu = 15, max_iterations_sigma = 15) {  # updates mu and sigma respectively, each time with an updated value of the other parameter
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
    calc_sigma()
  }
  return(list(mu_hat = mu_new,
              sigma_hat = sigma_new,
              iterations = iterations,
              mu_mat = mu_mat,
              sigma_mat = sigma_mat,
              GD_mat = GD_mat))
}
