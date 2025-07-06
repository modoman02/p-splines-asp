update_parameters <- function (X, Z, y, max_iterations, K_mu, K_sigma, max_iterations_mu = 15,
                               max_iterations_sigma = 15, tolerance = sqrt(.Machine$double.eps), from_mu = 0.5, to_mu, stepsize_mu,
                               from_sigma = 0.5, to_sigma, stepsize_sigma, lambda_init_mu = 1, lambda_init_sigma = 1) {  # updates mu and sigma respectively, each time with an updated value of the other parameter
  n <- length(y)
  init_vals <- get_initial_values(X = X, Z = Z, y = y)
  mu_hat <- matrix(NA, nrow = n, ncol = max_iterations+1)
  sigma_hat <- matrix(NA, nrow = n, ncol = max_iterations+1)
  GD_mat <- matrix(NA, nrow = n, ncol = (max_iterations+1))
  mu_hat[,1] <- init_vals$mu_hat
  sigma_hat[,1] <- init_vals$sigma_hat
  GD_mat[1] <- calc_deviance(y, mu_hat = mu_hat[,1], sigma_hat = sigma_hat[,1])
  lambda_grid_mu <- seq(from_mu, to_mu, by = stepsize_mu)
  lambda_grid_sigma <- seq(from_sigma, to_sigma, by = stepsize_sigma)
  lambda_mu_seq <- numeric(max_iterations)
  lambda_sigma_seq <- numeric(max_iterations)

  for (i in 1:max_iterations) {
  # iterative updates for sigma and mu respectively
    mu_result <- calc_mu(X = X, y = y, K_mu = K_mu, mu_init = mu_hat[, i], sigma_hat = sigma_hat[, i],
                         lambda_mu = lambda_init_mu, lambda_grid = lambda_grid_mu, max_iterations_mu = max_iterations_mu, tolerance = tolerance)
    mu_hat[, i + 1] <- mu_result$mu_new
    GD_mat_mu <- mu_result$GD_mu
    lambda_mu_seq[i] <- mu_result$lambda_opt
    sigma_result <- calc_sigma(Z = Z, y = y, K_sigma = K_sigma, sigma_init = sigma_hat[, i], mu_hat = mu_hat[, i + 1],
                               lambda_sigma = lambda_init_sigma, lambda_grid = lambda_grid_sigma, max_iterations_sigma = max_iterations_sigma, tolerance = tolerance)
    sigma_hat[, i + 1] <- sigma_result$sigma_new
    GD_mat_sigma <- sigma_result$GD_sigma
    lambda_sigma_seq[i] <- sigma_result$lambda_opt
    GD_mat[i + 1] <- calc_deviance(y, mu_hat[, i + 1], sigma_hat[, i + 1])
    if (abs(GD_mat[i + 1] - GD_mat[i]) < tolerance) {
      break
    }
  }
  return(list(mu_hat = mu_hat[, i+1],
              sigma_hat = sigma_hat[, i+1],
              mu_mat = mu_hat,
              sigma_mat = sigma_hat,
              GD_mat = GD_mat,
              GD_mat_mu = GD_mat_mu,
              GD_mat_sigma = GD_mat_sigma,
              lambda_mu = lambda_mu_seq[i],
              lambda_sigma = lambda_sigma_seq[i],
              lambda_mu_seq = lambda_mu_seq[1:i],
              lambda_sigma_seq = lambda_sigma_seq[1:i],
              iterations = i))
}
