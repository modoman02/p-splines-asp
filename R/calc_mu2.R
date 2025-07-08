#' Estimate Mean Function
#'
#' Updates the mean curve using penalized IRLS.
#'
#' @param X Design matrix for the mean.
#' @param y Response vector.
#' @param K_mu Penalty matrix for the mean.
#' @param mu_init Initial estimates of the mean.
#' @param sigma_hat Current variance estimates.
#' @param lambda_grid Grid of smoothing parameters to test.
#' @param lambda_mu Starting lambda value.
#' @param max_iterations_mu Maximum IRLS iterations.
#' @param tolerance Convergence threshold.
#'
#' @return List with updated mean estimates and diagnostics.
#' @export

calc_mu2 <- function(X, y, K_mu, mu_init, sigma_hat, lambda_grid, lambda_mu = 1, max_iterations_mu, tolerance) { # estimate next mu using Fisher Updates
  # lambda_grid = grid to choose optimal lambda from, lambda_mu = initial lambda value, thats used to approximate optimal mu in the IRLS algorithm

  n <- length(y)
  mu_hat <- matrix(NA, nrow = n, ncol = max_iterations_mu + 1)
  mu_hat[,1] <- mu_init
  GD_mat <- numeric(max_iterations_mu + 2)
  for (i in 1:max_iterations_mu) {
    sigma_hat <- as.vector(sigma_hat)
    # calc score
    residuals <- y - mu_hat[,i]
    u_mu <- residuals / sigma_hat^2
    W_mu <- diag(1 / sigma_hat^2, nrow = n, ncol = n)

    # calc pseudo response z
    z_mu <- mu_hat[,i] + solve(W_mu, u_mu)

    # update mu
    beta_hat <- solve(t(X) %*% W_mu %*% X + lambda_mu * K_mu) %*% t(X) %*% W_mu %*% z_mu  # lambda fehlt noch
    mu_hat[,i+1] <- X %*% beta_hat

    # check convergence
    GD_mat[i+1] <- calc_deviance(y = y, mu_hat = mu_hat[,i+1], sigma_hat = sigma_hat)   # gucken, wie man das wegen Rundungen etc. regelt
    if (i > 1 && abs(GD_mat[i+1] - GD_mat[i]) < tolerance) {
      break
    }
  }

  # approximate best lambda
  # score <- numeric(length(lambda_grid))
  # for (j in seq_along(lambda_grid)) {
  #   lambda <- lambda_grid[j]
  #   beta_hat_lambda <- solve(t(X) %*% W_mu %*% X + lambda * K_mu) %*% t(X) %*% W_mu %*% z_mu # using W and z from the last IRLS Iteration
  #   mu_lambda <- X %*% beta_hat_lambda
  #   S_lambda <- X %*% solve(t(X) %*% W_mu %*% X + lambda * K_mu) %*% t(X) %*% W_mu
  #   residuals_lambda <- z_mu - mu_lambda
  #   score[j] <- (mean(residuals_lambda^2)) / (1 - mean(diag(S_lambda)))^2
  # }
  # lambda_hat = lambda_grid[which.min(score)]
  #
  # # one last IRLS Iteration, using the optimal lambda from the grid, to get the optimal mu_hat
  # beta_hat_optimal <- solve(t(X) %*% W_mu %*% X + lambda_hat * K_mu) %*% t(X) %*% W_mu %*% z_mu
  # mu_hat_optimal <- X %*% beta_hat_optimal


  return(list(mu_new = mu_hat_optimal,
              mu_mat = mu_hat[,1:(i+1)],
              GD_mu = GD_mat[1:(i+1)],
              #lambda_hat = lambda_hat,
              gcv_score = score))
}
