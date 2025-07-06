#' Estimate Variance Function
#'
#' Updates the variance curve using penalized IRLS on the log scale.
#'
#' @param Z Design matrix for the variance.
#' @param y Response vector.
#' @param K_sigma Penalty matrix for the variance.
#' @param sigma_init Initial sigma estimates.
#' @param mu_hat Current mean estimates.
#' @param lambda_grid Grid of smoothing parameters to test.
#' @param lambda_sigma Starting lambda value.
#' @param max_iterations_sigma Maximum IRLS iterations.
#' @param tolerance Convergence threshold.
#'
#' @return List with updated variance estimates and diagnostics.
#' @export

calc_sigma <- function (Z, y, K_sigma, sigma_init, mu_hat, lambda_grid, lambda_sigma = 1, max_iterations_sigma, tolerance) { # calc next sigma using Fisher Updates
  n <- length(y)
  epsilon <- 1e-6
  sigma_hat <- matrix(NA, nrow = n, ncol = max_iterations_sigma + 2)
  sigma_hat[, 1] <- sigma_init
  GD_mat <- numeric(max_iterations_sigma + 2)
  for (i in 1:max_iterations_sigma) {
    # calc score
    u_sigma <- ((y-mu_hat)^2-sigma_hat[,i]^2)/sigma_hat[,i]^2
    w_sigma <- as.numeric(u_sigma^2)
    z_sigma <- as.matrix((log(sigma_hat[,i]) + u_sigma^(-1)), ncol = 1)
    W_sigma <- w_sigma * diag(length(w_sigma))

    # update sigma
    gamma_hat <- solve(t(Z) %*% W_sigma %*% Z + lambda_sigma * K_sigma) %*% t(Z) %*% W_sigma %*% z_sigma
    sigma_hat[,i + 1] <- exp(Z %*% gamma_hat)
    sigma_hat[,i + 1][is.na(sigma_hat[,i + 1]) | sigma_hat[,i + 1] < epsilon] <- epsilon
    # check convergence
    GD_mat[i+1] <- calc_deviance(y = y, mu_hat = mu_hat, sigma_hat = sigma_hat[,i+1])   # gucken, wie man das wegen Rundungen etc. regelt
    if (i > 1 && abs(GD_mat[i+1] - GD_mat[i]) < tolerance) {
      break
    }
  }

  # approximate best lambda
  score <- numeric(length(lambda_grid))
  for (j in 1:length(lambda_grid)) {
    lambda <- lambda_grid[j]
    gamma_hat_lambda <- solve(t(Z) %*% W_sigma %*% Z + lambda * K_sigma) %*% t(Z) %*% W_sigma %*% z_sigma
    sigma_hat_lambda <- exp(Z %*% gamma_hat_lambda)
    W_sigma_lambda <- diag(as.numeric(1 / sigma_hat_lambda^2), nrow = n)
    S_lambda <- Z %*% solve(t(Z) %*% W_sigma_lambda %*% Z + lambda * K_sigma) %*% t(Z) %*% W_sigma
    residuals_lambda <- z_sigma - log(sigma_hat_lambda)
    score[j] <- mean(residuals_lambda^2) / (1 - mean(diag(S_lambda)))^2
  }
  lambda_hat <- lambda_grid[which.min(score)]

  # one last IRLS Iteration with the optimal lambda
  gamma_hat_optimal <- solve(t(Z) %*% W_sigma %*% Z + lambda_hat * K_sigma) %*% t(Z) %*% W_sigma %*% z_sigma
  sigma_hat_optimal <- exp(Z %*% gamma_hat_optimal)

  return(list(sigma_new = sigma_hat_optimal,
              sigma_mat = sigma_hat[,1:(i+1)],
              GD_sigma = GD_mat[1:(i+1)],
              lambda_hat = lambda_hat,
              gcv_score = score))
}
