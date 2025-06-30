calc_sigma <- function (Z, y, K_sigma, sigma_init, mu_hat, lambda_grid, lambda_sigma, max_iterations_sigma, tolerance) { # calc next sigma using Fisher Updates
  n <- length(y)
  sigma_hat <- matrix(NA, nrow = n, ncol = max_iterations_sigma + 2)
  sigma_hat[, 1] <- sigma_init
  GD_mat <- numeric(max_iterations_sigma + 2)
  for (i in 1:max_iterations_sigma) {
    # calc score
    residuals <- y - mu_hat
    u_sigma <- residuals^2 / sigma_hat[,i]^2 - 1
    W_sigma <- diag(1 / sigma_hat[,i]^2, nrow = n, ncol = n)

    # calc pseudo response z
    z_sigma <- log(sigma_hat[,i]) + u_sigma * sigma_hat[,i]^2
    # update sigma
    gamma_hat <- solve(t(Z) %*% W_sigma %*% Z + lambda_sigma * K_sigma) %*% t(Z) %*% W_sigma %*% z_sigma
    sigma_hat[,i + 1] <- exp(Z %*% gamma_hat)

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
    S_lambda <- Z %*% solve(t(Z) %*% W_sigma %*% Z + lambda * K_sigma) %*% t(Z) %*% W_sigma
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
