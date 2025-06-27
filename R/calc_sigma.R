calc_sigma <- function (Z, y, K_sigma, sigma_init, mu_hat, lambda_grid, max_iterations_sigma, tolerance) { # calc next sigma using Fisher Updates
  n <- length(y)
  sigma_hat <- matrix(NA, nrow = n, ncol = max_iterations_sigma + 1)
  sigma_hat[, 1] <- sigma_init
  GD_mat <- numeric(max_iterations_sigma + 1)
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
    if (abs(GD_mat[i+1] - GD_mat[i]) < tolerance) {
      break
    }
  }
  return(list(sigma_new = sigma_hat[,i+1],
              sigma_mat = sigma_hat,
              GD_sigma = GD_mat))
}
