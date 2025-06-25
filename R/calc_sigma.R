calc_sigma <- function (Z, y, K_sigma, mu_hat, sigma_hat, lambda_sigma, max_iterations_sigma) {
  n <- length(y)
  iterations <- 1
  for (i in 1:max_iterations_sigma) {
    # calc score
    residuals <- y - sigma_hat[,i]
    u_sigma <- residuals^2 / sigma_hat[,i]^2 - 1
    W_sigma <- diag(1 / sigma_hat[,i]^2, nrow = n, ncol = n)

    # calc pseudo response z
    z_sigma <- sigma[,i] + u_sigma / diag(W_sigma)

    # update sigma
    gamma_hat <- solve(t(Z) %*% W_sigma %*% Z + lambda_sigma * K_sigma) %*% t(Z) %*% W_sigma %*% z_sigma
    sigma_new <- exp(Z %*% gamma_hat)

    sigma_hat[,i] <- sigma_new
    iterations <- iterations + 1

    # check convergence
    GD_mat[i+1] <- calc_deviance(y = y, mu_hat = mu_hat, sigma_hat = sigma_new)   # gucken, wie man das wegen Rundungen etc. regelt
    if (GD_mat[i] < tolerance) {
      break
    }
  }
  return(list(mu_hat = mu_hat, sigma_hat = sigma_new))
}
