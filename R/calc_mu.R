calc_mu <- function(X, y, K_mu, mu_init, sigma_hat, lambda_grid , max_iterations_mu, tolerance) { # estimate next mu using Fisher Updates
  n <- length(y)
  mu_hat <- matrix(NA, nrow = n, ncol = max_iterations_mu + 1)
  mu_hat[,1] <- mu_init
  GD_mat <- numeric(max_iterations_mu + 1)
  for (i in 1:max_iterations_mu) {
    # calc score
    residuals <- y - mu_hat[,i]
    u_mu <- residuals / sigma_hat^2
    W_mu <- diag(1 / sigma_hat^2, nrow = n, ncol = n)

    # calc pseudo response z
    z_mu <- mu_hat[,i] + u_mu / diag(W_mu)

    # update mu
    beta_hat <- solve(t(X) %*% W_mu %*% X + lambda_mu * K_mu) %*% t(X) %*% W_mu %*% z_mu  # lambda fehlt noch
    mu_hat[,i+1] <- X %*% beta_hat

    # check convergence
    GD_mat[i+1] <- calc_deviance(y = y, mu_hat = mu_hat[,i+1], sigma_hat = sigma_hat)   # gucken, wie man das wegen Rundungen etc. regelt
    if (i > 1 && abs(GD_mat[i+1] - GD_mat[i]) < tolerance) { #wenn i > 1 nicht gecheckt wird, vergleicht er mit initialem NA in GD_mat
      break
    }
  }

  # approximate best lambda
  S_hat <- matrix()

  return(list(mu_new = mu_hat[,i+1],
              mu_mat = mu_hat,
              GD_mu = GD_mat))
}
