calc_mu <- function(X, y, K_mu, mu_hat, sigma_hat, lambda_mu, max_iterations_mu) { # estimate next mu using WLS
  n <- length(y)
  iterations <- 1
  for (i in 1:max_iterations_mu) {
    # calc score
    residuals <- y - mu_hat[,i]
    u_mu <- residuals / sigma_hat[,i]^2
    W_mu <- diag(1 / sigma_hat[,i]^2, nrow = n, ncol = n)

    # calc pseudo response z
    z_mu <- mu[,i] + u_mu / diag(W_mu)

    # update mu
    beta_hat <- solve(t(X) %*% W_mu %*% X + lambda_mu * K_mu) %*% t(X) %*% W_mu %*% z_mu  # lambda fehlt noch
    mu_new <- X %*% beta_hat

    mu_hat[,i+1] <- mu_new  # mu, sigma und GD Matrizen müssen wahrscheinlich global angelegt werden, damit calc_mu und calc_sigma sie kennen
    iterations <- iterations + 1

    # check convergence
    GD_mat[i+1] <- calc_deviance(y = y, mu_hat = mu_new, sigma_hat = sigma_hat)   # gucken, wie man das wegen Rundungen etc. regelt
    if (GD_mat[i] < tolerance) {
      break
    }
  }
  return(list(mu_hat = mu_new, sigma_hat = sigma_hat))
}
