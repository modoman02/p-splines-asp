calc_mu <- function(x, y, lambda, beta_hat, ) { # estimate next mu using LRLS
  for (i in 1:max_iterations_mu) {
    #  calc score
    residuals <- y - mu_hat[,i]
    u_mu <- residuals / sigma_hat[,i]^2
    W_mu <- diag(1 / sigma_hat[,i]^2, nrow = n, ncol = n)
    z_mu <- mu[,i] + u_mu / diag(W_mu)

    # update mu
    beta_hat <- solve(t(X) %*% W_mu %*% X + lambda_mu * K_mu) %*% t(X) %*% W_mu %*% z_mu  # lambda fehlt noch
    mu_new <- X %*% beta_hat

    mu_hat[,i+1] <- mu_new
    iterations <- iterations + 1
    # check convergence
    GD_mat[i+1] <- calc_deviance(y = y, mu_hat = mu_new, sigma_hat = sigma_new)   # gucken, wie man das wegen Rundungen etd. regelt
    if (GD_mat[i] < tolerance) {
      break
    }
    else if (iterations == max_iterations) {
      break
    }
  }
}
