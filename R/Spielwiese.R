calc_mu <- function(X, y, K_mu, mu_init, sigma_hat, lambda_mu, max_iterations_mu) { # estimate next mu using WLS
  n <- length(y)
  mu_hat <- matrix(NA, nrow = n, ncol = max_iterations_mu + 1)
  GD_mat <- numeric(max_iterations_mu + 1)
  for (i in 1:max_iterations_mu) {
    # calc score
    residuals <- y - mu_hat[,i]
    u_mu <- residuals / sigma_hat[,i]^2
    W_mu <- diag(1 / sigma_hat[,i]^2, nrow = n, ncol = n)

    # calc pseudo response z
    z_mu <- mu[,i] + u_mu / diag(W_mu)

    # update mu
    beta_hat <- solve(t(X) %*% W_mu %*% X + lambda_mu * K_mu) %*% t(X) %*% W_mu %*% z_mu  # lambda fehlt noch
    mu_hat[,i+1] <- X %*% beta_hat

    # check convergence
    GD_mat[i+1] <- calc_deviance(y = y, mu_hat = mu_hat[,i+1], sigma_hat = sigma_hat)   # gucken, wie man das wegen Rundungen etc. regelt
    if (abs(GD_mat[i+1] - GD_mat[i]) < tolerance) {
      break
    }
  }
  return(list(mu_new = mu_hat[,i+1],
              mu_mat = mu_hat,
              GD_mu = GD_mat))
}

calc_sigma <- function (Z, y, K_sigma, sigma_init, mu_hat, lambda_sigma, max_iterations_sigma) {
  n <- length(y)
  sigma_hat <- matrix(NA, nrow = n, ncol = max_iterations_sigma + 1)
  sigma_hat[, i] <- sigma_init
  GD_mat <- numeric(max_iterations_sigma + 1)
  for (i in 1:max_iterations_sigma) {
    # calc score
    residuals <- y - sigma_hat[,i]
    u_sigma <- residuals^2 / sigma_hat[,i]^2 - 1
    W_sigma <- diag(1 / sigma_hat[,i]^2, nrow = n, ncol = n)

    # calc pseudo response z
    z_sigma <- log(sigma[,i]) + u_sigma / diag(W_sigma)

    # update sigma
    gamma_hat <- solve(t(Z) %*% W_sigma %*% Z + lambda_sigma * K_sigma) %*% t(Z) %*% W_sigma %*% z_sigma
    sigma_hat[i + 1] <- exp(Z %*% gamma_hat)

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

update_parameters <- function (y, X, Z, lambda_mu, lambda_sigma, tolerance,
                               max_iterations, K_mu, K_sigma, max_iterations_mu = 15, max_iterations_sigma = 15) {  # updates mu and sigma respectively, each time with an updated value of the other parameter
  n <- length(y)
  init_vals <- get_initial_values(X = X, Z = Z, y = y)

  mu_hat <- matrix(NA, nrow = n, ncol = max_iterations+1)
  sigma_hat <- matrix(NA, nrow = n, ncol = max_iterations+1)
  GD_mat <- matrix(NA, nrow = 1, ncol = max_iterations+1)
  mu_hat[,1] <- init_vals$mu_hat
  sigma_hat[,1] <- init_vals$sigma_hat

  for (i in 1:max_iterations) {
    # iterative updates for sigma and mu respectively
    mu_result <- calc_mu(X = X, y = y, K_mu = K_mu, mu_init = mu_hat[, i], sigma_hat = sigma_hat[, i],
                         lambda_mu = lambda_mu, max_iterations_mu = max_iterations_mu, tolerance = tolerance)
    mu_hat[, i + 1] <- mu_result$mu_new
    sigma_result <- calc_sigma(Z = Z, y = y, K_sigma = K_sigma, sigma_init = sigma_hat[, i], mu_hat = mu_hat[, i + 1],
                               lambda_sigma = lambda_sigma, max_iterations_sigma = max_iterations_sigma, tolerance = tolerance)
    sigma_hat[, i + 1] <- sigma_result$sigma_new

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
              iterations = i))
}

