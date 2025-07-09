# für Berechnung des AIC braucht man die Smoother Matrix S

# lambda grid sollte als Argument frei wählbar sein, damit Genauigkeit/Rechenleistung trade-off abgewägt werden kann
for (i in 1:10) {
  if (i == 4) break
}
print(i)  # gibt 4 aus


# GD matrix wird aktuell in update_parameters() und in calc_mu/sigma() initialisiert. Am besten wäre, es nur in calc_mu/calc_sigma zu
# initialisieren und in jeder update_parameter() eine GD matrix mit dem GD vektor aus den Iterationen upzudaten

# gcv Logik nochmal überprüfen - braucht man ne eigene Funktion dafür oder reicht die kurze Formel?

# Buffer wichtig für die Splines, sodass kkompletter Wertebereich geschätzt werden kann (siehe, wie es bei testgelände gemacht wurde)


#' Iteratively Update Spline Parameters
#'
#' Alternates estimation of mean and variance curves until convergence.
#'
#' @param X Design matrix for the mean.
#' @param Z Design matrix for the variance.
#' @param y Response vector.
#' @param max_iterations Maximum number of outer iterations.
#' @param K_mu Penalty matrix for the mean.
#' @param K_sigma Penalty matrix for the variance.
#' @param max_iterations_mu Maximum IRLS iterations for the mean.
#' @param max_iterations_sigma Maximum IRLS iterations for the variance.
#' @param tolerance Convergence threshold.
#' @param from_mu Start of lambda grid for the mean.
#' @param to_mu End of lambda grid for the mean.
#' @param stepsize_mu Step size for lambda grid for the mean.
#' @param from_sigma Start of lambda grid for the variance.
#' @param to_sigma End of lambda grid for the variance.
#' @param stepsize_sigma Step size for lambda grid for the variance.
#' @param lambda_init_mu Initial lambda value for mean estimation.
#' @param lambda_init_sigma Initial lambda value for variance estimation.
#'
#' @return List of fitted curves and diagnostics.
#' @export

update_parameters2 <- function (X, Z, y, max_iterations, K_mu, K_sigma, max_iterations_mu = 15,
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
    #lambda_mu_seq[i] <- mu_result$lambda_hat
    sigma_result <- calc_sigma2(Z = Z, y = y, K_sigma = K_sigma, sigma_init = sigma_hat[, i], mu_hat = mu_hat[, i + 1],
                                lambda_sigma = lambda_init_sigma, max_iterations_sigma = max_iterations_sigma, tolerance = tolerance)
    sigma_hat[, i + 1] <- sigma_result$sigma_new
    GD_mat_sigma <- sigma_result$GD_sigma
    #lambda_sigma_seq[i] <- sigma_result$lambda_hat
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


calc_sigma2 <- function(Z, y, K_sigma, sigma_init, mu_hat,
                        lambda_sigma = 1, max_iterations_sigma = 50, tolerance = 1e-6) {

  cat("🚀 calc_sigma start\n")
  n <- length(y)
  sigma_hat <- matrix(NA, nrow = n, ncol = max_iterations_sigma + 2)
  sigma_hat[, 1] <- sigma_init
  GD_mat <- numeric(max_iterations_sigma + 2)

  # Startwerte
  gamma_hat_last <- rep(0, ncol(Z))
  sigma_last <- sigma_init

  for (i in 1:max_iterations_sigma) {
    cat("🔁 Iteration:", i, "\n")

    sigma_vec <- sigma_last
    residuals <- as.vector(y - mu_hat)
    u_sigma <- -1 + (residuals^2) / sigma_vec^2
    w_sigma <- rep(2, n)
    eta_sigma <- log(sigma_vec)
    z_sigma <- eta_sigma + u_sigma / w_sigma
    W_sigma <- diag(w_sigma)

    # LGS aufstellen
    lhs <- t(Z) %*% W_sigma %*% Z + lambda_sigma * K_sigma
    rhs <- t(Z) %*% W_sigma %*% z_sigma

    # Robust lösen
    gamma_hat_try <- tryCatch(
      solve(lhs, rhs),
      error = function(e) {
        warning("⚠️ solve() failed in iteration ", i)
        return(rep(NA, length(rhs)))
      }
    )

    if (any(is.na(gamma_hat_try)) || any(!is.finite(gamma_hat_try))) {
      cat("❌ gamma_hat numerically unstable, breaking\n")
      break
    }

    eta_new <- Z %*% gamma_hat_try
    eta_new[eta_new > 50] <- 50
    eta_new[eta_new < -20] <- -20
    sigma_new <- exp(eta_new)

    # Check auf NAs
    if (any(is.na(sigma_new)) || any(!is.finite(sigma_new))) {
      cat("❌ sigma_new contains NA or Inf, breaking\n")
      break
    }

    # Update nur bei Erfolg
    sigma_hat[, i+1] <- sigma_new
    gamma_hat_last <- gamma_hat_try
    sigma_last <- sigma_new

    GD_mat[i+1] <- calc_deviance(y = y, mu_hat = mu_hat, sigma_hat = sigma_new)
    cat("   ➤ GD:", GD_mat[i+1], "\n")

    if (i > 1 && abs(GD_mat[i+1] - GD_mat[i]) < tolerance) {
      cat("✅ Converged\n")
      break
    }
  }

  return(list(
    sigma_new = sigma_last,
    sigma_mat = sigma_hat[, 1:(i+1)],
    GD_sigma = GD_mat[1:(i+1)]
  ))
}
