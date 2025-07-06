summary_all <- function(result) {
  cat("=== P-Spline summary ===\n\n")

  cat("number of iterations: ", result$iterations, "\n\n")

  final_dev <- tail(na.omit(result$GD_mat), 1)
  cat("final global deviance: ", round(final_dev, 4), "\n\n")

  mu <- result$mu_hat
  cat("estimated mu:\n")
  cat("  - mean:         ", round(mean(mu), 4), "\n")
  cat("  - minimum:      ", round(min(mu), 4), "\n")
  cat("  - maximum:      ", round(max(mu), 4), "\n\n")

  sigma <- result$sigma_hat
  cat("estimated sigma:\n")
  cat("  - mean:         ", round(mean(sigma), 4), "\n")
  cat("  - minimum:      ", round(min(sigma), 4), "\n")
  cat("  - maximum:      ", round(max(sigma), 4), "\n\n")

  if (!is.null(result$lambda_mu_best)) {
    cat("best lambda for mu:     ", result$lambda_mu_best, "\n")
  }
  if (!is.null(result$lambda_sigma_best)) {
    cat("best lambda for sigma:  ", result$lambda_sigma_best, "\n")
  }

  cat("\n=======================================\n")
}
