#' Summarize P-spline Results
#'
#' Prints a short summary of estimated parameters and convergence statistics
#' from the adaptive P-spline model results.
#'
#' @param result A list containing model output from `run_all_funcs()` or similar,
#'   including mu_hat, sigma_hat, GD_mat, and iteration details.
#'
#' @return Prints text output to the console.
#' @export

summary_all <- function(result) {
  cat("=== P-Spline summary ===\n\n")

  cat("number of iterations: ", result$iterations, "\n\n")

  final_dev <- min(result$GD_mat[result$GD_mat != 0])
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

  if (!is.null(result$lambda_mu)) {
    cat("best lambda for mu:     ", result$lambda_mu, "\n")
  }
  if (!is.null(result$lambda_sigma)) {
    cat("best lambda for sigma:  ", result$lambda_sigma, "\n")
  }

  cat("\n=======================================\n")
}
