
#' This prints a summary of the results from a P-spline model fitted by `update_parameters()`.
#' This includes optimal lambda values, final deviance, number of iterations, and
#' optionally a preview of the estimated mu and sigma curves.
#'
#' Here again crucially we have to add a result_list A list returned by the `update_parameters()` function,
#'   containing the fitted model results.
#'
#' @return Invisibly returns the `result_list`.
#'
#' The function extracts key information from the `result_list` to provide a concise
#' overview of the P-spline model fit. It is certainly aligned to a normal "summary" Rbase function.
#'
#' # Again here assuming 'my_pspline_results' is a list returned by update_parameters()
#' The final function would work as follows: summary_results_pspline(my_pspline_results)
#'
summary_results_pspline <- function(result_list) {

  if (!inherits(result_list, "list")) {
    stop("Input must be a list returned by update_parameters().")
  }

  cat("\n--- P-spline Model Summary ---\n")
  # cat("Optimal Lambda (mu): ", result_list$lambda, "\n")
  # cat("Optimal Lambda (sigma): ", result_list$lambda_sigma, "\n")
  cat("Final Deviance: ", max(which(is.na(result_list$GD_mat))), "\n")

  if (!is.null(result_list$iterations)) {
    cat("Number of Iterations: ", result_list$iterations, "\n")
  }

  cat("\n--- Estimated Curves Preview ---\n")
  if (!is.null(result_list$mu_hat) && length(result_list$mu_hat) > 0) {
    cat("First 5 estimated mu values: ", head(result_list$mu_hat, 5), "...\n")
  }

  if (!is.null(result_list$sigma_hat) && length(result_list$sigma_hat) > 0) {
    cat("First 5 estimated sigma values: ", head(result_list$sigma_hat, 5), "...\n")
  }

  cat("-------------------------------\n")

  invisible(result_list)
}

summarytest <- summary_results_pspline(test)

