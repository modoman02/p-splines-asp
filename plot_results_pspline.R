#\' Plots the estimated mu and sigma curves from a P-spline model fitted by
#\' `update_parameters()`. Optionally, we can overlay the true mu and sigma values.
#\'
#\' result_list = A list returned by the `update_parameters()` function,
#\'   containing the fitted model results.
#\' true_mu = (Optional) A numeric vector of true mu values for comparison.
#\' true_sigma = (Optional) A numeric vector of true sigma values for comparison.
#\'
#\' @return A plot displaying the estimated curves. Invisibly here returns `NULL`. This we have to deal with 
#\'
#\' @details
#\' This function generates two plots: one for the estimated mu curve and one for
#\' the estimated sigma curve. If `true_mu` or `true_sigma` are provided, they
#\' will be added to the respective plots for visual assessment of the model fit.
#\' The plots are generated using base R graphics.
#\'
#\' All the example assume something which has to be changed in the package. The main thin is the result list. Otherwise its just a bit complicated to do the graphs. Its not a big change.
#\' # Assuming 'my_pspline_results' is a list returned by update_parameters()
#\' # and 'true_mu_values', 'true_sigma_values' are available.
#\' # plot_results_pspline(my_pspline_results, true_mu = true_mu_values, true_sigma = true_sigma_values)

plot_results_pspline <- function(result_list, true_mu = NULL, true_sigma = NULL) {

  if (!inherits(result_list, "list")) {
    stop("Input must be a list returned by update_parameters().")
  }

  if (is.null(result_list$x_values) || is.null(result_list$mu_estimated) || is.null(result_list$sigma_estimated)) {
    stop("result_list must contain 'x_values', 'mu_estimated', and 'sigma_estimated'.")
  }

  x_values <- result_list$x_values #here you can see that this is all dependend on us having a resultslist
  mu_estimated <- result_list$mu_estimated
  sigma_estimated <- result_list$sigma_estimated

  # Plot for mu
  plot(x_values, mu_estimated, type = "l", col = "blue", lwd = 2,
       xlab = "Predictor (x)", ylab = "Estimated mu",
       main = "Estimated Mu Curve")
  if (!is.null(true_mu)) {
    lines(x_values, true_mu, col = "red", lty = 2, lwd = 2)
    legend("topright", legend = c("Estimated mu", "True mu"),
           col = c("blue", "red"), lty = c(1, 2), lwd = 2)
  }

  # Plot for sigma
  plot(x_values, sigma_estimated, type = "l", col = "darkgreen", lwd = 2,
       xlab = "Predictor (x)", ylab = "Estimated sigma",
       main = "Estimated Sigma Curve")
  if (!is.null(true_sigma)) {
    lines(x_values, true_sigma, col = "purple", lty = 2, lwd = 2)
    legend("topright", legend = c("Estimated sigma", "True sigma"),
           col = c("darkgreen", "purple"), lty = c(1, 2), lwd = 2)
  }

  invisible(NULL)
}


