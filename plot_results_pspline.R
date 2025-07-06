plot_results_pspline <- function(result_list, true_mu = NULL, true_sigma = NULL) {

  if (!inherits(result_list, "list")) {
    stop("Input must be a list returned by update_parameters().")
  }


  x_values <- x #here you can see that this is all dependend on us having a resultslist
  mu_estimated <- result_list$mu_hat
  sigma_estimated <- result_list$sigma_hat

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
