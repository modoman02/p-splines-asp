#' Calculate Deviance
#'
#' Computes the deviance for fitted values and variance estimates.
#'
#' @param y Observed response values.
#' @param mu_hat Estimated mean values.
#' @param sigma_hat Estimated standard deviation values.
#'
#' @return A single numeric value: the deviance.
#' @export

calc_deviance <- function (y, mu_hat, sigma_hat) {
  return (sum(log(sigma_hat) + (y - mu_hat)^2 / sigma_hat))
}
