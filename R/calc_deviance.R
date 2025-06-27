calc_deviance <- function (y, mu_hat, sigma_hat) {
  return (sum(log(sigma_hat) + (y - mu_hat)^2 / sigma_hat))
}
