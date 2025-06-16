calc_deviance <- function (y, mu_hat, sigma_hat) {  # man könnte noch andere KOnvergenzkriterien hinzunehmen und daraus z.B. einen Index bilden, GD sollte aber auch reichen
  return (sum(log(sigma_hat) + (y - mu_hat)^2 / sigma_hat))
}
