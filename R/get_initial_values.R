get_initial_values <- function (x, y) {
  initial_model <- lm(y ~ x)
  y_hat <- fitted.values(initial_model)
  mu_hat <- rep(mean(y_hat), times = length(y))
  sigma_hat <- rep(sigma(initial_model), times = length(y))
  return(list(mu_hat = mu_hat, sigma_hat = sigma_hat))
}
