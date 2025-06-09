get_initial_values <- function(x, y, l, knots) { # ist wohl besser, das alles über Matrix Kalkulation schon mit Spline Design-Matrix zu lösen, da schnellere Konvergenz und noch ein paar Vorteile
  X <- fit_spline(x = x, knots = knots, l = l)
  beta <- solve(t(X) %*% X) %*% t(X) %*% y
  mu_hat <- as.vector(X %*% beta)
  resid <- abs(y - mu_hat)
  sigma_hat <- rep(mean(resid), length(y))
  return(list(mu_hat = mu_hat, sigma_hat = sigma_hat))
}
