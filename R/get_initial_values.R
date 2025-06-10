get_initial_values <- function(x, y, l, knots) { # initial value = erster OLS Schätzer ohne weight matrix; unklar ob man die Funktion überhaupt braucht oder ob man die paar Rechnungen sonst auch einfach im Wrapper durchführen könnte
  X <- fit_spline(x = x, knots = knots, l = l)
  beta <- solve(t(X) %*% X) %*% t(X) %*% y
  mu_hat <- as.vector(X %*% beta)
  resid <- abs(y - mu_hat)
  sigma_hat <- rep(mean(resid), length(y))
  return(list(mu_hat = mu_hat, sigma_hat = sigma_hat))
}
