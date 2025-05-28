fit_spline <- function (x, knots, l, tp = FALSE) {
  n <- length(x)
  if (tp) {
    d <- l + 1 + length(knots)
    Z <- matrix(0, nrow = n, ncol = d)
    for (i in 1:n) {
      for (j in 1:d) {
        Z[i, j] <- base_fun(x = x[i], knots = knots, l = l, j = j, tp = TRUE)
      }
    }
  }
  else {
    d <- length(knots) - l - 1
    Z <- matrix(0, nrow = n, ncol = d)
    for (i in 1:n) {  # rows
      for(j in 1:d) { # cols
        Z[i, j] <- base_fun(x = x[i], knots = knots, l = l, j = j)
      }
    }
  }
  return(Z)
}
