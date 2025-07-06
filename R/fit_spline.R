fit_spline <- function (x, knots) {  # returns Z-matrix, containing the Basis function for every data point
  tp <- knots$tp
  l <- knots$l
  knots <- knots$knots
  n <- length(x)
  if (tp) {
    d <- l + 1 + length(knots)
    Z <- matrix(NA, nrow = n, ncol = d)
    for (i in 1:n) {
      for (j in 1:(l+1)) {
        Z[i, j] <- x[i]^(j-1)
      }
      for (j in 1:length(knots)) {
        Z[i, l + 1 + j] <- base_fun(x = x[i], knots = list(knots = knots, tp = tp, l = l), m = length(knots), j = j)
      }
    }
  }
  else {
    d <- length(knots) - l - 1
    Z <- matrix(NA, nrow = n, ncol = d)
    for (i in 1:n) {  # rows
      for(j in 1:d) { # cols
        Z[i, j] <- base_fun(x = x[i], knots = list(knots = knots, tp = tp, l = l), j = j, m = NA)
      }
    }
  }
  return(Z)
}
