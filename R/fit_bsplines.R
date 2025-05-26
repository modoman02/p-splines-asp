fit_bsplines <- function (x, knots, m, l) {
  n <- length(x)
  d <- length(knots) - l + 1
  Z <- matrix(0, nrow = n, ncol = d)
  for (i in 1:n) {  # rows
    for(j in 1:d) { # cols
        Z[i, j] <- base_fun(x = x[i], knots = knots, l = l, j = j)
    }
  }
  return(Z)
}
