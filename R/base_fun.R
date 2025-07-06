#' Evaluate a Spline Basis Function
#'
#' Computes a single spline basis function at given x-values.
#'
#' @param x Numeric vector of data points.
#' @param knots List with knot positions and spline settings.
#' @param m Number of inner knots.
#' @param j Index of the basis function to evaluate.
#'
#' @return Numeric vector with evaluated basis function values.
#' @export

base_fun <- function(x, knots, m, j) { # x = data, knots = list of knots, l = degree, m = num of knots in interval [a,b], j = current knot
  l <- knots$l
  tp <- knots$tp
  knots <- knots$knots
  if (tp) {
    d <- m + l - 1
    kappa_j <- knots[j]
    return(pmax(0, x - kappa_j)^l)
  }
  else {
    if (l == 0) {
      if (j == length(knots) - 1) {
        return(ifelse(x >= knots[j] & x <= knots[j+1], 1, 0))
      } else {
        return(ifelse(x >= knots[j] & x < knots[j+1], 1, 0))
      }
    }
    denom1 <- knots[j + l] - knots[j]
    denom2 <- knots[j + l + 1] - knots[j + 1]

    term1 <- if (denom1 == 0) {0} else ((x - knots[j]) / denom1) *
      base_fun(x = x, knots = list(knots = knots, tp = tp, l = l - 1), m = m, j = j)
    term2 <- if (denom2 == 0) {0} else ((knots[j + l + 1] - x) / denom2) *
      base_fun(x = x, knots = list(knots = knots, tp = tp, l = l - 1), m = m, j = j + 1)
    return(term1 + term2)
  }
}
