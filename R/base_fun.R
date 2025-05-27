base_fun <- function(x, knots, l, j, tp=FALSE) {
  if (tp) {

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

    term1 <- if (denom1 == 0) {0} else ((x - knots[j]) / denom1) * base_fun(x, knots, l - 1, j)
    term2 <- if (denom2 == 0) {0} else ((knots[j + l + 1] - x) / denom2) * base_fun(x, knots, l - 1, j + 1)
    return(term1 + term2)
  }
}
