create_knots <- function (x, a, b, m = 20, l, equi = T) {
  if (!equi) {
    probs <- seq(1, m) / (m + 1)
    knots <- quantile(x, probs = probs, names = F)
    h_left <- inner_knots[2] - inner_knots[1]
    h_right <- inner_knots[length(inner_knots)] - inner_knots[length(inner_knots) - 1]
    left_knots <- inner_knots[1] - seq(from = l, to = 1) * h_left
    right_knots <- inner_knots[length(inner_knots)] + seq(from = 1, to = l) * h_right
  }
  else {
    h <- (b - a) / (m - 1)
    inner_knots <- seq(a, b, by = h)
    left_knots <- seq(a - l * h, a - h, by = h)
    right_knots <- seq(b + h, b + l * h, by = h)
  }
  knots <- c(left_knots, inner_knots, right_knots)
  return(knots)
}
