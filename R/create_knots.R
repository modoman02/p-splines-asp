create_knots <- function(x, a, b, m = 20, l, equi = TRUE, tp = FALSE) { #
  if (equi) {
    inner_knots <- seq(a, b, length.out = m)
  } else {
    probs <- seq(1, m) / (m + 1)
    inner_knots <- quantile(x, probs = probs, names = FALSE)
  }

  # Knoten werden nur im case B-Splines erweitert, sowohl bei quantile based, als auch bei equidistant knots ist die Erweiterungsmethode aber die gleiche
  if (!tp) {
    left <- rep(inner_knots[1], l)
    right <- rep(inner_knots[length(inner_knots)], l)
    knots <- c(left, inner_knots, right)
  } else {
    knots <- inner_knots
  }

  return(list(
    knots = knots,
    tp = tp,
    l = l))
}
