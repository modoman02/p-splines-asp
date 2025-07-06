#' Create Spline Knot Sequence
#'
#' Generates inner and boundary knots for spline construction.
#'
#' @param x Vector of data points.
#' @param a Lower boundary.
#' @param b Upper boundary.
#' @param m Number of inner knots.
#' @param l Degree of the spline.
#' @param equi Logical. Equidistant or quantile-based knots.
#' @param tp Logical. Use truncated power basis if TRUE.
#' @param buffer Percentage buffer beyond data range.
#'
#' @return List containing knot positions and settings.
#' @export

create_knots <- function(x, a, b, m = 20, l, equi = TRUE, tp = FALSE, buffer = 0.05) { # a and b input is usually the minimum/maximum of x, which leads to
  # problems at the edges. a and b get a little buffer therefore
  a <- min(x) - buffer * (max(x) - min(x))
  b <- max(x) + buffer * (max(x) - min(x))
  if (equi) {
    inner_knots <- seq(a, b, length.out = m)
  } else {
    probs <- seq(1, m) / (m + 1)
    inner_knots <- quantile(x, probs = probs, names = FALSE)
  }

  # Knoten werden nur im case B-Splines erweitert, sowohl bei quantile based, als auch bei equidistant knots ist die Erweiterungsmethode aber die gleiche
  if (!tp) {
    left <- rep(a, l)
    right <- rep(b, l)
    knots <- c(left, inner_knots, right)
  } else {
    knots <- inner_knots
  }

  return(list(
    knots = knots,
    tp = tp,
    l = l))
}
