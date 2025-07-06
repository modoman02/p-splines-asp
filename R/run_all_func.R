#' Run Full Spline Estimation Pipeline
#'
#' Complete procedure fitting mean and variance splines for data.
#'
#' @param x Predictor values.
#' @param y Response values.
#' @param a Lower boundary for knots.
#' @param b Upper boundary for knots.
#' @param m Number of inner knots.
#' @param l Degree of splines.
#' @param equi Logical. Equidistant or quantile-based knots.
#' @param tp Logical. Use truncated power basis if TRUE.
#' @param buffer Percentage buffer beyond data range.
#'
#' @return List of fitted model results.
#' @export

run_all_funcs <- function(x, y, a, b, m = 20, l, equi = T, tp = F, buffer = 0.05) { # Wrapper, that .does everything from start to finish
  # fit splines
  knots_X <- create_knots(x = x, a = min(x), b = max(x), l = 3, equi = T)
  knots_Z <- create_knots(x = x, a = min(x), b = max(x), l = 3, equi = T, m = 10)
  X <- fit_spline(x = x, knots = knots_X)
  Z <- fit_spline(x = x, knots = knots_Z)

  # get initial values for first iteration
  init_values <- get_initial_values(X = X, Z = Z, y = y)

  # get penalty matrices
  K_mu <- get_pen_mat(knots = knots_X)
  K_sigma <- get_pen_mat(knots = knots_Z)

  # run all functions
  result <- update_parameters(X = X, Z = Z, y = y, max_iterations = 20, K_mu = K_mu, K_sigma = K_sigma, to_mu = 5, stepsize_mu = 0.5,
                                              to_sigma = 5, stepsize_sigma = 0.5)

}

