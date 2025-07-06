library(devtools)
library(gamlss)
############################## testing for base_fun(), fit_spline() and create_knots() ################################
x <- abdom$x
y <- abdom$y
l <- 3
padding <- 0.05 * (max(x) - min(x))
a <- min(x) + padding
b <- max(x) - padding
knots <- create_knots(x = x, a = min(x), b = max(x), l = l)
knots1 <- create_knots(x = abdom$x, a = min(abdom$x), b = max(abdom$x), l = 1, tp = T)
knots2 <- create_knots(x = abdom$x, a = min(abdom$x), b = max(abdom$x), l = 2, tp = T)
knots3 <- create_knots(x = abdom$x, a = min(abdom$x), b = max(abdom$x), l = 3, tp = T)
knots5 <- create_knots(x = abdom$x, a = min(abdom$x), b = max(abdom$x), l = 3, tp = T, equi = F)
knots4 <- create_knots(x = x$x, a = a, b = b, l = 3, tp = T)
Z <- fit_spline(x = x, knots = knots)
Z1 <- fit_spline(x = x, knots = knots1)
Z2 <- fit_spline(x = x, knots = knots2)
Z3 <- fit_spline(x = x, knots = knots3)
Z4 <- fit_spline(x = x, knots = knots4)
Z5 <- fit_spline(x = x, knots = knots5)




## testing penalty matrix construction
K_mu <- get_pen_mat(d = length(knots$knots), r = 2)

############### schätzung von mu hat am rechten Rand immer sehr starken Knick, Peter, Bob und Justus ermitteln
a <- min(x) - 0.05 * (max(x) - min(x)) # Buffer wichtig für die Splines, sodass kkompletter Wertebereich geschätzt werden kann
b <- max(x) + 0.05 * (max(x) - min(x))
knots_X <- create_knots(x = x, a = min(x), b = max(x), l = 4, equi = T)
knots_Z <- create_knots(x = x, a = min(x), b = max(x), l = 4, equi = T, m = 10)
X <- fit_spline(x = x, knots = knots_X)
Z <- fit_spline(x = x, knots = knots_Z)
init_values <- get_initial_values(X = X, Z = Z, y = y)
# plots for testing mu
plot(x, y, col = "gray")
lines(x, init_values$mu_hat, col = "blue", lwd = 2)

plot(init_values$mu_hat[5:(length(init_values$mu_hat)-5)], type = "l")


beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y  # fit initial values with ordinary OLS
plot(x, X %*% beta_hat, type = "l", col = "blue")
plot(x, init_values$sigma_hat, type = "l", col = "red")


############## testing for the big ones ###############


####### testing calc_mu() #########
knots_X <- create_knots(x = x, a = min(x), b = max(x), l = 3, equi = T)
X <- fit_spline(x = x, knots = knots_X)
K_mu <- get_pen_mat(knots = knots_X, r = 2)
mu_init <- init_values$mu_hat
sigma_hat <- init_values$sigma_hat
lambda_grid_mu <- seq(0, 5, by = 0.5)
max_iterations_mu <- 10
tolerance <- sqrt(.Machine$double.eps)
mu_iteration <- calc_mu(X = X, y = y, K_mu = K_mu, mu_init = mu_init, sigma_hat = sigma_hat,
                        lambda_grid = lambda_grid_mu, max_iterations_mu = max_iterations_mu, tolerance = tolerance)
mu_lm <- lm(y ~ 0 + X)  # ohne Intercept
plot(fitted(mu_lm), type = "l", col = "blue")
lines(mu_iteration$mu_new, col = "red")
lambda <- 2
S_lambda <- X %*% solve(t(X) %*% W_mu %*% X + lambda * K_mu) %*% t(X) %*% W_mu
trace_S <- sum(diag(S_lambda))
gcv <- sum((y - S_lambda %*% y)^2) / (1 - trace_S / n)^2
gcv

n <- length(y)
sigma_hat <- as.vector(sigma_hat)
W_mu <- diag(as.numeric(1 / sigma_hat^2), nrow = n, ncol = n)
dim(X)          # sollte n × p
dim(W_mu)       # sollte n × n
dim(t(X))       # sollte p × n
dim(K_mu)       # sollte p × p


######## testing calc_sigma() ########
knots_Z <- create_knots(x = x, a = min(x), b = max(x), l = 3, m = 10)
Z <- fit_spline(x = x, knots = knots_Z)
K_sigma <- get_pen_mat(knots = knots_Z, r = 2)
sigma_init <- init_values$sigma_hat
mu_hat <- init_values$mu_hat
lambda_grid_sigma <- seq(0.5, 5, by = 0.5)
max_iterations_sigma <- 10
sigma_iteration <- calc_sigma(Z = Z, y = y, K_sigma = K_sigma, sigma_init = sigma_init, mu_hat = mu_hat, lambda_grid = lambda_grid_sigma,
                              max_iterations_sigma = max_iterations_sigma, tolerance = tolerance)
sf <- sigma_iteration$sigma_new
plot(x, sf, col = "gray", main = "Vergleich: Wahres sigma vs. geschätztes sigma",
     ylab = "sigma", xlab = "x", pch = 16)
lines(x, result_sigma$sigma_hat, col = "blue", lwd = 2)








