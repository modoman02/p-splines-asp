plot_all <- function(result, x, y, alpha = 0.05, show_sigma = T) {
  data <- abdom

  z_alpha <- qnorm(1 - alpha / 2)

  lower <- result$mu_hat - z_alpha * result$sigma_hat
  upper <- result$mu_hat + z_alpha * result$sigma_hat

  plot <- ggplot(data, aes(x = x, y = y)) +
    geom_point(color = "black", alpha = 0.6) +
    geom_line(aes(y = result$mu_hat), color = "red", linewidth = 1) +
    labs(x = "x", y = "y", title = "estimated mu and sigma vs. data") +
    theme_minimal()

  if (show_sigma) {
    plot <- plot + geom_ribbon(aes(ymin = lower, ymax = upper),
                         alpha = 0.2, fill = "red", linetype = 2)
  }

  return(plot)
}
