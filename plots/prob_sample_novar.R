library(ggplot2)

prob_eq <- function(n) {
  (2 * 0.5^n) + 0.731^n + 0.269^n
}

n_vec <- 5:50

plot_df <- data.frame(
  n = n_vec,
  p = prob_eq(n_vec)
)

(prob_sample_novar_plot <- ggplot(
  plot_df, aes(x = n, y = p)) +
  stat_function(fun = prob_eq, linewidth = 0.8) +
  labs(x = "N", y = "P(sampling without variance)") +
  theme_minimal()
)
