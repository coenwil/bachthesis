library(ggplot2)
library(patchwork)

# define values and range
x <- seq(-1, 1, length.out = 500)
y <- dnorm(x, mean = 0, sd = 0.3)

delta_noninf <- 0.25

delta_eq <- 0.03

df <- data.frame(beta = x, density = y)

(sup <- ggplot(df, aes(x = beta, y = density)) +
  geom_line() +
  geom_area(data = subset(df, beta > 0), aes(x = beta, y = density), fill = "gray", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Superiority", x = expression(beta[1]), y = "Density") +
  theme_classic())

(noninf <- ggplot(df, aes(x = beta, y = density)) +
    geom_line() +
    geom_area(data = subset(df, beta > delta_noninf), aes(x = beta, y = density), fill = "gray", alpha = 0.5) +
    geom_vline(xintercept = delta_noninf, linetype = "dashed") +
    labs(title = "Non-inferiority", x = expression(beta[1]), y = "") +
    theme_classic())

(eq <- ggplot(df, aes(x = beta, y = density)) +
    geom_line() +
    geom_area(data = subset(df, beta > -delta_eq & beta < delta_eq), aes(x = beta, y = density), fill = "gray", alpha = 0.5) +
    geom_vline(xintercept = c(-delta_eq, delta_eq), linetype = "dashed") +
    labs(title = "Equivalence", x = expression(beta[1]), y = "") +
    theme_minimal())

sup_noninf_eq_plots <- sup + noninf + eq
