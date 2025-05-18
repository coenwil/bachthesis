# plots to visualize fit and complexity
library(ggplot2)
library(patchwork)

# data and distributions
x <- seq(-1, 1, length.out = 500)
delta_noninf <- -0.25
delta_eq <- 0.1

prior <- dnorm(x, mean = 0, sd = 0.75)
prior_noninf <- dnorm(x, mean = delta_noninf, sd = 0.75)
posterior <- dnorm(x, mean = 0.25, sd = 0.25)
df <- data.frame(x = x, prior = prior, prior_noninf = prior_noninf, posterior = posterior)


# superiority
(sup_fitcomp <- ggplot(df, aes(x)) +
  geom_area(data = subset(df, x > 0), aes(y = prior), fill = "lightgray") +
  geom_area(data = subset(df, x > 0), aes(y = posterior), fill = "lightgray") +
  geom_line(aes(y = posterior), size = 1) +
  geom_line(aes(y = prior), size = 1, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dotted", size = 0.8) +
  labs(title = "Superiority",
       subtitle = expression(H[1] * ": " * beta[1] > 0),
       x = expression(beta[1]), y = "Density") +
  theme_classic())


# non-inferiority
(noninf_fitcomp <- ggplot(df, aes(x)) +
  geom_area(data = subset(df, x > delta_noninf), aes(y = prior_noninf), fill = "lightgray") +
  geom_area(data = subset(df, x > delta_noninf), aes(y = posterior), fill = "lightgray") +
  geom_line(aes(y = posterior), size = 1) +
  geom_line(aes(y = prior_noninf), size = 1, linetype = "dashed") +
  geom_vline(xintercept = delta_noninf, linetype = "dotted", size = 0.8) +
  labs(title = "Non-inferiority",
       subtitle = bquote(H[1]: ~ beta[1] > delta  ~~ "(" ~ delta == -0.25 ~ ")"),
       x = expression(beta[1]), y = "") +
  theme_classic())

# equivalence
(eq_fitcomp <- ggplot(df, aes(x)) +
  geom_area(data = subset(df, x > -delta_eq & x < delta_eq), aes(y = prior), fill = "lightgray") +
  geom_area(data = subset(df, x > -delta_eq & x < delta_eq), aes(y = posterior), fill = "lightgray") +
  geom_line(aes(y = posterior), size = 1) +
  geom_line(aes(y = prior), size = 1, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dotted", size = 0.8) +
  labs(title = "Equivalence",
       subtitle = bquote(H[1]: ~ beta[1] %in% ~ "[" * -delta * ", " * delta * "]" ~ "(" ~ delta == 0.1 ~ ")"),
       x = expression(beta[1]), y = "") +
  theme_classic())


(sup_noninf_eq__fitcomp_plots <- (sup_fitcomp | noninf_fitcomp | eq_fitcomp) +
    plot_annotation(caption = "Focal point: dotted vertical line\nPrior distribution: dashed line\nPosterior distribution: solid line") +
    plot_layout(ncol = 3))
