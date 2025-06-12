# file to process data and make it usable for analysis

# packages
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

# loading in sequential simulation data
seq_sim <- fread("analysis/seq_sim.csv")

# removing first column (iteration), not of use here
seq_sim <- seq_sim[,-1]

# variables that are important for analysis
result_vars <- c("n", "bf", "fit")
# the other ones are constants
constant_vars <- setdiff(names(seq_sim), result_vars)

# giving each simulation a code to know what the parameter set was
seq_sim <- seq_sim %>%
  mutate(sim_code = paste0(
    "id=", sim_id,
    "_nstart=", n_start,
    "_nstep=", n_step,
    "_bftarg=", bf_target,
    "_b1=", beta_1,
    "_hyp=", hypothesis
  ))


# getting mean, std. dv. and coefficients of variation (sd/mean), grouped on parameter set
seq_grouped <- seq_sim %>%
  group_by(sim_id) %>%
  summarise(across(all_of(result_vars),
                   list(mean = ~mean(.), sd = ~sd(.), cv = ~sd(.) / mean(.)),
                   .names = "{.col}_{.fn}"),
            .groups = "drop")

# joining the code to the grouped table
sim_code_lookup <- seq_sim %>%
  distinct(sim_id, sim_code)

seq_grouped <- seq_grouped %>%
  left_join(sim_code_lookup, by = "sim_id")

# table to show mean and sd of CVs of variables of interest
cv_summary <- seq_grouped %>%
  select(sim_id, n_cv, bf_cv, fit_cv) %>%
  pivot_longer(cols = -sim_id, names_to = "variable", values_to = "cv") %>%
  group_by(variable) %>%
  summarise(
    mean_cv = mean(cv),
    sd_cv = sd(cv),
    .groups = "drop"
  )

# BF means, full range
bf_fullrange_plot <- ggplot(seq_grouped, aes(x = bf_mean)) +
  geom_density(fill = "grey", alpha = 0.6) +
  labs(x = "AAFBF mean",
       y = "Density") +
  theme_classic()

# BF means, range 0-10
bf_constrained_plot <- seq_grouped %>%
  filter(bf_mean >= 0, bf_mean <= 10) %>%
  ggplot(aes(x = bf_mean)) +
  geom_density(fill = "grey", alpha = 0.6) +
  labs(x = "AAFBF mean (range 0–10)", y = "") +
  theme_classic()

(bf_plot <- (bf_fullrange_plot | bf_constrained_plot))
