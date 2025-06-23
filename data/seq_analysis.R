# file to process data and make it usable for analysis

# packages
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(stringr)

# loading in sequential simulation data
seq_sim <- fread("data/seq_sim.csv")

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
  select(sim_id, n_cv, bf_cv) %>%
  pivot_longer(cols = -sim_id, names_to = "variable", values_to = "cv") %>%
  group_by(variable) %>%
  summarise(
    mean_cv = mean(cv),
    sd_cv = sd(cv),
    .groups = "drop"
  )

# plot of BF means, full range
bf_fullrange_plot <- ggplot(seq_grouped, aes(x = bf_mean)) +
  geom_histogram(fill = "grey", color = "black") +
  labs(x = "AAFBF mean",
       y = "Count") +
  theme_classic()

# plot BF means, range 0-10
bf_constrained_plot <- seq_grouped %>%
  filter(bf_mean >= 0, bf_mean <= 20) %>%
  ggplot(aes(x = bf_mean)) +
  geom_histogram(fill = "grey", color = "black") +
  labs(x = "AAFBF mean (range 0–20)", y = "") +
  theme_classic()

(bf_plot <- (bf_fullrange_plot | bf_constrained_plot))

# correlation between bayes factor mean and CV
cor(seq_grouped$bf_mean, seq_grouped$bf_sd, method = "pearson")

# seeing how often the sequential design found support when it should
# extracting parameters from sim_code
seq_grouped <- seq_grouped %>%
  mutate(
    hyp = str_extract(sim_code, "hyp=[^_]+"),
    hyp = str_remove(hyp, "hyp="),
    
    b1 = str_extract(sim_code, "b1=\\d+"),
    b1 = as.numeric(str_remove(b1, "b1=")),
    
    thresh = str_extract(sim_code, "bftarg=[0-9.]+"),
    thresh = as.numeric(str_remove(thresh, "bftarg=")),
    
    nstart = str_extract(sim_code, "nstart=[0-9.]+"),
    nstart = as.numeric(str_remove(nstart, "nstart=")),
    
    step = str_extract(sim_code, "nstep=[0-9.]+"),
    step = as.numeric(str_remove(step, "nstep=")),
    
    correct = case_when(
      (hyp %in% c("non-inferiority", "superiority") & (b1 == 1 & bf_mean >= thresh)) ~ 1,
      
      (hyp == "equivalence" & (b1 == 0 & bf_mean >= thresh)) ~ 1,
      TRUE ~ 0
    ),
    conclusive = case_when(
      (bf_mean >= thresh | bf_mean <= 1/thresh) ~ 1,
      TRUE ~ 0
    ),
    trial_aligned = case_when(
      (hyp %in% c("non-inferiority", "superiority") & (b1 == 1)) ~ 1,
      (hyp == "equivalence" & (b1 == 0)) ~ 1,
      TRUE ~ 0
    )
  )

# conditional table with results
full_cond_table <- seq_grouped %>% count(conclusive, correct, hyp, b1, nstart, step)

# all trial aligned sims were correct
seq_grouped %>% count(trial_aligned, correct)

sum(seq_sim$fail_count)

sum(seq_sim$fail_count) / nrow(seq_sim)

seq_grouped %>% 
  filter(conclusive == 1, correct == 1, nstart == 50) %>% 
  summarise(conc_correct_n = mean(n_mean))

seq_grouped %>% 
  filter(conclusive == 1, correct == 1, thresh == 10) %>% 
  summarise(conc_correct_n = mean(n_mean))

seq_grouped %>% 
  filter(conclusive == 1, correct == 1, hyp == "equivalence") %>% 
  summarise(conc_correct_n = mean(n_mean))
