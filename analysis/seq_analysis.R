# file to process data and make it usable for analysis

# packages
library(data.table)
library(dplyr)

# loading in sequential simulation data
seq_sim <- fread("analysis/seq_sim.csv")

# removing first column (iteration), not of use here
seq_sim <- seq_sim[,-1]

# variables that are important for analysis
result_vars <- c("n", "bf", "fit", "complexity")
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
