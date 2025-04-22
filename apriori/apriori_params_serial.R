# looping over different parameter sets
# for computing sample sizes a priori

# data.table for speed
library(data.table)

# source sample size computation function
source("apriori.R")

# all relevant parameters
# cross product, so all combinations get tested
param_grid_apriori <- CJ(
  n_min     = c(10, 30),
  n_max     = c(100, 1000),
  bf_target = c(3, 5, 20),
  beta_1    = c(0, 0.5, 1),
  intercept = c(-2, 0, 1),
  t         = c(1000, 10000),
  eta       = c(0.8, 0.9)
)
