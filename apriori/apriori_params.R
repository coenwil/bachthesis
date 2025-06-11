# looping over different parameter sets
# for computing sample sizes a priori

# data.table for speed
library(data.table)

# source sample size computation function
source("apriori/apriori.R")

# parameters that go together
fixed_params_prio <- data.table(
  n_min = c(10, 50),
  n_max = c(100, 1000)
)

# parameters that should vary across all combinations
free_params_prio <- CJ(
  bf_thresh = c(5, 10),
  eta = c(0.8, 0.9),
  intercept = -1, # duplaga
  beta_1 = c(0, 1),
  delta = 0.3,
  hypothesis = c("superiority", "non-inferiority", "equivalence"),
  t = 10000
)

# giving each a dummy to join on
fixed_params_prio[, dummy := TRUE]
free_params_prio[, dummy := TRUE]

# joining on dummy
param_grid_prio_test <- free_params_prio[fixed_params_prio, on = .(dummy), allow.cartesian = TRUE]

# removing dummy column
param_grid_prio_test[, dummy := NULL]

# 48 parameter sets
nrow(param_grid_prio_test)

# creating a list out of  every row in param_grid
# this means we have a list of lists that contain parameter sets
param_list_apriori <- lapply(1:nrow(param_grid_prio_test), function(x) as.list(param_grid_prio_test[x]))


# function to loop over all parameters
apriori_params <- function(param_list_apriori, ...) {
  
  # get a list with the extra arguments (to modify nr_it)
  extra_args <- list(...)
  
  # initiate empty list to store results
  # (this is a more memory efficient way of doing sim_results <- list() )
  sim_results_apriori <- vector("list", length(param_list_apriori))
  
  for (i in seq_along(param_list_apriori)) {
    
    # print progress
    message(sprintf("Running simulation %d of %d", i, length(param_list_apriori)))
    
    # get the current parameter set and the optional extra args in ...
    params <- modifyList(param_list_apriori[[i]], extra_args)
    
    # run aafbf() with current parameters and store results
    sim_result_apriori <- do.call(SSDlog, params)
    
    # add sim ID for tracing (TODO: make codes)
    sim_result_apriori[, sim_id := i]
    
    # store this 
    sim_results_apriori[[i]] <- sim_result_apriori
  }
  
  # return a big table with all results
  return(rbindlist(sim_results_apriori, use.names = TRUE, fill = TRUE))
  
}

# uncomment this for a test run
# recommend doing t = 100 for quick run, in practice would be 10000
full_test_sim_prio <- apriori_params(param_list_apriori)
