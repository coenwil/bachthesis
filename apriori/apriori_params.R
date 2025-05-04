# looping over different parameter sets
# for computing sample sizes a priori

# data.table for speed
library(data.table)

# source sample size computation function
source("apriori/apriori.R")

# all relevant parameters
# cross product, so all combinations get tested
param_grid_apriori <- CJ(
  n_min     = c(10, 30),
  n_max     = c(100, 1000),
  bf_thresh = c(3, 5, 20),
  beta_1    = c(0, 0.5, 1),
  intercept = c(-2, 0, 1),
  t         = 100,
  eta       = c(0.8, 0.9)
)

# creating a list out of  every row in param_grid
# this means we have a list of lists that contain parameter sets
param_list_apriori <- lapply(1:nrow(param_grid_apriori), function(x) as.list(param_grid_apriori[x]))


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
full_test_sim <- apriori_params(param_list_apriori, t = 100)
