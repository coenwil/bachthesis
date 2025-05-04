# computing priors and posteriors manually
# looping over different parameter sets

# also use data.table here for speed advantage
library(data.table)

# source sequential AAFBF simulation script
source("aafbf.R")

# creating all combinations of parameters
param_grid_aafbf <- CJ(
  n_start   = c(5, 15, 50),
  n_step    = c(1, 2, 5),
  max_n     = c(100, 1000),
  bf_target = c(3, 5, 20),
  beta_1    = c(0, 0.5, 1),
  intercept = c(-2, 0, 1)
)

# creating a list out of  every row in param_grid
# this means we have a list of lists that contain parameter sets
param_list <- lapply(1:nrow(param_grid), function(x) as.list(param_grid[x]))


# function to loop over all parameters
aafbf_params_serial <- function(param_list, ...) {
  
  # get a list with the extra arguments (to modify nr_it)
  extra_args <- list(...)
  
  # initiate empty list to store results
  # (this is a more memory efficient way of doing sim_results <- list() )
  sim_results <- vector("list", length(param_list))
  
  for (i in seq_along(param_list)) {
    
    # print progress
    message(sprintf("Running simulation %d of %d", i, length(param_list)))
    
    # get the current parameter set and the optional extra args in ...
    params <- modifyList(param_list[[i]], extra_args)
    
    # run aafbf() with current parameters and store results
    sim_result <- do.call(aafbf, params)
    
    # add sim ID for tracing (future: make codes)
    sim_result[, sim_id := i]
    
    # store this 
    sim_results[[i]] <- sim_result
  }
  
  # return a big table with all results
  return(rbindlist(sim_results, use.names = TRUE, fill = TRUE))
  
}

# uncomment this for a test run
# recommend doing nr_it = 10 for quick run, in practice would be 5000
full_test_sim <- aafbf_params_serial(param_list, nr_it = 10)