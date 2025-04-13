# computing priors and posteriors manually
# looping over different parameter sets

# also use data.table here for speed advantage
library(data.table)
# simulating this in parallel
library(parallel)
library(doParallel)

# setting up parallel computing
num_cores <- max(1, detectCores() - 1)
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# source simPrior to get AAFBF simulation function
source("aafbf.R")

# setting environment explicitly for windows
clusterExport(cl, varlist = c("bayes_factor", "aafbf"), envir = environment())

# creating all combinations of parameters
param_grid <- CJ(
  n_start   = c(5, 15, 50),
  n_step    = c(1, 2, 5),
  bf_target = c(5, 10, 20),
  beta_1    = c(0, 0.25, 0.5, 0.75),
  intercept = c(-0.5, 0, 0.5)
)

# creating a list out of  every row in param_grid
# this means we have a list of lists that contain parameter sets
param_list <- lapply(1:nrow(param_grid), function(x) as.list(param_grid[x]))


# function to loop over all parameters
aafbf_params_parallel <- function(param_list, ...) {
  
  # give start time
  start_time <- Sys.time()
  print(paste("Start time:", format(start_time, "%Y-%m-%d %H:%M:%S")))
  
  # get a list with the extra arguments (to modify nr_it)
  extra_args <- list(...)
  
  # compute in parallel
  par_results <- foreach(i = seq_along(param_list),
                         .combine = rbind,
                         .packages = c("data.table")) %dopar% {
    # get random seed for each iteration                       
    set.seed(1 + i)
  
    # get the current parameter set and the optional extra args in ...
    params <- modifyList(param_list[[i]], extra_args)
    
    # run aafbf() with current parameters and store results
    sim_result <- do.call(aafbf, params)
    
    # add sim ID for tracing (future: make codes)
    sim_result[, sim_id := i]
    
    # store this 
    return(sim_result)
  }
  
  # give end time and elapsed time
  end_time <- Sys.time()
  print(paste("End time:", format(end_time, "%Y-%m-%d %H:%M:%S")))
  print(round(difftime(end_time, start_time, units = "auto"), 1))
  
  # return a big table with all results
  return(par_results)
}

# uncomment this for a test run
# recommend doing nr_it = 10 for quick run, in practice would be 5000
full_test_sim <- aafbf_params_parallel(param_list, nr_it = 10)

stopCluster(cl)
