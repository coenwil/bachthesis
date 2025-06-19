# parallelized version to conduct simulations for a priori method
# this is the script that was to generate the data in the end
library(data.table)
library(parallel)

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

apriori_params_parallel <- function(param_list_apriori, n_cores = NULL, ...) {
  
  # set cores
  if (is.null(n_cores)) {
    n_cores <- detectCores() - 2
  }
  
  cat("Using", n_cores, "cores for", length(param_list_apriori), "parameter sets\n")
  
  # create cluster
  cl <- makeCluster(n_cores)
  
  # load packages and helper files for each worker
  clusterEvalQ(cl, {
    library(data.table)
    source("helpers/load_helpers.R")
    source("apriori/apriori.R")
  })
  
  # get extra arguments
  extra_args <- list(...)
  
  # every worker gets 1 param set
  sim_results_apriori <- parLapply(cl, 1:length(param_list_apriori), function(i) {
    
    # get current parameter set and merge with extra args
    params <- modifyList(param_list_apriori[[i]], extra_args)
    
    # run SSDlog function
    sim_result_apriori <- do.call(SSDlog, params)
    
    # dd sim ID   
    sim_result_apriori[, sim_id := i]
    
    return(sim_result_apriori)
  })
  
  stopCluster(cl)
  
  # combine  results
  return(rbindlist(sim_results_apriori, use.names = TRUE, fill = TRUE))
}

full_apriori_sim <- apriori_params_parallel(param_list_apriori, n_cores = NULL)

fwrite(full_apriori_sim, "data/apriori_sim_parallel.csv")

