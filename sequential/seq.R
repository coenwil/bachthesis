# function to determine sample size sequentially using bayes factors

# for faster storage of data
library(data.table)

# source helpers
# for computing fit, complexity, and bayes factor
source("helpers/load_helpers.R")

seqSSD <- function(n_start = 15, n_step = 1, max_n = 50000, 
                  bf_target = 5, beta_1 = 0.5, intercept = 0,
                  delta = 0, 
                  hypothesis = c("superiority", "non-inferiority", "equivalence"),
                  pop_size = 50000, nr_it = 5000) {
  
  # give starting time
  start_time <- Sys.time()
  print(paste("Start time:", format(start_time, "%Y-%m-%d %H:%M:%S")))
  
  # ensure hypothesis argument can be used
  hypothesis <- match.arg(hypothesis)
  
  # initiate results table
  results <- data.table(
    iteration = 1:nr_it,
    n = NA_integer_,
    bf = NA_real_,
    fit = NA_real_,
    complexity = NA_real_,
    n_start = rep(n_start, nr_it),
    n_step = rep(n_step, nr_it),
    bf_target = rep(bf_target, nr_it),
    intercept = rep(intercept, nr_it),
    beta_1 = rep(beta_1, nr_it),
    delta = rep(delta, nr_it),
    hypothesis = rep(hypothesis, nr_it)
  )
  
  # generate predictor variable
  x <- rep(c(0, 1), each = max_n / 2)
  
  # generate outcome variable 
  y <- rbinom(max_n, size = 1, prob = plogis(intercept + beta_1*x))
  
  pop_data <- data.table(x, y)
  
  # run simulation for set nr of iterations
  for (i in 1:nr_it) {
    
    # current time at the iteration
    current_time <- Sys.time()
    
    # elapsed time since beginning the function
    elapsed_time <- as.numeric(difftime(current_time, start_time, units = "secs"))
    # print every 500 trials
    if (i %% 500 == 0) {
      print(paste0("Trial ", i, ", elapsed time: ", round(elapsed_time, 0), " secs"))
    }
    
    
    # start at bayes factor = 1
    bf <- 1
    
    # start at n = n_start
    n <- n_start
    
    # make sure fit and comp have values for error prevention
    fit <- NA
    comp <- NA
    
    # initialize fail counter (when only 1's or 0's are sampled)
    fail_count <- 0
    
    # initiate while loop counter
    iteration <- 0
    
    while (!is.na(bf) && bf < bf_target && bf > 1/bf_target && n <= max_n) {
      
      # update counter
      iteration <- iteration + 1
      
      # compute b
      b <- 1/n
      
      # sampling from population
      sample_data <- pop_data[sample(nrow(pop_data), n), ]
      
      # sometimes the sample contains only 1s or 0s for x or y
      # this causes the glm to break
      if (length(unique(sample_data$y)) < 2 | length(unique(sample_data$x)) < 2) {
        # increase fail count
        fail_count <- fail_count + 1
        # try again with an n_step larger sample
        n <-  n + n_step
        next
      } else {
        
        # do the logistic regression
        log_reg <- glm(y ~ x, family = "binomial", data = sample_data)
        
        # get the coefficients and covariance matrix
        coefs <- coef(log_reg)["x"]
        cov_mat <- vcov(log_reg)["x", "x"]
        
        # computing complexity, fit, and bayes factor
        comp <- compute_complexity(hypothesis, delta, var = cov_mat, b)
        
        fit <- compute_fit(hypothesis, delta, coef = coefs, var = cov_mat)
        
        bf <- bayes_factor(fit, comp)
        
        # increase sample size and do the loop again
        n <- n + n_step
      }
    }
    # fill in the results of each trial
    results_list <- list(
      n = n,
      bf = bf,
      fit = fit,
      complexity = comp,
      fail_count = fail_count
    )
    
    # update results table
    results[i, names(results_list) := results_list]
  }
  
  # show end time
  end_time <- Sys.time()
  print(paste("End time:", format(end_time, "%Y-%m-%d %H:%M:%S")))
  print(round(difftime(current_time, start_time, units = "auto"), 1))
  
  return(results)
}
