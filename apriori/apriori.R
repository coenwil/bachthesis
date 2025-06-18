# function to determine sample size a priori using bayes factors
# as described in Fu (2020)

# for faster data storage and manipulation
library(data.table)

# source helpers
# for computing fit, complexity, and bayes factor
source("helpers/load_helpers.R")

SSDlog <- function(n_min = 10, n_max = 100, 
                   bf_thresh = 5, eta = 0.8, t = 10000,
                   intercept = 0, beta_1 = 0.5,
                   delta = 0, 
                   hypothesis = c("superiority", "non-inferiority", "equivalence"),
                   pop_size = 50000) {
  
  # ensure hypothesis argument can be used
  hypothesis <- match.arg(hypothesis)
  
  # generate population
  x <- rep(c(0, 1), each = pop_size / 2)
  
  # generate outcome variable under alternative hypothesis
  y_alt <- rbinom(pop_size, size = 1, prob = plogis(intercept + beta_1*x))
  # generate y under null hypothesis (beta_1 = 0)
  y_null <- if (hypothesis %in% c("superiority", "non-inferiority")) {
    rbinom(pop_size, size = 1, prob = plogis(intercept + -beta_1*x))
  } else if (hypothesis == "equivalence") {
    rbinom(pop_size, size = 1, prob = plogis(intercept + (2*delta*x)))
  }
  pop_data <- data.table(x, y_alt, y_null)
  
  # flag for the first iteration (to evaluate n_min)
  first_iter <- TRUE
  
  # initialize values
  p_h1 <- NA_real_
  p_h0 <- NA_real_
  prop_fail <- 0
  
  # run until sample size is minimal while still having enough support
  while (n_max - n_min > 1) {
    
    n_mid <- floor((n_min + n_max) / 2)
    
    # in the first iteration, use n_min
    if (first_iter) {
      n_mid <- n_min
      first_iter <- FALSE
    }
    
    # define b for the fractional bayes factors
    b <- 1/n_mid
    
    # vectors to store bayes factors for this iteration
    bf_h1_vec <- numeric(t)
    bf_h0_vec <- numeric(t)
    
    # storing fail count to check proportion
    fail_count <- 0
    
    # doing T iterations of the current sample size to approx. P(BF > eta)
    for (i in 1:t) {
      # sample from alternative hypothesis population
      sample_alt <- pop_data[sample(nrow(pop_data), n_mid), .(x, y = y_alt)]
      # sample from null hypothesis population
      sample_null <- pop_data[sample(nrow(pop_data), n_mid), .(x, y = y_null)]
      
      # check if there is not only 0's or 1's sampled
      # if yes, increase fail count and go to next iteration in T
      if (var(sample_alt$y) == 0 || var(sample_alt$x) == 0 || 
          var(sample_null$y) == 0 || var(sample_null$x) == 0) {
        fail_count <- fail_count + 1
        next
      }
      
      # H1 logistic regression and computing bayes factor
      logreg_h1 <- glm(y ~ x, family = "binomial", data = sample_alt)
      
      # get the coefficients and covariance matrix
      coef_h1 <- coef(logreg_h1)["x"]
      var_h1 <- vcov(logreg_h1)["x", "x"]
      
      # computing complexity, fit
      comp_h1 <- compute_complexity(hypothesis, delta, var = var_h1, b)
      
      fit_h1 <- compute_fit(hypothesis, delta, coef = coef_h1, var = var_h1)
      
      # get the bayes factor
      # store all of them in a vector to get the mean later
      bf_h1 <- bayes_factor(fit_h1, comp_h1)
      bf_h1_vec[i] <- bf_h1
      
      # H0 logistic regression and computing bayes factor
      logreg_h0 <- glm(y ~ x, family = "binomial", data = sample_null)
      coef_h0 <- coef(logreg_h0)["x"]
      var_h0 <- vcov(logreg_h0)["x", "x"]
      comp_h0 <- compute_complexity(hypothesis, delta, var = var_h0, b)
      fit_h0 <- compute_fit(hypothesis, delta, coef = coef_h0, var = var_h0)
      bf_h0 <- bayes_factor(fit_h0, comp_h0)
      bf_h0_vec[i] <- bf_h0
    }
    
    # check proportion of failures. if > 5%, not enough valid samples
    fail_prop <- fail_count / t
    if (fail_prop > 0.05) {
      warning("More than 5% of samples had no variance! Retrying with n + 1")
      n_min <- n_min + 1
      prop_fail <- prop_fail + 1
      next
    }
    
    # compute probability of bayes factor being above eta
    p_h1 <- mean(bf_h1_vec > bf_thresh)
    p_h0 <- mean((1 / bf_h0_vec) > bf_thresh)
    
    cat("n_mid:", n_mid, "p_h1:", p_h1, "p_h0", p_h0, "\n")
    
    # binary search; continue or stop searching
    if (p_h0 > eta && p_h1 > eta) {
      # if support is 'too much', do lower sample size
      n_max <- n_mid
    } else {
      # if not enough support, increase sample size
      n_min <- n_mid
    }
  }
  # get final value
  n <- n_max
  
  return(data.table(
    n,
    p_h1,
    p_h0,
    bf_h1,
    bf_h0,
    fit_h1,
    fit_h0,
    comp_h1,
    comp_h0,
    n_min,
    n_max,
    bf_thresh,
    eta,
    intercept,
    beta_1,
    hypothesis,
    delta,
    prop_fail
  ))
}

test_prio <- SSDlog(t = 100, beta_1 = 0, bf_thresh = 5, hypothesis = "equivalence", delta = .3)
