# function to determine sample size a priori using bayes factors
# as described in Fu (2020)

# for faster data storage and manipulation
library(data.table)

# making a helper function to compute bayes factor
bayes_factor <- function(fit, complexity) {
  (fit/complexity) / ((1 - fit) / (1 - complexity))
}

SSDlog <- function(n_min = 10, n_max = 100, 
                   bf_thresh = 5, eta = 0.8, t = 10000,
                   intercept = 0, beta_1 = 0.5,
                   nr_it = 1, pop_size = 50000) {
  
  # generate population
  x <- rep(c(0, 1), each = pop_size / 2)
  
  # generate outcome variable under alternative hypothesis
  y_alt <- rbinom(pop_size, size = 1, prob = plogis(intercept + beta_1*x))
  # generate y under null hypothesis (beta_1 = 0)
  y_null <- rbinom(pop_size, size = 1, prob = plogis(intercept))
  
  pop_data <- data.table(x, y_alt, y_null)
  
  # flag for the first iteration (to evaluate n_min)
  first_iter <- TRUE
  
  # run until sample size is minimal while still having enough support
  while (n_max - n_min > 1) {
    
    # in the first iteration, use n_min
    if (first_iter) {
      n_mid <- n_min
      first_pass <- FALSE
    # after that, just do the normal binary search
    } else {
      n_mid <- floor((n_min + n_max) / 2)
    }
    
    # define b for the fractional bayes factors
    b <- 1/n_mid
    
    # vectors to store bayes factors for this iteration
    bf_h1_vec <- numeric(t)
    bf_h0_vec <- numeric(t)
    
    # doing T iterations of the current sample size to get eta
    for (i in 1:t) {
      # sample from alternative hypothesis population
      sample_alt <- pop_data[sample(nrow(pop_data), n_mid), .(x, y = y_alt)]
      # sample from null hypothesis population
      sample_null <- pop_data[sample(nrow(pop_data), n_mid), .(x, y = y_null)]
      
      # H1 logistic regression and computing bayes factor
      logreg_h1 <- glm(y ~ x, family = "binomial", data = sample_alt)
      # get the coefficients and covariance matrix
      coef_h1 <- coef(logreg_h1)["x"]
      var_h1 <- vcov(logreg_h1)["x", "x"]
      
      # computing complexity, fit, and bayes factor
      comp_h1 <- pnorm(0, 0, b * sqrt(var_h1))
      
      fit_h1 <- pnorm(0, coef_h1, sqrt(var_h1))
      
      # get the bayes factor
      # store all of them in a vector to get the mean later
      bf_h1 <- bayes_factor(fit_h1, comp_h1)
      bf_h1_vec[i] <- bf_h1
      
      # H0 logistic regression and computing bayes factor
      logreg_h0 <- glm(y ~ x, family = "binomial", data = sample_null)
      coef_h0 <- coef(logreg_h0)["x"]
      var_h0 <- vcov(logreg_h0)["x", "x"]
      comp_h0 <- pnorm(0, 0, b * sqrt(var_h0))
      fit_h0 <- pnorm(0, coef_h0, sqrt(var_h0))
      bf_h0 <- bayes_factor(fit_h0, comp_h0)
      bf_h0_vec[i] <- bf_h0
    }
    
    # compute probability of bayes factor being above eta
    p_h1 <- mean(bf_h1_vec > bf_thresh)
    p_h0 <- mean(bf_h0_vec > bf_thresh)
    
    # binary search; continue or stop searching
    if (p_h0 > eta && p_h1 > eta) {
      # if support is 'too much', do lower sample size
      n_max <- n_mid
    } else {
      # if not enough support, increase sample size
      n_min <- n_mid
    }
  }
  return(data.table(
    n = n_mid,
    p_h1 = p_h1,
    p_h0 = p_h0,
    bf_thresh = bf_thresh,
    eta = eta,
    intercept = intercept,
    beta_1 = beta_1
  ))
}

test_prio <- SSDlog(t = 10, beta_1 = 0.5, bf_thresh = 5)
