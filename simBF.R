# computing priors and posteriors manually

# simulating data with only dichotomous predictor (intervention)

# populations of the intervention and non-intervention group
# also setting the effect size
effect_size <-  0.6
pop_size <- 50000
sample_size <- 15

x0 <- rnorm(n = pop_size, mean = 0, sd = 1)
x1 <- rnorm(n = pop_size, mean = effect_size, sd = 1)

# making a helper function to compute bayes factor
bf <- function(fit, complexity) {
  (fit/complexity) / ((1 - fit) / (1 - complexity))
}

aafbf <- function(n_start = 15, n_step = 1, max_n = 50000, 
                  bf_target = 5, hypothesis = "x>0", 
                  effect_size = 0.2, pop_size = 50000, delta, b = 1/n) {
  # control population
  #pop_x0 <- rep(0, pop_size/2)
  # intervention population
  #pop_x1 <- rep(1, pop_size/2)
  
  # generate predictor variable
  x <- rep(c(0, 1), each = pop_size / 2)
  
  # generate outcome variable 
  # intercept = 0
  y <- rbinom(pop_size, size = 1, prob = plogis(0 + effect_size*x))
  
  pop_data <- data.frame(x, y)
  
  # start at bayes factor = 1
  bf <- 1
  
  n <- n_start
  
  iteration <- 0
  
  while (bf < bf_target & bf > 1/bf_target & n <= max_n) {
    
    iteration <- iteration + 1
    
    # sampling from population
    sample_data <- pop_data[sample(nrow(pop_data), n), ]
    print(paste("Sample size of this iteration =", n))
    
    # do the logistic regression
    log_reg <- glm(y ~ x, family = "binomial", data = sample_data)
    
    # get the coefficients and covariance matrix
    coefs <- coef(log_reg)
    # multiply the covariance matrix with identity matrix 
    cov_mat <- vcov(log_reg)
    
    # computing priors and posteriors
    comp <- pnorm(0, 0, b * sqrt(cov_mat) / n)
    
    fit <- pnorm(0, coefs["x"], sqrt(cov_mat["x", "x"]) / n)
    print(paste("complexity =", comp))
    print(paste("fit =", fit))
    
    # this is for the case of only inequality constraints
    bf <- bf(fit, comp)
    print(paste("BF = ", bf))
    
    # increase sample size and do the loop again
    n <- n + n_step
  }
}

aafbf()
