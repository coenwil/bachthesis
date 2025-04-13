# computing priors and posteriors

# reference the file in which I simulate data and compute a logistic regression
# important objects: sim (data) and simReg (logistic regression)
source("simLogReg.R")

# function to compute a prior of a logistic regression
# mu is a vector of the parameters to be estimated
logPrior <- function(logReg, b) {
  
  # get N
  N = nrow(logReg$data)
  
  # get number of parameters
  numPar <- length(coef(logReg))
  
  # get residual variance (sigma hat squared)
  resVar = sum(residuals(logReg)^2) / (N - numPar)
  
  # compute prior distribution
  # do * diag(numPar) to get only the diagonal filled in, rest = 0
  priorCov <- (1 / b) * (resVar / N) * diag(numPar)
  
  # return means vector and covariance matrix
  return(list(mean = rep(0, numPar), covariance = priorCov))
}

# test the function
test <- logPrior(simReg, 2)

# get a normal distribution
test_distr <- rnorm(n = nrow(simReg$data),
      mean = test$mean,
      sd = test$covariance)

# plot
plot(density(test_distr), col = "blue", lwd = 2)
