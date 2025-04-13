set.seed(123456)

# sample size
n <- 500

# generating continuous and binary predictor variables, both uniform
x2 <- rbinom(n, 1, 0.5)

# defining coefficients
form <- -2 + 0.6*x2

# logit to transform the linear formula into probabilities
logit <- exp(form) / (1 + exp(form))

# generating outcome based on the logit
y <- rbinom(n = n, size = 1, prob = logit)

# generated proportion of outcomes
mean(y)

# make data frame
sim <- data.frame(x2, y)

# defining regression and getting summary
simReg <- glm(y ~ x2, family = "binomial", data = sim)
summary(simReg)

# getting covariance matrix
(simCov <- round(vcov(simReg), 3))