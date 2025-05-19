# helper function to compute complexity
# the area of the prior distribution that is in agreement with the hypothesis

compute_complexity <- function(hypothesis = c("superiority", "non-inferiority", "equivalence"),
                               delta = 0, var, b) {
  
  hypothesis <- match.arg(hypothesis)
  
  if (hypothesis == "superiority") {
    return(pnorm(0, mean = 0, sd = (1/b) * sqrt(var), lower.tail = FALSE))
  }
  
  if (hypothesis == "non-inferiority") {
    return(pnorm(delta, mean = delta, sd = (1/b) * sqrt(var), lower.tail = FALSE))
  }
  
  if (hypothesis == "equivalence") {
    return(pnorm(delta, mean = 0, sd = (1/b) * sqrt(var), lower.tail = TRUE)) -
      pnorm(-delta, mean = 0, sd = (1/b) * sqrt(var), lower.tail = TRUE)
  }
}