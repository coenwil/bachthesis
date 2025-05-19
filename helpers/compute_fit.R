# helper function to compute fit
# the area of the posterior distribution that is in agreement with the hypothesis

compute_fit <- function(hypothesis = c("superiority", "non-inferiority", "equivalence"),
                        delta = 0, coef, var) {
  
  hypothesis <- match.arg(hypothesis)
  
  if (hypothesis == "superiority") {
    return(pnorm(0, mean = coef, sd = sqrt(var), lower.tail = FALSE))
  }
  
  if (hypothesis == "non-inferiority") {
    return(pnorm(delta, mean = coef, sd = sqrt(var), lower.tail = FALSE))
  }
  
  if (hypothesis == "equivalence") {
    return(pnorm(delta, mean = coef, sd = sqrt(var), lower.tail = TRUE) -
             pnorm(-delta, mean = coef, sd = sqrt(var), lower.tail = TRUE))
  }
  
}
