# making a helper function to compute bayes factor
bayes_factor <- function(fit, complexity) {
  (fit/complexity) / ((1 - fit) / (1 - complexity))
}